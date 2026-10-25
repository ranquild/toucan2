(ns toucan2.jdbc.postgres
  "PostgreSQL integration."
  (:require
   [methodical.core :as m]
   [toucan2.connection :as conn]
   [toucan2.jdbc.pipeline :as jdbc.pipeline]
   [toucan2.jdbc.query :as jdbc.query]
   [toucan2.jdbc.read :as jdbc.read]
   [toucan2.pipeline :as pipeline]
   [toucan2.util :as u])
  (:import
   (java.sql ResultSet ResultSetMetaData Types)))

(set! *warn-on-reflection* true)

(when-let [pg-connection-class (try
                                 (Class/forName "org.postgresql.jdbc.PgConnection")
                                 (catch Throwable _
                                   nil))]
  (derive pg-connection-class ::connection))

(m/defmethod jdbc.read/read-column-thunk [#_conn  ::connection
                                          #_model :default
                                          #_type  Types/TIMESTAMP]
  "Both Postgres `timestamp` and `timestamp with time zone` come back as `java.sql.Types/TIMESTAMP`; check the actual
  database column type name so we can fetch objects as the correct class."
  [_conn _model ^ResultSet rset ^ResultSetMetaData rsmeta ^Long i]
  (let [^Class klass (if (= (u/lower-case-en (.getColumnTypeName rsmeta i)) "timestamptz")
                       java.time.OffsetDateTime
                       java.time.LocalDateTime)]
    (jdbc.read/get-object-of-class-thunk rset i klass)))

(m/prefer-method! #'jdbc.read/read-column-thunk
                  [::connection :default Types/TIMESTAMP]
                  [java.sql.Connection :default Types/TIMESTAMP])

(m/defmethod conn/dml-returns-rows? ::connection
  [_connection]
  true)

(defn- returning-columns
  "The column names to return from a DML query that asked for `columns`, or `true` for every column."
  [columns]
  (if (and (seq columns) (every? keyword? columns))
    (mapv name columns)
    true))

(m/defmethod pipeline/transduce-execute-with-connection [#_conn       ::connection
                                                         #_query-type ::jdbc.pipeline/DML-queries-returning-instances
                                                         #_model      :default]
  "Postgres returns the rows a DML query inserted, updated, or deleted when it is asked for generated keys, as the driver
  adds `RETURNING` to the query, so read the instances from those rows rather than doing a second SELECT."
  [rf conn _query-type model sql-args]
  (let [extra-options {:return-keys (returning-columns (:columns pipeline/*parsed-args*))}
        result        (jdbc.query/reduce-jdbc-query rf (rf) conn model sql-args extra-options)]
    (rf result)))

(m/prefer-method! #'pipeline/transduce-execute-with-connection
                  [::connection ::jdbc.pipeline/DML-queries-returning-instances :default]
                  [java.sql.Connection ::jdbc.pipeline/DML-queries-returning-instances :default])
