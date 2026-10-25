(ns toucan2.jdbc.h2
  "H2 integration."
  (:require
   [honey.sql :as hsql]
   [methodical.core :as m]
   [toucan2.connection :as conn]
   [toucan2.honeysql2 :as t2.honeysql]
   [toucan2.jdbc.pipeline :as jdbc.pipeline]
   [toucan2.jdbc.query :as jdbc.query]
   [toucan2.model :as model]
   [toucan2.pipeline :as pipeline]))

(set! *warn-on-reflection* true)

(when-let [h2-connection-class (try
                                 (Class/forName "org.h2.jdbc.JdbcConnection")
                                 (catch Throwable _
                                   nil))]
  (derive h2-connection-class ::connection))

(m/defmethod conn/dml-returns-rows? ::connection
  [_connection]
  true)

(defn- returning-sql-args
  "`sql-args` for the DML query in `sql-args` wrapped in an H2 data change delta table, selecting `columns`, or every
  column when there are none, in primary key order: the rows a `DELETE` removed (`OLD TABLE`), or the rows an `INSERT`
  or `UPDATE` left (`FINAL TABLE`)."
  [query-type model columns [sql & args]]
  (let [identifiers (fn [ks]
                      (mapv (comp keyword name) ks))
        format-sql  (fn [honeysql]
                      (first (hsql/format honeysql (t2.honeysql/options))))
        select      (format-sql {:select (if (seq columns) (identifiers columns) [:*])})
        order-by    (format-sql {:order-by (identifiers (model/primary-keys model))})
        table       (if (isa? query-type :toucan.query-type/delete.*)
                      "OLD TABLE"
                      "FINAL TABLE")]
    (into [(format "%s FROM %s (%s) %s" select table sql order-by)] args)))

(defn- returning-columns
  "The columns to return from a DML query that asked for `columns`, or nil for every column."
  [columns]
  (when (and (seq columns) (every? keyword? columns))
    columns))

(defn- delta-table-options
  "Options for reading the rows of a data change delta table as rows of `model`'s table: H2 reports `INSERT`, `UPDATE`,
  or `DELETE` as their table name, so qualify their columns with the namespace of `model`'s table instead."
  [model]
  (let [table-ns (some-> (get (model/table-name->namespace model) (name (model/table-name model))) name)]
    {:qualifier-fn (constantly table-ns)}))

(m/defmethod pipeline/transduce-execute-with-connection [#_conn       ::connection
                                                         #_query-type ::jdbc.pipeline/DML-queries-returning-instances
                                                         #_model      :default]
  "H2 returns the rows a DML query inserted, updated, or deleted when the query is wrapped in a data change delta table,
  so read the instances from those rows rather than doing a second SELECT."
  [rf conn query-type model sql-args]
  (let [sql-args (returning-sql-args query-type model (returning-columns (:columns pipeline/*parsed-args*)) sql-args)
        result   (jdbc.query/reduce-jdbc-query rf (rf) conn model sql-args (delta-table-options model))]
    (rf result)))

(m/prefer-method! #'pipeline/transduce-execute-with-connection
                  [::connection ::jdbc.pipeline/DML-queries-returning-instances :default]
                  [java.sql.Connection ::jdbc.pipeline/DML-queries-returning-instances :default])

(m/defmethod pipeline/transduce-execute-with-connection [#_conn       ::connection
                                                         #_query-type :toucan.result-type/pks
                                                         #_model      :default]
  "Read the primary keys of the rows a DML query wrote from a data change delta table selecting only those columns."
  [rf conn query-type model sql-args]
  (let [rf       ((map (model/select-pks-fn model)) rf)
        sql-args (returning-sql-args query-type model (model/primary-keys model) sql-args)
        result   (jdbc.query/reduce-jdbc-query rf (rf) conn model sql-args (delta-table-options model))]
    (rf result)))

(m/prefer-method! #'pipeline/transduce-execute-with-connection
                  [::connection :toucan.result-type/pks :default]
                  [java.sql.Connection :toucan.result-type/pks :default])
