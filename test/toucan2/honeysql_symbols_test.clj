(ns toucan2.honeysql-symbols-test
  "Every identifier Toucan 2 generates -- clause names, table names, operators, the columns it qualifies -- is a symbol.
  Honey SQL treats a symbol exactly like the keyword of the same name, so the SQL is unchanged; what it buys you is
  that a keyword left in a built query got there as data, which an application can then refuse to compile."
  (:require
   [clojure.test :refer :all]
   [methodical.core :as m]
   [toucan2.core :as t2]
   [toucan2.model :as model]
   [toucan2.query :as query]
   [toucan2.tools.compile :as compile]))

(set! *warn-on-reflection* true)

(m/defmethod model/table-name ::venues
  [_model]
  'venues)

(m/defmethod model/table-name ::keyword-table-name
  [_model]
  :some_table)

(m/defmethod model/table-name ::string-table-name
  [_model]
  "string_table_name")

(defn- keywords-in
  "Every keyword anywhere in `form`, in keys or values."
  [form]
  (cond
    (keyword? form) [form]
    (map? form)     (mapcat (fn [[k v]] (concat (keywords-in k) (keywords-in v))) form)
    (coll? form)    (mapcat keywords-in form)))

(deftest ^:parallel table-name-is-a-symbol-test
  (is (= 'venues
         (model/table-name ::venues)))
  (testing "a table name that isn't already a symbol is coerced to one"
    (is (= 'some_table
           (model/table-name ::keyword-table-name)))
    (is (= 'string_table_name
           (model/table-name ::string-table-name)))))

(deftest ^:parallel select-test
  (is (= '{select [*], from [[venues]], where [= name "Tempest"]}
         (compile/build (t2/select ::venues 'name "Tempest"))))
  (testing "multiple kv-args are combined with a symbol `and`"
    (is (= '{select [*], from [[venues]], where [and [= name "Tempest"] [> id 5]]}
           (compile/build (t2/select ::venues 'name "Tempest" 'id ['> 5])))))
  (testing "a lone primary key value"
    (is (= '{select [*], from [[venues]], where [= id 1]}
           (compile/build (t2/select ::venues 1)))))
  (testing "explicit columns are qualified with the table name"
    (is (= '{select [venues/name venues/id], from [[venues]]}
           (compile/build (t2/select [::venues 'name 'id])))))
  (testing "count"
    (is (= '{select [[%count.* count]], from [[venues]]}
           (compile/build (t2/count ::venues)))))
  (testing "exists?"
    (is (= '{select [[[exists {select [[[inline 1]]], from [[venues]], where [= id 1]}] exists]]}
           (compile/build (t2/exists? ::venues 'id 1))))))

(deftest ^:parallel insert-update-delete-test
  (is (= '{insert-into [venues], values [{name "x"}]}
         (update (compile/build (t2/insert! ::venues {'name "x"})) 'values vec)))
  (is (= '{update [venues], set {name "x"}, where [= id 1]}
         (compile/build (t2/update! ::venues 1 {'name "x"}))))
  (testing "row-map keys are used exactly as written -- Toucan 2 converts nothing on the way in"
    (is (= '[{:name "x"}]
           (:rows (query/parse-args :toucan.query-type/insert.* [::venues {:name "x"}]))))
    (is (= '[{name "x"}]
           (:rows (query/parse-args :toucan.query-type/insert.* [::venues {'name "x"}]))))
    (is (= '{name "x"}
           (:changes (query/parse-args :toucan.query-type/update.* [::venues 1 {'name "x"}]))))))

(deftest ^:parallel delete-test
  (is (= '{delete-from [venues], where [= id 1]}
         (compile/build (t2/delete! ::venues 'id 1)))))

(deftest ^:parallel nothing-toucan-generates-is-a-keyword-test
  (testing "a query built entirely out of symbols has no keyword left in it anywhere"
    (doseq [query [(compile/build (t2/select ::venues 'name "Tempest" 'id ['in [1 2]]))
                   (compile/build (t2/select [::venues 'name] {'limit 5}))
                   (compile/build (t2/count ::venues 'id 1))
                   (compile/build (t2/exists? ::venues 'id 1))
                   (compile/build (t2/update! ::venues 1 {'name "x"}))
                   (compile/build (t2/delete! ::venues 'id 1))]]
      (is (= [] (vec (keywords-in query)))
          (pr-str query)))))

(deftest ^:parallel empty-in-clause-test
  (testing "`['in x []]` still gets rewritten to `false` rather than compiling to invalid `IN ()`"
    (is (= ["SELECT * FROM \"venues\" WHERE FALSE"]
           (compile/compile (t2/select ::venues 'id ['in []]))))))
