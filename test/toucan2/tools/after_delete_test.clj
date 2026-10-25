(ns toucan2.tools.after-delete-test
  (:require
   [clojure.test :refer :all]
   [toucan2.delete :as delete]
   [toucan2.instance :as instance]
   [toucan2.select :as select]
   [toucan2.test :as test]
   [toucan2.tools.after-delete :as after-delete]
   [toucan2.tools.before-delete :as before-delete]))

(set! *warn-on-reflection* true)

(def ^:dynamic ^:private *calls* nil)

(derive ::venues.after-delete ::test/venues)

(after-delete/define-after-delete ::venues.after-delete
  [venue]
  (when *calls*
    (swap! *calls* conj [(select-keys venue [:id :name :category])
                         (select/exists? ::test/venues :id (:id venue))]))
  nil)

(deftest ^:synchronized after-delete-test
  (test/with-discarded-table-changes :venues
    (binding [*calls* (atom [])]
      (is (= 2
             (delete/delete! ::venues.after-delete :category "bar")))
      (testing "after-delete gets each deleted row as it was, after the delete"
        (is (= [[{:id 1, :name "Tempest", :category "bar"} false]
                [{:id 2, :name "Ho's Tavern", :category "bar"} false]]
               (sort-by (comp :id first) @*calls*))))
      (is (= [(instance/instance ::venues.after-delete {:id 3, :name "BevMo", :category "store"})]
             (select/select [::venues.after-delete :id :name :category]))))))

(derive ::venues.after-delete-exception ::test/venues)

(after-delete/define-after-delete ::venues.after-delete-exception
  [venue]
  (when (= (:category venue) "store")
    (throw (ex-info "Don't delete a store!" {:venue venue}))))

(deftest ^:synchronized after-delete-exception-test
  (test/with-discarded-table-changes :venues
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Don't delete a store!"
         (delete/delete! ::venues.after-delete-exception)))
    (testing "the delete is rolled back, as it is done in the same transaction"
      (is (= [1 2 3]
             (select/select-fn-vec :id ::venues.after-delete-exception {:order-by [[:id :asc]]}))))))

(derive ::venues.before-and-after-delete ::test/venues)

(before-delete/define-before-delete ::venues.before-and-after-delete
  [venue]
  (when *calls*
    (swap! *calls* conj [:before (:id venue)]))
  nil)

(after-delete/define-after-delete ::venues.before-and-after-delete
  [venue]
  (when *calls*
    (swap! *calls* conj [:after (:id venue)]))
  nil)

(deftest ^:synchronized before-and-after-delete-test
  (test/with-discarded-table-changes :venues
    (binding [*calls* (atom [])]
      (is (= 1
             (delete/delete! ::venues.before-and-after-delete 3)))
      (testing "a model with both runs before-delete, then the delete, then after-delete"
        (is (= [[:before 3] [:after 3]]
               @*calls*))))))

(deftest ^:parallel macroexpansion-test
  (testing "define-after-delete defines an after-delete method for the model"
    (is (= `(do
              (toucan2.util/unparent-descendants ::venues.after-delete ::after-delete/after-delete)
              (toucan2.util/maybe-derive ::venues.after-delete ::after-delete/after-delete))
           (take 3 (macroexpand-1 `(after-delete/define-after-delete ::venues.after-delete
                                     [~'venue]
                                     nil)))))))
