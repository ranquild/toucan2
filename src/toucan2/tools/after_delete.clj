(ns toucan2.tools.after-delete
  (:require
   [clojure.spec.alpha :as s]
   [methodical.core :as m]
   [toucan2.connection :as conn]
   [toucan2.log :as log]
   [toucan2.pipeline :as pipeline]
   [toucan2.realize :as realize]
   [toucan2.tools.before-delete :as before-delete]
   [toucan2.types :as types]
   [toucan2.util :as u]))

(set! *warn-on-reflection* true)

(comment types/keep-me)

(m/defmulti after-delete
  "Underlying method implemented when using [[define-after-delete]]. You *probably* shouldn't be adding implementations
  to this method directly, unless you know what you are doing!"
  {:arglists            '([model₁ instance])
   :defmethod-arities   #{2}
   :dispatch-value-spec (s/nonconforming ::types/dispatch-value.model)}
  u/dispatch-on-first-arg)

(m/defmethod after-delete :around :default
  [model instance]
  (log/tracef "Do after-delete for %s %s" model instance)
  (next-method model instance))

(defn- matching-rows
  "Select and realize the rows matching the args passed to `delete!`, before they are deleted."
  [model parsed-args resolved-query]
  (pipeline/transduce-query
   ((map realize/realize) conj)
   :toucan.query-type/select.instances
   model
   parsed-args
   resolved-query))

(m/defmethod pipeline/transduce-query [#_query-type     :toucan.query-type/delete.*
                                       #_model          ::after-delete
                                       #_resolved-query :default]
  "Do a recursive SELECT query with the args passed to `delete!`, run the `next-method` to delete the matching rows, and
  then apply [[after-delete]] to each row that was deleted. This is all done inside of a transaction."
  [rf query-type model parsed-args resolved-query]
  (conn/with-transaction [_conn nil {:nested-transaction-rule :ignore}]
    (let [rows   (matching-rows model parsed-args resolved-query)
          result (next-method rf query-type model parsed-args resolved-query)]
      (doseq [row rows]
        (after-delete model row))
      result)))

;;; A model with both before-delete and after-delete runs its before-delete methods first, and the after-delete method
;;; above as the `next-method`.
(m/prefer-method! #'pipeline/transduce-query
                  [:toucan.query-type/delete.* ::before-delete/before-delete :default]
                  [:toucan.query-type/delete.* ::after-delete :default])

(defn ^:no-doc -after-delete-impl
  "Implementation of [[define-after-delete]]; don't call this directly."
  [next-method model instance f]
  ;; if `f` didn't return anything, just use the original instance again.
  (let [result (or (f model instance)
                   instance)]
    (if next-method
      (next-method model result)
      result)))

(defmacro define-after-delete
  "Define a method that will be called for every instance that was deleted, with the instance as it was before the
  delete. The results of this after-delete method are ultimately ignored, but the entire operation (the recursive
  select, the original delete, and the after-delete methods) is done in a transaction, so you can use after-delete to
  enforce postconditions and abort deletes when they fail, or to keep other rows up to date in the same transaction.

  After-delete is implemented by first selecting all the rows matching the [[toucan2.delete/delete!]] conditions, then
  running the `DELETE`, and then calling the [[after-delete]] method this macro defines on each of those rows. Like
  [[toucan2.tools.before-delete/define-before-delete]], defining an after-delete method means every `delete!` has to
  fetch every matching row first, which can be quite expensive, so be sure you *really* need after-delete behavior
  before opting in to it.

  To skip after-delete behavior, you can always use the model's raw table name directly, e.g.

  ```clj
  (t2/delete! (t2/table-name :models/user) ...)
  ```"
  {:style/indent :defn}
  [model [instance-binding] & body]
  `(do
     (u/unparent-descendants ~model ::after-delete)
     (u/maybe-derive ~model ::after-delete)
     (m/defmethod after-delete ~model
       [model# instance#]
       (-after-delete-impl ~'next-method
                           model#
                           instance#
                           (fn [~'&model ~instance-binding]
                             ~@body)))))

(s/fdef define-after-delete
  :args (s/cat :model    some?
               :bindings (s/spec (s/cat :instance :clojure.core.specs.alpha/binding-form))
               :body     (s/+ any?))
  :ret any?)
