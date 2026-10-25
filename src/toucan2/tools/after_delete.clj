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

;;; The select of the rows a `delete!` is about to delete. Default fields don't apply to it, so after-delete methods get
;;; every column of the deleted rows.
(derive ::select-for-after-delete :toucan.query-type/select.instances)

(defn- matching-rows
  "Select and realize the rows matching the args passed to `delete!`, before they are deleted. The rows are collected as
  they are reduced, so a query that is only compiled, e.g. with [[toucan2.tools.compile/compile]], returns none."
  [model parsed-args resolved-query]
  (let [rows (volatile! [])]
    (pipeline/transduce-query
     ((map (fn [row]
             (vswap! rows conj (realize/realize row))))
      (constantly nil))
     ::select-for-after-delete
     model
     parsed-args
     (before-delete/-select-query resolved-query))
    @rows))

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
  "Define a method that will be called for every row deleted by [[toucan2.delete/delete!]], with the row as it was right
  before the `DELETE`: a realized instance of the model with all of its columns, as default fields don't apply to it.
  The results of this after-delete method are ignored, other than being passed to the next method, e.g. the
  after-delete method of a parent model. The recursive select, the original delete, and the after-delete methods are
  all done in one transaction, so you can use after-delete to keep other rows up to date in the same transaction, or to
  enforce postconditions and abort deletes when they fail: an exception thrown by an after-delete method rolls back the
  delete, unless code inside a transaction that was already open catches it, in which case that transaction keeps the
  `DELETE`.

  After-delete is implemented by first selecting all the rows matching the [[toucan2.delete/delete!]] conditions, then
  running the `DELETE`, and then calling the [[after-delete]] method this macro defines on each of those rows. Unlike
  [[toucan2.tools.before-delete/define-before-delete]], which handles the rows one at a time, after-delete has to hold
  every matching row in memory until the `DELETE` is done, so defining an after-delete method can be quite expensive
  for deletes of many rows! Be sure you *really* need after-delete behavior before opting in to it. A model with
  before-delete methods as well runs them first, and after-delete then selects the rows again, so its methods see any
  changes the before-delete methods made. Deletes with raw SQL aren't supported, as their rows can't be selected first.

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
