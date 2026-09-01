(ns toucan2.save
  (:require
   [clojure.spec.alpha :as s]
   [methodical.core :as m]
   [toucan2.connection :as conn]
   [toucan2.instance :as instance]
   [toucan2.log :as log]
   [toucan2.model :as model]
   [toucan2.protocols :as protocols]
   [toucan2.types :as types]
   [toucan2.update :as update]
   [toucan2.util :as u]))

(comment s/keep-me
         types/keep-me)

(set! *warn-on-reflection* true)

(m/defmulti save!*
  {:arglists            '([object])
   :defmethod-arities   #{1}
   :dispatch-value-spec (s/nonconforming ::types/dispatch-value.model)}
  (fn [object]
    (protocols/dispatch-value (protocols/model object))))

(m/defmethod save!* :around :default
  [object]
  (u/try-with-error-context ["save changes" {::model   (protocols/model object)
                                             ::object  object
                                             ::changes (protocols/changes object)}]
    (log/debugf "Save %s %s changes %s" (protocols/model object) object (protocols/changes object))
    (next-method object)))

(m/defmethod save!* :default
  [object]
  (assert (instance/instance? object)
          (format "Don't know how to save something that's not a Toucan instance. Got: ^%s %s"
                  (some-> object class .getCanonicalName)
                  (pr-str object)))
  (if-let [changes (not-empty (protocols/changes object))]
    (let [model         (protocols/model object)
          ;; the object's keys are how the row came back from the DB; the query we build out of them is ours, and the
          ;; identifiers in a query Toucan 2 builds are symbols.
          ->column      (fn [k] (symbol (namespace k) (name k)))
          pk-values     (update-keys (select-keys object (model/primary-keys model)) ->column)
          rows-affected (update/update! model pk-values (update-keys changes ->column))]
      (when-not (pos? rows-affected)
        (throw (ex-info (format "Unable to save object: %s with primary key %s does not exist."
                                (pr-str model)
                                (pr-str pk-values))
                        {:object object
                         :pk     pk-values})))
      (when (> rows-affected 1)
        (log/warnf "Warning: more than 1 row affected when saving %s with primary key %s" model pk-values))
      (instance/reset-original object))
    object))

(defn save!
  ([object]
   (save!* object))
  ([connectable object]
   (if connectable
     (binding [conn/*current-connectable* connectable]
       (save!* object))
     (save!* object))))
