(ns gps
  (:use [clojure.set :as set])
  (:use clojure.test))

(defrecord Op [action preconds add-set del-set])

(defn appropriate? [goal op]
  ((:add-set op) goal))

(defn gps-with-state
  "General Problem Solver: achieve all goals using ops"
  [initial-state goals ops]
  (let [state (atom initial-state)]
    (letfn [(apply-op [op]
              (when (every? achieve (:preconds op))
                (println "Executing " (:action op))
                (swap! state set/difference (:del-set op))
                (swap! state set/union (:add-set op))))
            (achieve [goal]
              (or (@state goal)
                  (some apply-op
                        (filter (partial appropriate? goal)
                                ops))))]
      (if (every? achieve goals) :solved))))

(defn gps-functional
  "General Problem Solver: achieve all goals using ops"
  [initial-state goals ops]
  (letfn [(apply-op [state op]
            (when-let [new-state (achieve-all state (:preconds op))]
              (println "Executing " (:action op))
              (-> new-state
                  (set/difference (:del-set op))
                  (set/union (:add-set op)))))
          (achieve [state goal]
            (if (contains? state goal)
                state
                (some #(and (appropriate? goal %)
                            (apply-op state %))
                      ops)))
          (achieve-all [state goals]
            (reduce achieve state goals))]
    (if (achieve-all initial-state goals) :solved)))

;;; Test

(def school-ops
  [(Op. :drive-son-to-school
     #{:son-at-home :car-works}
     #{:son-at-school}
     #{:son-at-home})
   (Op. :shop-installs-battery
     #{:car-needs-battery :shop-knows-problem :shop-has-money}
     #{:car-works}
     #{})
   (Op. :tell-shop-problem
     #{:in-communication-with-shop}
     #{:shop-knows-problem}
     #{})
   (Op. :telephone-shop
     #{:know-phone-number}
     #{:in-communication-with-shop}
     #{})
   (Op. :look-up-number
     #{:have-phone-book}
     #{:know-phone-number}
     #{})
   (Op. :give-shop-money
     #{:have-money :shop-knows-problem}
     #{:shop-has-money}
     #{:have-money})])


(let [initial-state #{:son-at-home :car-needs-battery :have-money :have-phone-book}
      goals #{:son-at-school}]
  (is (= :solved (gps-with-state initial-state goals school-ops)))
  (is (= :solved (gps-functional initial-state goals school-ops))))

(let [initial-state #{:son-at-home :car-needs-battery :have-money}
      goals #{:son-at-school}]
  (is (not (gps-with-state initial-state goals school-ops)))
  (is (not (gps-functional initial-state goals school-ops))))


(let [initial-state #{:son-at-home :car-works}
      goals #{:son-at-school}]
  (is (= :solved (gps-with-state initial-state goals school-ops)))
  (is (= :solved (gps-functional initial-state goals school-ops))))

