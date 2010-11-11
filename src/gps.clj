(ns gps
  "The General Problem Solver, developed in 1957 by Alan Newell and Herbert Simon,
   embodied a grandiose vision: a single computer program that could solve _any_
   problem, given a suitable description of the problem. GPS caused quite a stir when
   it was introduced, and some people in AI felt it would sweep in a grand new era
   of intelligent machines.
   It was the first program to separate its problem-solving strategy from its knowledge
   of particular problems."
  (:use [clojure.set :as set])
  (:use clojure.test))

(defrecord Op [action preconds add-set del-set])

(defn appropriate? [goal op]
  (contains? (:add-set op) goal))

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
      (every? achieve goals))))

(defn gps-functional
  "General Problem Solver: achieve all goals using ops"
  [initial-state goals ops]
  (letfn [(apply-op [state op]
            (when-let [before-state (achieve-all state (:preconds op))]
              (println "Executing " (:action op))
              (-> before-state
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
    (achieve-all initial-state goals)))

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
  (is (gps-with-state initial-state goals school-ops))
  (is (gps-functional initial-state goals school-ops)))

(let [initial-state #{:son-at-home :car-needs-battery :have-money}
      goals #{:son-at-school}]
  (is (not (gps-with-state initial-state goals school-ops)))
  (is (not (gps-functional initial-state goals school-ops))))

(let [initial-state #{:son-at-home :car-works}
      goals #{:son-at-school}]
  (is (gps-with-state initial-state goals school-ops))
  (is (gps-functional initial-state goals school-ops)))

;;; The Prerequisite Clobbers Sibling Problem

; Should not pass, since we don't have money in the end, but because :have-money
; is checked first we think we do.
(is
  (gps-functional
    (sorted-set :son-at-home :car-needs-battery :have-money :have-phone-book)
    (sorted-set :have-money :son-at-school)
    school-ops))

(defn gps-noclobber
  "General Problem Solver: achieve all goals using ops"
  [initial-state goals ops]
  (letfn [(apply-op [state op]
            (when-let [before-state (achieve-all state (:preconds op))]
              (println "Executing " (:action op))
              (-> before-state
                  (set/difference (:del-set op))
                  (set/union (:add-set op)))))
          (achieve [state goal]
            (if (contains? state goal)
                state
                (some #(and (appropriate? goal %)
                            (apply-op state %))
                      ops)))
          (achieve-all [state goals]
            (when-let [new-state (reduce achieve state goals)]
              (if (subset? goals new-state) new-state)))]
    (achieve-all initial-state goals)))

(is
  (not
    (gps-noclobber
      (sorted-set :son-at-home :car-needs-battery :have-money :have-phone-book)
      (sorted-set :have-money :son-at-school)
      school-ops)))
(is
  (gps-noclobber
    (sorted-set :son-at-home :car-needs-battery :have-money :have-phone-book)
    (sorted-set :son-at-school)
    school-ops))
  