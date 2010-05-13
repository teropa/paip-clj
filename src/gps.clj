(ns gps
  (:use [clojure.set :as set])
  (:use [clojure.test]))

(defrecord Op [action preconds add-set del-set])

(defn gps [initial-state goals ops]
  "General Problem Solver: achieve all goals using ops"
  (let [state (atom initial-state)]
    (letfn [(apply-op [op]
              (when (every? achieve (:preconds op))
                (println "Executing " (:action op))
                (swap! state set/difference (:del-set op))
                (swap! state set/union (:add-set op))))
            (appropriate? [goal op]
              ((:add-set op) goal))
            (achieve [goal]
              (or (@state goal)
                  (some apply-op
                        (filter (partial appropriate? goal)
                                ops))))]
      (if (every? achieve goals) :solved))))


;;; Test

(def *school-ops*
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

(is
  (= :solved
     (gps
       #{:son-at-home :car-needs-battery :have-money :have-phone-book}
       #{:son-at-school}
       *school-ops*)))

(is
  (nil?
    (gps
      #{:son-at-home :car-needs-battery :have-money}
      #{:son-at-school}
      *school-ops*)))

(is
  (= :solved
     (gps
       #{:son-at-home :car-works}
       #{:son-at-school}
       *school-ops*)))




