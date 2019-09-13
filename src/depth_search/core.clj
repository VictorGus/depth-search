(ns depth-search.core
  (:require [clojure.string :as string]))

(def state (atom {}))

(def checked-states (atom {}))

(defn init-state [instate]
  (let [array-of-keys [:a :b :c :d :e :f :g :h :i]
        initialized-state (cond
                            (= clojure.core$atom (type instate))
                            instate
                            (vector? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys instate)))
                            (list? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys (vec instate))))
                            (string? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys (vec (map #(Character/digit % 10) (filter #(not (= % \space)) (vec instate)))))))
                            :else (throw (Exception. (str "Input value is supposed to be string, vector or list, but got " (type instate)))))]
    (reset-meta! initialized-state {:gap (nth (->> @initialized-state
                                                   (keep #(if (= 0 (val %)) (key %)))) 0)})
    initialized-state))

(defn convert-state-to-string [state]
  (-> @state
      vals
      vec
      string/join))

(defn make-state-checked [state]
  (swap! checked-states #(assoc % (convert-state-to-string state) nil)))

(defn state-checked? [state]
  (let [converted-state (convert-state-to-string state)]
    (contains? @checked-states converted-state)))

(defn state-goal? [state goal-state]
  (= @state @goal-state))

(defn swap-state [state]
  (let [moved-state state
        result-state (cond
                       (and (= :a (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :a (:b @moved-state))
                                                                                                               (assoc :b 0)))))))

                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :a (:b @moved-state))
                                                    (assoc :b 0))))

                       (and (= :a (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :a (:d @moved-state))
                                                                                                               (assoc :d 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :a (:d @moved-state))
                                                    (assoc :d 0))))

                       (and (= :b (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :b (:a @moved-state))
                                                                                                               (assoc :a 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :b (:a @moved-state))
                                                    (assoc :a 0))))
                       (and (= :b (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :b (:c @moved-state))
                                                                                                               (assoc :c 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :b (:c @moved-state))
                                                    (assoc :c 0))))

                       (and (= :b (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :b (:e @moved-state))
                                                                                                               (assoc :e 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :b (:e @moved-state))
                                                    (assoc :e 0))))

                       (and (= :c (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :c (:b @moved-state))
                                                                                                               (assoc :b 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :c (:b @moved-state))
                                                    (assoc :b 0))))

                       (and (= :c (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :c (:f @moved-state))
                                                                                                               (assoc :f 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :c (:f @moved-state))
                                                    (assoc :f 0))))

                       (and (= :d (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :d (:a @moved-state))
                                                                                                               (assoc :a 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :d (:a @moved-state))
                                                    (assoc :a 0))))

                       (and (= :d (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :d (:e @moved-state))
                                                                                                               (assoc :e 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :d (:e @moved-state))
                                                    (assoc :e 0))))

                       (and (= :d (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :d (:g @moved-state))
                                                                                                               (assoc :g 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :d (:g @moved-state))
                                                    (assoc :g 0))))

                       (and (= :e (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :e (:f @moved-state))
                                                                                                               (assoc :f 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :e (:f @moved-state))
                                                    (assoc :f 0))))

                       (and (= :e (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :e (:h @moved-state))
                                                                                                               (assoc :h 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :e (:h @moved-state))
                                                    (assoc :h 0))))

                       (and (= :e (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :e (:b @moved-state))
                                                                                                               (assoc :b 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :e (:b @moved-state))
                                                    (assoc :b 0))))

                       (and (= :e (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :e (:d @moved-state))
                                                                                                               (assoc :d 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :e (:d @moved-state))
                                                    (assoc :d 0))))
                       (and (= :f (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :f (:i @moved-state))
                                                                                                               (assoc :i 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :f (:i @moved-state))
                                                    (assoc :i 0))))

                       (and (= :g (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :g (:h @moved-state))
                                                                                                               (assoc :h 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :g (:h @moved-state))
                                                    (assoc :h 0))))

                       (and (= :g (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :g (:d @moved-state))
                                                                                                               (assoc :d 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :g (:d @moved-state))
                                                    (assoc :d 0))))

                       (and (= :h (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :h (:i @moved-state))
                                                                                                               (assoc :i 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :h (:i @moved-state))
                                                    (assoc :i 0))))

                       (and (= :h (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :h (:e @moved-state))
                                                                                                               (assoc :e 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :h (:e @moved-state))
                                                    (assoc :e 0))))

                       (and (= :h (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :h (:g @moved-state))
                                                                                                               (assoc :g 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :h (:g @moved-state))
                                                    (assoc :g 0))))

                       (and (= :i (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :i (:h @moved-state))
                                                                                                               (assoc :h 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :i (:h @moved-state))
                                                    (assoc :h 0))))

                       (and (= :i (:gap (meta moved-state))) (not (state-checked? (atom (into (sorted-map) (-> @moved-state
                                                                                                               (assoc :i (:f @moved-state))
                                                                                                               (assoc :f 0)))))))
                       (atom (into (sorted-map) (-> @moved-state
                                                    (assoc :i (:f @moved-state))
                                                    (assoc :f 0)))))]
    (make-state-checked result-state)
    (make-state-checked state)
    result-state))

(defn search-for-goal-state [state goal-state]
  (loop [moved-state (init-state state)]
    (when (not (state-goal? moved-state goal-state))
      (println @moved-state)
      (recur (swap-state moved-state)))))

(defn -main []
  (search-for-goal-state "123405678" (init-state "123456780")))
