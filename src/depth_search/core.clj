(ns depth-search.core
  (:require [clojure.string :as string]))

(def state (atom {}))

(def checked-states (atom {}))

(defn init-state [instate]
  (let [array-of-keys [:a :b :c :d :e :f :g :h :i]
        initialized-state (cond
                            (vector? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys instate)))
                            (list? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys (vec instate))))
                            (string? instate)
                            (atom (into (sorted-map) (zipmap array-of-keys (vec (map #(Character/digit % 10) (filter #(not (= % \space)) (vec instate)))))))
                            :else (throw (Exception. (str "Input value is supposed to be string, vector or list, but got " (type instate)))))]
    (reset-meta! initialized-state {:gap (nth (keep #(if (= 0 (val %)) (key %)) @initialized-state) 0)})
    initialized-state))

(defn convert-state-to-string [state]
  (string/join (vec (vals @state))))

(defn make-state-checked [state]
  (swap! checked-states #(assoc % (convert-state-to-string state) nil)))

(defn state-checked? [state]
  (let [converted-state (convert-state-to-string state)]
    (contains? @checked-states converted-state)))

(defn state-goal? [state goal-state]
  (= @state @goal-state))

(defn change-state [state]
  (atom {}))
