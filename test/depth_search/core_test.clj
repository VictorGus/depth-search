(ns depth-search.core-test
  (:require [clojure.test :refer :all]
            [depth-search.core :refer :all]))

(def firstTestState (atom (into (sorted-map) {:a 3 :b 4 :c 5
                                              :d 1 :e 2 :f 7
                                              :g 8 :h 9 :i 0})))

(def secondTestState (atom (into (sorted-map) {:a 3 :b 4 :c 5
                                               :d 1 :e 0 :f 7
                                               :g 8 :h 2 :i 9})))

(defn add-meta-fixture [f]
  (reset-meta! firstTestState {:gap :i})
  (reset-meta! secondTestState {:gap :e})
  (f))

(use-fixtures :once add-meta-fixture)

(deftest conversion-to-string-test
  (testing "Conversion from state to string"
    (is (= (convert-state-to-string firstTestState) "345127890"))
    (is (= (convert-state-to-string (atom {})) ""))))

(deftest make-state-checked-test
  (reset! checked-states {})
  (testing "Making state checked"
    (is (= (make-state-checked firstTestState) {"345127890" nil}))))

(deftest state-checked-test
  (testing "Has the test been already checked?"
    (is (= (state-checked? (atom {})) false))
    (is (= (state-checked? firstTestState) true))))

(reset! checked-states {"345127890" nil
                        "345127809" nil
                        "345017829" nil
                        "345170829" nil
                        "035146829" nil})


(deftest swap-state-test
  (testing "Has the state been changed properly?"
    (is (= @(swap-state firstTestState) @(atom (into (sorted-map) {:a 3 :b 4 :c 5
                                                                   :d 1 :e 2 :f 0
                                                                   :g 8 :h 9 :i 7}))))

    (is (= @(swap-state secondTestState) @(atom (into (sorted-map) {:a 3 :b 0 :c 5
                                                                   :d 1 :e 4 :f 7
                                                                    :g 8 :h 2 :i 9}))))))

;; (deftest change-state-test
;;   (testing "Has the state been changed properly?"
;;     (is (= (change-state firstTestState) (atom (into (sorted-map) {:a 3 :b 4 :c 5
;;                                                                      :d 1 :e 2 :f 0
;;                                                                      :g 8 :h 9 :i 7}))))

;;     (is (= (change-state secondTestState) (atom (into (sorted-map) {:a 3 :b 0 :c 5
;;                                                                       :d 1 :e 4 :f 7
;;                                                                       :g 8 :h 2 :i 9}))))

;;     (is (= (change-state thirdTestState) (atom (into (sorted-map) {:a 3 :b 5 :c 0
;;                                                                      :d 1 :e 4 :f 7
;;                                                                      :g 8 :h 2 :i 9}))))))

(deftest initialize-state-test
  (testing "Has the state been initialized properly?"
    (is (= @(init-state [1 0 2 3 4 5 6 7 8]) (into (sorted-map) {:a 1 :b 0 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 8})))
    (is (= @(init-state '(1 0 2 3 4 5 6 7 8)) (into (sorted-map) {:a 1 :b 0 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 8})))
    (is (= @(init-state "1 0 2 3 4 5 6 7 8") (into (sorted-map) {:a 1 :b 0 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 8})))
    (is (= @(init-state "102345678") (into (sorted-map) {:a 1 :b 0 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 8})))
    (is (thrown? Exception (init-state 1234)))
    (is (= (:gap (meta (init-state "102345678"))) :b))))
