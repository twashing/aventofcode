(ns adventofcode.two-test
  (:require [adventofcode.two :as two]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(def one-though-nine-generator (gen/choose 1 9))

(def move-up-generator (gen/choose 4 6))
(def move-down-generator (gen/choose 1 6))
(def move-right-generator (gen/such-that #(not= 0 (rem % 3))
                                         one-though-nine-generator))
(def move-left-generator (gen/elements [2 3 5 6 8 9]))

(def direction-generator (gen/such-that #(not (empty? %))
                          (gen/vector (gen/elements ["U" "D" "L" "R"]))))

(defspec test-find-row-valid
  100
  (prop/for-all [v one-though-nine-generator]
                (let [result (two/find-row v)]
                  (and (= 3 (count result))
                       (some (into #{} result) [v])))))

(defspec test-find-row-invalid
  100
  (prop/for-all [v (gen/such-that #(not (some (into #{} (range 1 10)) [%])) gen/large-integer)]
                (let [result (two/find-row v)]
                  (empty? result))))

(defspec test-move-up
  100
  (prop/for-all [v move-up-generator]
                (let [{key :key} (two/move "U" {:move nil :last-key v :key v})]
                  (= key (- v 3)))))

(defspec test-move-down
  100
  (prop/for-all [v move-down-generator]
                (let [{key :key} (two/move "D" {:move nil :last-key v :key v})]
                  (= key (+ v 3)))))

(defspec test-move-left
  100
  (prop/for-all [v move-left-generator]
                (let [{key :key} (two/move "L" {:move nil :last-key v :key v})]
                  (= key (- v 1)))))

(defspec test-move-right
  100
  (prop/for-all [v move-right-generator]
                (let [{key :key} (two/move "R" {:move nil :last-key v :key v})]
                  (= key (+ v 1)))))


(defspec test-calculate-line
  100
  (prop/for-all [v direction-generator]

                (let [result (two/calculate-line [{:move nil
                                                   :last-key 5
                                                   :key 5}]
                                                 v)]

                  (every? #(some #{:move :last-key :key} (keys %))
                          result))))

(defspec test-calculate
  100
  (prop/for-all [v (gen/such-that #(not (empty? %))
                                  (gen/vector direction-generator))]

                (let [result (two/calculate [{:move nil
                                              :last-key 5
                                              :key 5}]
                                            v)]

                  (every? #(some #{:move :last-key :key} (keys %))
                          result))))


(two/calculate-line [{:move nil
                      :last-key 5
                      :key 5}]
                    ["U" "L" "L"])

(two/calculate-line [{:move nil
                      :last-key 1
                      :key 1}]
                    ["R" "R" "D" "D" "D"])

(two/calculate-line [{:move nil
                      :last-key 9
                      :key 9}]
                    ["L" "U" "R" "D" "L"])

(two/calculate [{:move nil
                 :last-key nil
                 :key 5
                 :code []}]
               two/input-set)

