(ns adventofcode.four-test
  (:require [adventofcode.four :as four]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(def letter-list-generator
  (gen/such-that #(not (empty? %))
                 (gen/vector (gen/fmap (comp str/lower-case str) gen/char-alpha))))

(defspec test-group-letters
  100
  (prop/for-all [v letter-list-generator]
                (let [result (four/group-letters v)]
                  (every? (fn [[k v]]
                            (and (integer? k)
                                 (every? (fn [[kk vv]]
                                           (keyword? kk)
                                           (integer? vv))
                                         v)))
                          result))))

(defspec test-sort-letters
  100
  (prop/for-all [v letter-list-generator]
                (let [result ((comp four/sort-letters four/group-letters) v)]
                  (and (not (empty? result))
                       (every?
                        (fn [e]
                          (every? (fn [f]
                                    (re-matches #"[a-z]" f))
                                  e))
                        result)))))

(deftest test-group-letters-value
  (is (= (four/group-letters '("a" "a" "a" "a" "a" "b" "b" "b" "z" "y" "x"))
         '([5 [[:a 5]]] [3 [[:b 3]]] [1 [[:z 1] [:y 1] [:x 1]]]))))

(deftest test-sort-letters-value
  (is (= ((comp four/sort-letters four/group-letters)
          '("a" "a" "a" "a" "a" "b" "b" "b" "z" "y" "x"))
         [["a"] ["b"] ["x" "y" "z"]])))

(deftest test-calculate-checksum
  (is (= (four/calculate-checksum four/input-example-a)
         "abxyz")))

(deftest test-valid-checksum-value
  (is (= (filter four/valid-checksum?
                 [four/input-example-a
                  four/input-example-b
                  four/input-example-c
                  four/input-example-d])
         '("aaaaa-bbb-z-y-x-123[abxyz]" "a-b-c-d-e-f-g-h-987[abcde]" "not-a-real-room-404[oarel]"))))

(deftest test-calculate-value
  (is (= (four/calculate [four/input-example-a
                          four/input-example-b
                          four/input-example-c
                          four/input-example-d])
         1514)))

(deftest test-start-invalid-input
  (is (thrown? Exception (four/start ["foo"]))))
