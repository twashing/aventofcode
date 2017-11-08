(ns adventofcode.one-test
  (:require [adventofcode.one :as one]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [plumula.mimolette.alpha :refer [defspec-test]]))


(def instruction-generator (gen/let [direction (gen/elements ["R" "L"])
                                     distance gen/nat]
                             (str direction distance)))

(def instructions-generator (gen/such-that #(not (empty? %))
                                           (gen/vector instruction-generator)))

(defn check-instruction-element [[dir dist]]
  (and (some #{"R" "L"} [dir])
       (number? dist)))

(defn check-instruction-split [[dir dist]]
  (and (some #{"R" "L"} [dir])
       (and (string? dist)
            (not (empty? dist)))))


(defspec test-split-with-delimiter
  100
  (prop/for-all [v instruction-generator]
                (let [instructions-split (one/split-with-delimiter v (subs v 0 1))]
                  (check-instruction-split instructions-split))))

(defspec test-split-direction
  100
  (prop/for-all [v instruction-generator]
                (let [[dir dist] (one/split-direction v)]
                  (check-instruction-element [dir dist]))))

(defspec test-split-directions
  100
  (prop/for-all [v instructions-generator]
                (let [split-directions (one/split-directions v)]
                  (every? check-instruction-element split-directions))))

(defspec test-calculate
  100
  (prop/for-all [v instructions-generator]
                (let [directions-final (one/calculate v)]
                  (every? #(= [:blocks-from-start :coordinates :orientation :step]
                              (-> % keys sort))
                          directions-final))))

(deftest test-split-directions-values
  (is (= (one/split-directions one/directions-all)
         '(["R" 5] ["L" 2] ["L" 20] ["R" 6]))))

(deftest test-split-direction-values
  (is (= (one/split-direction (first one/directions-all))
         ["R" 5])))

(deftest test-calculate-values
  (is (= (one/calculate one/directions-all)
         [{:step 0, :coordinates [0 0], :blocks-from-start 0, :orientation :N} {:step 1, :orientation :E, :coordinates [5 0], :blocks-from-start 5} {:step 2, :orientation :N, :coordinates [5 2], :blocks-from-start 7} {:step 3, :orientation :W, :coordinates [-15 2], :blocks-from-start 17} {:step 4, :orientation :N, :coordinates [-15 8], :blocks-from-start 23}])))

(deftest test-start-invalid-input
  (is (thrown? Exception (one/start ["R" "2L" "" "58"]))))
