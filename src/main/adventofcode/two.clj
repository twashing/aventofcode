(ns adventofcode.two
  (:require [clojure.spec.alpha :as s]
            [adventofcode.core :refer [check-input]]))


(def possible-keys (range 1 10))
(def keypad (partition 3 possible-keys))


(s/def :two/direction #(re-matches #"[UDLR]{1}" %))
(s/def :two/input-line (s/coll-of :two/direction))
(s/def :two/input-set (s/coll-of :two/input-line))

(s/def :two/allowed-key #(some (into #{} possible-keys) [%]))


(defn find-row [number]
  (flatten (filter #(some (into #{} %) [number]) keypad)))

(defmulti move (fn [x y] x))

(defmethod move "U" [direction {:keys [last-key key move] :as keypad-state}]
  (let [result (- key 3)
        result-final (if (not (= :clojure.spec.alpha/invalid
                                 (s/conform :two/allowed-key result)))
                       result
                       key)]
    (assoc keypad-state
           :move direction
           :last-key result-final
           :key result-final)))

(defmethod move "D" [direction {:keys [last-key key move] :as keypad-state}]
  (let [result (+ key 3)
        result-final (if (not (= :clojure.spec.alpha/invalid
                                 (s/conform :two/allowed-key result)))
                       result
                       key)]
    (assoc keypad-state
           :move direction
           :last-key result-final
           :key result-final)))

(defmethod move "L" [direction {:keys [last-key key move] :as keypad-state}]
  (let [result (- key 1)
        last-row (into #{} (find-row last-key))
        result-final (if (some last-row [result])
                       result
                       key)]
    (assoc keypad-state
           :move direction
           :last-key result-final
           :key result-final)))

(defmethod move "R" [direction {:keys [last-key key move] :as keypad-state}]
  (let [result (+ key 1)
        last-row (into #{} (find-row last-key))
        result-final (if (some last-row [result])
                       result
                       key)]
    (assoc keypad-state
           :move direction
           :last-key result-final
           :key result-final)))


(defn calculate-line [keypad-state line]
  (reduce (fn [acc direction]
            (conj acc (move direction (last acc))))
          keypad-state
          line))

(defn calculate [keypad-state input-set]
  (reduce (fn [acc ech-set]

            (let [result (calculate-line acc ech-set)
                  {:keys [code key] :as last-result} (last result)]

              (into [] (concat (butlast result)
                               [(assoc last-result
                                       :code (conj code key))]))))
          keypad-state
          input-set))

(defn start [input-set]

  ;; Same here - We can explicitly check input or turn on spec instrumentation
  (check-input :two/input-set input-set)

  (-> (calculate [{:move nil
                   :last-key nil
                   :key 5
                   :code []}]
                 input-set)
      last :code))
