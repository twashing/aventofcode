(ns adventofcode.one
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [adventofcode.core :refer [check-input]]))


(def directions [:N :E :S :W])
(def direction-mapping
  (apply merge
         (map (fn [e]
                {(second e) [(first e) (last e)]})
              (partition 3 1 (take 6 (cycle directions))))))

(defn split-with-delimiter [s d]
  (str/split s (re-pattern (str "(?=" d ")|(?<=" d ")"))))

(defn split-direction [direction]
  (let [[direction distance] (split-with-delimiter direction "[RL]")]
    [direction (Integer/parseInt distance)]))

(defn split-directions [directions]
  (map #(split-direction %) directions))

(defn calculate-blocks-from-start [coordinates]
  (apply + (map abs coordinates)))

(defn calculate [directions]
  (reduce (fn [acc ech]
            (let [{:keys [step coordinates blocks-from-start orientation]} (last acc)
                  [move-direction move-distance] (split-direction ech)

                  step-new (inc step)

                  [toL toR] (orientation direction-mapping)
                  orientation-new (if (= "L" move-direction)
                                    toL toR)

                  coordinates-new (if (some #{:N :S} [orientation-new])
                                    (do
                                      [(first coordinates)
                                       (if (= :N orientation-new)
                                         (+ (second coordinates) move-distance)
                                         (- (second coordinates) move-distance))])
                                    (do
                                      [(if (= :E orientation-new)
                                         (+ (first coordinates) move-distance)
                                         (- (first coordinates) move-distance))
                                       (second coordinates)]))

                  blocks-from-start-new (calculate-blocks-from-start coordinates-new)]

              (conj acc {:step step-new
                         :orientation orientation-new
                         :coordinates coordinates-new
                         :blocks-from-start blocks-from-start-new})))
          [{:step 0
            :coordinates [0 0] ;; x, y coordinate system
            :blocks-from-start 0
            :orientation :N}]
          directions))

(defn start [directions]

  ;; We can do this or turn on instrumentation
  (check-input :one/directions directions)

  (calculate directions))
