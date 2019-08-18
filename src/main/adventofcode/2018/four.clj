(ns main.adventofcode.2018.four
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split split-lines replace trim]]
            [clojure.tools.trace :refer [trace]]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]))


(defn read-input [f]
  (-> f
      resource
      slurp
      split-lines))

(def custom-formatter (f/formatter "yyyy-MM-dd HH:mm"))

(defn parse-time [r]
  (update-in r [0] (partial f/parse custom-formatter)))

(defn isolate-time [r]
  (-> r
      (split #"\]")
      (update-in [0] replace #"\[" "")
      (update-in [1] trim)))

(defn calculation-time [l]
  (->> (map isolate-time l)
       (map parse-time)))

(defn group-shifts [l]
  (->> l
       (partition-by (fn [[l r]] (re-find #"Guard #" r)))
       (partition 2)))

(defn flatten-guard-records [l]
  (->> (map flatten l)
       (map #(partition 2 %))))

(defn ->guard-state [s]
  (cond
    (= s "falls asleep") :asleep
    (or (= s "wakes up")
        (re-find #"begins shift" s)) :awake))

(defn format-records [l]
  (let [guard-id (re-find #"\d+" (-> l first second))]
    (map (fn [[l r]]
           {:guard-id guard-id
            :time l
            :state (->guard-state r)})
         l)))

(defn calculate-durations [l]
  (let [last-time (-> l last :time)
        last-midnight (-> last-time t/with-time-at-start-of-day (#(t/plus % (t/hours 1))))
        last-duration (t/in-minutes (t/interval last-time last-midnight))]
    (as-> l x
      (partition 2 1 x)
      (map #(->> %
                 (map :time)
                 (apply t/interval)
                 t/in-minutes) x)
      (concat x [last-duration])
      (map #(assoc %1 :duration %2) l x))))

(defn group-guards [l]
  (->> (group-by #(-> % first :guard-id) l)
       (map (fn [[k v]]
              [k (apply concat v)]))))

(defn ->guard-who-sleeps-most [l]
  (->> (map (fn [e] (filter #(= :asleep (:state %)) e)) l)
       group-guards
       (map (fn [[id l]]
              [id (reduce #(+ %1 (:duration %2)) 0 l)]))
       (sort-by second)
       ((comp first reverse))))

(defn solve [f]
  (let [guard-records (->> (read-input f)
                           calculation-time
                           (sort-by first)
                           group-shifts
                           flatten-guard-records
                           (map format-records)
                           (map calculate-durations))
        [gid _] (->guard-who-sleeps-most guard-records)
        guard-selection (get (->> (group-guards guard-records)
                                  (apply concat)
                                  (apply hash-map))
                             gid)
        hours-asleep (filter #(= :asleep (:state %)) guard-selection)]

    (->> hours-asleep
         (map (fn [e]
                (let [{:keys [time duration]} e]
                  (->> (iterate #(t/plus % (t/minutes 1)) time)
                       (take duration)
                       (map t/minute)))))

         (apply concat)
         frequencies
         seq
         (sort-by second)
         ((comp first last)))))

(comment
  
  (solve "4.1.input")
  (solve "input.day4"))


"1518-11-01T00:55:00.000Z
 1518-11-01T23:58:00.000Z"
