(ns main.adventofcode.2018.four
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split split-lines replace trim]]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]))


;; [1518-11-01 00:00]
;; (def custom-formatter (f/formatter "'[yyyy-MM-dd HH:mm']"))
(def custom-formatter (f/formatter "yyyy-MM-dd HH:mm"))

(defn isolate-time [r]
  (-> r
      (split #"\]")
      (update-in [0] replace #"\[" "")
      (update-in [1] trim)))

(defn parse-time [r]
  (update-in r [0] (partial f/parse custom-formatter)))


;; parse-time
;; order records
;; id guard
;; calculate duration asleep / awak
;; which guard sleeps longest?
;; which hour is there the most sleep overlap (that guard most likely to sleep)
(comment

  (->> "input.day4" ;; "4.1.input"
       resource
       slurp
       split-lines
       (map isolate-time)
       (map parse-time)
       (sort-by first)))
