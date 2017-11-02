(ns adventofcode.four
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [split join]]
            [adventofcode.core :refer [check-input]]))


(s/def :four/code-input #(re-matches #"[[a-z]+\-]+[0-9]+\[[a-z]+\]" %))
(s/def :four/code-inputs (s/coll-of :four/code-inputs))

(def input-example-a "aaaaa-bbb-z-y-x-123[abxyz]")
(def input-example-b "a-b-c-d-e-f-g-h-987[abcde]")
(def input-example-c "not-a-real-room-404[oarel]")
(def input-example-d "totally-real-room-200[decoy]")

(def input-bad-a "aaa-bb-c[abcde]")
(def input-bad-b "aaa-bb-c[abcde]")


;; A room is real if
;; i. the checksum is the five most common letters in the encrypted name,
;; ii. in order,
;; iii. with ties broken by alphabetization.


(defn group-letters [letters]
  (let [coalated (reduce (fn [acc e]
                           (let [lkeyword (keyword e)
                                 lcount (lkeyword acc)
                                 lcount (if (nil? lcount)
                                          0 lcount)]
                             (assoc acc lkeyword (inc lcount))))
                         {} letters)]
    (reverse
     (sort-by first
              (seq (group-by second (seq coalated)))))))

(defn sort-letters [letters-grouped]
  (reduce (fn [acc [fst snd]]
            (let [values-sorted (sort-by first snd)]
              (conj acc
                    (reduce (fn [ac [f s]]
                              (conj ac (name f)))
                            [] values-sorted))))
          [] letters-grouped))

(defn calculate-checksum [input]
  (let [letters (map str
                     (mapcat #(seq (char-array %))
                             (butlast (split input #"\-"))))
        letters-grouped (group-letters letters)
        letters-sorted (sort-letters letters-grouped)]

    (join (map str (take 5 (join (apply concat letters-sorted)))))))

(defn input-checksum [input]
  (let [input-cropped (re-find #"\[[a-z]+\]" input)]
    (subs input-cropped 1 (dec (count input-cropped)))))

(defn valid-checksum? [input]
  (let [checksum-from-input (input-checksum input)
        checksum (calculate-checksum input)]
    (= checksum-from-input
       checksum)))

(defn input-sector-id [input]
  (Integer/parseInt
   (re-find #"\d+" (last (split input #"\-")))))

(defn calculate [inputs]
  (let [valid-records (filter valid-checksum? inputs)]
    (reduce (fn [acc input]
              (let [sector-id (input-sector-id input)]
                (+ acc sector-id)))
            0 valid-records)))

(defn start [inputs]

  (check-input :four/code-inputs inputs)

  (calculate inputs))
