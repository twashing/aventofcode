(ns adventofcode.two)


(def input-line-good ["U" "L" "L"])
(def input-line-bad ["Z" "O" "D"])
(def input-set
  [["U" "L" "L"]
   ["R" "R" "D" "D" "D"]
   ["L" "U" "R" "D" "L"]
   ["U" "U" "U" "U" "D"]])
(def input-set-bad
  [["U" "L" "Z"]
   ["R" "R" "D" "D" "D"]])


(defn calculate [input-lines])


(comment

  ;; A
  (s/conform :two/direction "U")
  (s/conform :two/direction "LL")
  (s/conform :two/direction "ZD")

  ;; B
  (s/conform :two/input-line input-line-good)
  (s/explain-data :two/input-line input-line-bad)

  ;; C
  (s/conform :two/input-set input-set)

  ;; D - good input key
  (s/conform :two/allowed-key 3)

  ;; E - bad input key
  (s/conform :two/allowed-key 46)
  (check-input :two/allowed-key 46))

(comment

  (find-row 5)
  (find-row 7)

  (move "U" {:move nil
             :last-key 5
             :key nil})
  (move "U" {:move nil
             :last-key 2
             :key nil})
  (move "D" {:move nil
             :last-key 9
             :key nil})

  (move "L" {:move nil
             :last-key 3
             :key nil})
  (move "L" {:move nil
             :last-key 1
             :key nil})
  (move "R" {:move nil
             :last-key 4
             :key nil})
  (move "R" {:move nil
             :last-key 9
             :key nil}))

(comment

  (def two-line (calculate-line [{:move nil
                                  :last-key 5
                                  :key 5}]
                                ["U" "L" "L"]))

  (clojure.pprint/pprint two-line)

  (clojure.pprint/pprint (calculate-line [{:move nil
                                           :last-key 1
                                           :key 1}]
                                         ["R" "R" "D" "D" "D"]))

  (clojure.pprint/pprint (calculate-line [{:move nil
                                           :last-key 9
                                           :key 9}]
                                         ["L" "U" "R" "D" "L"])))

(comment

  (def two-set (calculate [{:move nil
                            :last-key nil
                            :key 5}]
                          input-set))

  (clojure.pprint/pprint two-set))

(comment

  (start input-set)
  (start input-set-bad))
