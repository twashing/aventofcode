(ns adventofcode.one)


(def directions-some ["R5" "L2" "L" "J30" "L20" "R6"])
(def directions-all ["R5" "L2" "L20" "R6"])
(def directions-none ["R" "2L" "" "58"])

(def directions-ex1 ["R2" "L3"])
(def directions-ex2 ["R2" "R2" "R2"])
(def directions-ex3 ["R5" "L5" "R5" "R3"])

(comment

  ;; A
  (map #(s/conform :one/direction %)
       directions-some)

  ;; B
  (s/conform :one/directions directions-all)
  (s/conform :one/directions directions-some)
  (s/conform :one/directions directions-none))

(comment

  ;; C
  (map #(split-with-delimiter % #"[RL]") directions-all)

  (split-directions directions-all)
  (split-direction (first directions-all)))

(comment

  ;; Good input
  (clojure.pprint/pprint directions-all)

  (def one (start directions-all))
  (clojure.pprint/pprint one)

  (clojure.pprint/pprint (start directions-ex1))
  (clojure.pprint/pprint (start directions-ex2))
  (clojure.pprint/pprint (start directions-ex3)))

(comment

  ;; Bad input
  (start directions-some)
  (start directions-none))
