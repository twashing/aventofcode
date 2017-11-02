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

(comment

  ;; Official input
  (def official-input
    [ "L1" "L5" "R1" "R3" "L4" "L5" "R5" "R1" "L2" "L2" "L3" "R4" "L2" "R3" "R1" "L2" "R5" "R3" "L4" "R4" "L3" "R3" "R3" "L2" "R1" "L3" "R2" "L1" "R4" "L2" "R4" "L4" "R5" "L3" "R1" "R1" "L1" "L3" "L2" "R1" "R3" "R2" "L1" "R4" "L4" "R2" "L189" "L4" "R5" "R3" "L1" "R47" "R4" "R1" "R3" "L3" "L3" "L2" "R70" "L1" "R4" "R185" "R5" "L4" "L5" "R4" "L1" "L4" "R5" "L3" "R2" "R3" "L5" "L3" "R5" "L1" "R5" "L4" "R1" "R2" "L2" "L5" "L2" "R4" "L3" "R5" "R1" "L5" "L4" "L3" "R4" "L3" "L4" "L1" "L5" "L5" "R5" "L5" "L2" "L1" "L2" "L4" "L1" "L2" "R3" "R1" "R1" "L2" "L5" "R2" "L3" "L5" "L4" "L2" "L1" "L2" "R3" "L1" "L4" "R3" "R3" "L2" "R5" "L1" "L3" "L3" "L3" "L5" "R5" "R1" "R2" "L3" "L2" "R4" "R1" "R1" "R3" "R4" "R3" "L3" "R3" "L5" "R2" "L2" "R4" "R5" "L4" "L3" "L1" "L5" "L1" "R1" "R2" "L1" "R3" "R4" "R5" "R2" "R3" "L2" "L1" "L5"])

  (def official-output (start official-input))
  (clojure.pprint/pprint official-output))


