(ns adventofcode.four)


(comment

  (re-matches #"[[a-z]+\-]+" "asdf-")
  (re-matches #"[[a-z]+\-]+" "asdf-qwerty-")

  (re-matches #"[[a-z]+-]+" "asdf")  ;;
  (re-matches #"[[a-z]+-]+" "1234")  ;; x


  (re-matches #"[[a-z]+\-]+[0-9]+" "asdf-qwerty-")  ;; x
  (re-matches #"[[a-z]+\-]+[0-9]+" "asdf-qwerty-1234")
  (re-matches #"[[a-z]+\-]+[0-9]+" "asdf-qwerty-1234[zxcv]")  ;; x
  (re-matches #"[[a-z]+\-]+[0-9]+\[[a-z]+\]" "asdf-qwerty-1234[zxcv]"))

(comment

  (start input-example-a)
  (start input-bad-a))

(comment

  (join (butlast (split input-example-a #"\-")))

  (mapcat #(seq (char-array %))
          (butlast (split input-example-a #"\-")))


  (def a (group-letters '("a" "a" "a" "a" "a" "b" "b" "b" "z" "y" "x")))
  ;; {:a 5, :b 3, :z 1, :y 1, :x 1}

  (def b (reverse
          (sort-by first
                   (seq (group-by second (seq one))))))
  ;; ([5 [[:a 5]]] [3 [[:b 3]]] [1 [[:z 1] [:y 1] [:x 1]]])

  (def c (sort-letters b))
  ;; [["a"] ["b"] ["x" "y" "z"]]

  (def checksum-a (calculate-checksum input-example-a))
  (def checksum-b (calculate-checksum input-example-b))
  (def checksum-c (calculate-checksum input-example-c))
  (def checksum-d (calculate-checksum input-example-d)))

(comment

  (def d (re-find #"\[[a-z]+\]" input-example-a))

  (subs d 1 (dec (count d)))

  (filter valid-checksum?
          [input-example-a
           input-example-b
           input-example-c
           input-example-d])

  (input-sector-id input-example-a)
  (input-sector-id input-example-b)
  (input-sector-id input-example-c)

  (calculate [input-example-a
              input-example-b
              input-example-c
              input-example-d]))
