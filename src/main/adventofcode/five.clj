(ns adventofcode.five
  (:import java.security.MessageDigest
           java.math.BigInteger))

;; The eight-character password for the door is generated one character at a time by
;; - finding the MD5 hash of some Door ID (your puzzle input) and
;; - an increasing integer index (starting with 0).
;;
;; A hash indicates the next character in the password if
;; - its hexadecimal representation starts with five zeroes. If it does,
;; - the sixth character in the hash is the next character of the password.

(defn ->md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))


;; - find password, given a [door ID] + [index]

(defn password-character-match? [md5]
  (re-matches #"00000\w+" md5))

(defn matching-indexes [door-id]

  (let [index-range (range)

        join-door-id-and-index #(str door-id %)
        door-id-and-index->md5 (fn [door-id-and-index]
                                 {:door-id-and-index door-id-and-index
                                  :md5 (->md5 door-id-and-index)})
        password-character-match-facade (fn [{md5 :md5}] (password-character-match? md5))]

    (->> (map join-door-id-and-index index-range)
         (map door-id-and-index->md5)
         (filter password-character-match-facade))))

(defn find-password

  ([door-id]
   (find-password door-id (matching-indexes door-id)))

  ([_ matching-index-seq]
   (let [extract-password-character
         (fn [md5]
           (-> (seq md5)
               (nth 5)
               str))]
     (->> matching-index-seq
          (map :md5)
          (map extract-password-character)
          (apply str)))))

(comment

  (def one (matching-indexes "abc"))
  (def two (matching-indexes "ojvtpuvg"))

  (def three '({:door-id-and-index "abc3231929", :md5 "00000155f8105dff7f56ee10fa9b9abd"}
               {:door-id-and-index "abc5017308", :md5 "000008f82c5b3924a1ecbebf60344e00"}
               {:door-id-and-index "abc5278568", :md5 "00000f9a2c309875e05c5a5d09f1b8c4"}
               {:door-id-and-index "abc5357525", :md5 "000004e597bd77c5cd2133e9d885fe7e"}
               {:door-id-and-index "abc5708769", :md5 "0000073848c9ff7a27ca2e942ac10a4c"}
               {:door-id-and-index "abc6082117", :md5 "00000a9c311683dbbf122e9611a1c2d4"}
               {:door-id-and-index "abc8036669", :md5 "000003c75169d14fdb31ec1593915cff"}
               {:door-id-and-index "abc8605828", :md5 "0000000ea49fd3fc1b2f10e02d98ee96"}))

  (find-password "abc" three)

  (find-password "abc")
  (find-password "ojvtpuvg")

  )
