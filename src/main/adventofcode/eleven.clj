(ns main.adventofcode.eleven
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [trace]]
            [clojure.math.combinatorics :as com]
            [adventofcode.eleven.neighbours
             :refer [find-neighbour collect-neighbours group-floors
                     get-neighbours floor->neighbours]]))

;; SUMMARIEZED

;; Generators (RTG), Microchips (Chip)
;;
;; Bring RTGs and Chips to the fourth floor.
;;
;; Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is
;;
;; Its capacity rating means it can carry at most:
;;   you + two RTGs or microchips in any combination.
;;
;; ** The elevator will only function if it contains at least one RTG or microchip.
;;
;; an elevator can move between the four floors, one floor at a time.
;; ** The elevator always stops on each floor
;;
;; Chips and RTGs can fry each other,
;;   if they are i) on the elevator, ii) on the floor, or iii) crossing each other on the elevator and floor
;;   you can prevent this if a Chip and its RTG are on the same floor; this way, as they can be connected while the elevator is recharging.
;;
;; Must keep chips connected to their corresponding RTG, when they're in the same room, and away from other RTGs otherwise.
;;
;; ** the elevator will start on the first floor.
;;
;;
;; F# for a Floor number,
;; E for Elevator,
;; H for Hydrogen,
;; L for Lithium,
;; M for Microchip,
;; G for Generator
;;
;; So these are possible Generator / Chip combinations
;; LG - Lithium Generator
;; LM - Lithium Chip
;; HG - Hydrogen Generator
;; HM - Hydrogen Chip
;;
;;
;; F4 .  .  .  .  .
;; F3 .  .  .  LG .
;; F2 .  HG .  .  .
;; F1 E  .  HM .  LM


;; DISTILLED

;; ** start on the first floor
;; ** find a path to the fourth floor


;; > RULES

;; elevator can only travel 1 floor at a time
;; elevator must travel with at least 1 chip or generator
;; elevator must travel with at most 2 chips or generators, in any combination

;; an incompatible chip or generator can fry each other if they are i) on the elevator, ii) on the floor, or iii) crossing each other on the elevator and floor
;;   can prevent this if a Chip and its RTG are connected on the same floor


;; --
;; can move iff
;;   source constraints are met
;;   destination constraints are met
;;   transit constraints are met (incompatible corgo cannot cross each other)
;; if a condition is not met, what is the "closest" move we can make


(defn manipulate-cargo [floors mfn from & cargo]
  (as-> floors e
    (get e from)
    (:floor e)
    (mfn #(some (into #{} cargo) [%]) e)))

(defn get-cargo [floors from & cargo]
  (apply (partial manipulate-cargo floors filter from) cargo))

(defn remove-cargo [floors from & cargo]
  (update-in floors [from :floor]
             (fn [e]
               (remove #(some (into #{} cargo)
                              [%])
                       e))))

(defn put-cargo [floors to & cargo]
  (update-in floors [to :floor] concat cargo))


(def compatible-cargo {:LG :LM :HG :HM})
(def compatible-cargo-entries (seq compatible-cargo))
(def cargo-set (->> compatible-cargo-entries
                    (apply concat)
                    (into #{})))

(def floors {4 {:bay nil :floor []}
             3 {:bay nil :floor [:LG]}
             2 {:bay nil :floor [:HG]}
             1 {:bay :E :floor [:HM :LM]}})

(defn move [floors from to & cargo]
  {:pre [(some cargo-set cargo)]}

  (as-> cargo xs
    (apply (partial remove-cargo floors from) xs)
    (apply (partial put-cargo xs to) cargo)
    (update-in xs [from :bay] (constantly nil))
    (update-in xs [to :bay] (constantly :E))))


;; [ok] list all possible moves from floor 1 to 4
;;   remove stubs
;;   give each move, a rank of :infinity

;; > Move Ranking
;;   how many pieces does move move (the more, the better)
;;   does move go up or down (up is better)

;; iterate through starting move combinations (combinations of floor contents)
;;   for each combination:
;;     check that constraints are met, on the i) from :floor and ii) to :floor
;;     rank that move

;; find "closest" move
;;   find neighbouring floors
;;   find a legal move to that floor
;;   rank each legal move

;; list possible moves, and ranking
;; sort all possible moves, by rank

;; ** take all possible ranked paths, in order
;; ** eliminate dead-ends


(defn location [floors]
  (->> floors
       (filter (fn [[k v]] (= (:bay v) :E)))
       first))

(defn compatible-units? [cargo-in-transit destination-cargo]
  (let [to-sets (partial map #(into #{} %))]
    (if (= 1 (count cargo-in-transit))
      [cargo-in-transit]
      (for [aset  (to-sets compatible-cargo-entries)
            bset  (to-sets (com/combinations cargo-in-transit 2))
            :when (= aset bset)]
        bset))))

(defn calculate-possible-moves [from-floor to-floor]

  ;; All constraints must be true when on any floor, or the elevator is crossing a floor
  ;; [ok] constraint: elevator 1 floor at a time
  ;; [ok] constraint: at least 1 chip or generator
  ;; [ok] constraint: at most 2 things, chips or generators in any combinator (given)
  ;; [ok] constraint: on floor (and elevator) like chip / generator must exist
  ;; [ok] constraint: 2 generators or chips can travel together

  (for [[from {fcargo :floor :as f}] [from-floor]
        [to {gcargo :floor :as g}]   [to-floor]
        fcargoS                      (com/subsets fcargo)
        :when                        (and (<= (count fcargoS) 2)
                                          (>= (count fcargoS) 1)
                                          ((comp not empty?) (compatible-units? fcargoS gcargo)))]
    {:from from :to to :cargo fcargoS :paths []}))

(defn goal-reached? [fs {:keys [from to cargo]}]
  (let [new-fs             (apply (partial move fs from to) cargo)
        {new-cargo :floor} (location new-fs)]
    (if (= (into #{} new-cargo)
             cargo-set)
      new-fs
      false)))

(defn possible-move->goal-reached? [{from :from to :to cargo :cargo :as p}]
  (let [fs (apply (partial move floors from to) cargo)]
    (goal-reached? fs p)))

(declare possible-paths)

(defn possible-move->possible-paths [floors {from :from to :to cargo :cargo}]
  (let [fs  (apply (partial move floors from to) cargo)
        loc (location fs)
        ns  (floor->neighbours fs to)]
    (possible-paths fs loc ns)))

(def move-history (atom #{}))

(defn duplicate-move? [move]
  (if (some @move-history (into #{} [move]))
    true
    (do
      (swap! move-history conj move)
      false)))

(defn possible-paths [floors loc neighbours]
  (let [possible-move-xf (comp (map (partial calculate-possible-moves loc))
                            (remove duplicate-move?))
        goal-reached-xf (comp (map possible-move->goal-reached?)
                           (remove false?))]
    (println "")
    (if-let [ps (trace (seq (flatten (sequence possible-move-xf neighbours))))]
      (if-let [gr (seq (sequence goal-reached-xf (trace ps)))]
        ps
        (map (partial possible-move->possible-paths floors) ps)))))


(comment


  ;; NEIGHBOURS for a floor
  (pprint (floor->neighbours floors 4))
  (pprint (floor->neighbours floors 3))
  (pprint (floor->neighbours floors 2))
  (pprint (floor->neighbours floors 1))


  ;; BASIC MOVE
  (move floors 3 1 :LG)
  (move floors 1 2 :HM)
  (remove-cargo floors 1 :HM)


  ;; POSSIBLE MOVES (one)
  (def floors1 {4 {:bay nil :floor []}
                3 {:bay nil :floor [:LG]}
                2 {:bay nil :floor [:HG]}
                1 {:bay :E :floor [:HM]}})

  (let [[loc floor] (location floors1)
        neighbours (floor->neighbours floors1 1)]
    (calculate-possible-moves [loc floor] neighbours))


  ;; POSSIBLE MOVES (none)
  (def floors2 {4 {:bay nil :floor []}
                3 {:bay nil :floor [:LG]}
                2 {:bay nil :floor [:HG]}
                1 {:bay :E :floor []}})

  (let [[loc floor] (location floors2)
        neighbours (floor->neighbours floors2 1)]
    (calculate-possible-moves [loc floor] neighbours))


  ;; ** POSSIBLE PATHS
  (def as (let [[loc floor] (location floors)
                neighbours (floor->neighbours floors loc)]
            (possible-paths floors [loc floor] neighbours))))
