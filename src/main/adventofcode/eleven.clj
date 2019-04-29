(ns main.adventofcode.eleven)


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


;; TODO - implement using Specter

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
(def cargo-set (->> compatible-cargo
                    seq
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


(defn find-neighbour [ky1 input]
  (filter (fn [[[k1 v1] [k2 v2]]]
            (= k2 ky1))
          input))

(defn collect-neighbours [grouped-floors]
  (->> grouped-floors
       (map (fn [[[ky1 vl1] [ky2 vl2]]]

              ;; removes itself
              (let [input (remove (fn [[[k1 _] _]] (= ky1 k1)) grouped-floors)
                    [[[rk rv] _]] (find-neighbour ky1 input)]

                (->> (concat [ky1 vl1] [ky2 vl2] [rk rv])
                     (remove (fn [v] (nil? v)))))))
       (map (fn [v]
              (->> v
                   (partition 2)
                   (sort-by first)
                   reverse
                   (apply concat))))))

(defn group-floors [floors]
  (partition-all 2 1 (map identity floors)))

(defn get-neighbours [ky neighbour-collection]
  (->> neighbour-collection
       (map (fn [v] (partition 2 v)))
       (filter (fn [il]
                 (-> (filter (fn [[k v]] (= ky k)) il)
                     empty?
                     not)))
       (map (fn [v] (partition 2 1 v)))
       (map (fn [v] (filter (fn [[[lk lv] [rk rv]]]
                             (or (= ky lk) (= rk ky)))
                           v)))
       (map flatten)
       (map (fn [v] (partition 2 v)))
       (map (fn [v] (remove (fn [[k' v']] (= ky k')) v)))
       (map flatten)
       (map (fn [v] (apply hash-map v)))
       (apply merge)))

(defn floor->neighbours [floors cargo]

  ;; TODO get floor of cargo
  (let [floor cargo]

    (->> floors
         (group-floors)
         collect-neighbours
         (get-neighbours floor))))

(defn possible-moves [floors cargo]
  (floor->neighbours floors cargo))


(comment


  (pprint (floor->neighbours floors 4))
  (pprint (floor->neighbours floors 3))
  (pprint (floor->neighbours floors 2))
  (pprint (floor->neighbours floors 1))


  (move floors 3 1 :LG)


  ;; ? What combination of items can I take
  ;; ? For each combination, where can I go
  

  #_(defn one [floors start-floor cargo]

    (for [[n fl] (seq (possible-moves floors 1))]

      (next-step floors start-floor n cargo)
      [n fl])

    (let [[n fl] (seq (possible-moves floors start-floor))]

      (map )))
  #_(pprint (take 6 (iterate (fn [[fl cargo from to]]

                              (let [nextf (next-step fl cargo from to)]
                                [nextf cargo to (inc to)]))

                            [floors :LG 3 1]))))
