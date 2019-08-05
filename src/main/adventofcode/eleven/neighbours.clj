(ns adventofcode.eleven.neighbours)


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
  (partition-all 2 1 floors))

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
       (apply merge)
       seq))

(defn floor->neighbours [floors floor]

  ;; TODO get floor of cargo
  (->> floors
       group-floors
       collect-neighbours
       (get-neighbours floor)))
