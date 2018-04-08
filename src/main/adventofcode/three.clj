(ns adventofcode.three
  (:require [clojure.core.match :refer [match]]))

;; number-sequence-to-grid
;;
;; can only turn left
;; what's the edge of the grid: current dimensions + 1


(defn bar [direction {width :width height :height x :x y :y} number]

  (println width " / " x)

  (match [direction width height x y]

         [_ (_ :guard #(= % x)) _ _ _]
         {:height height
         :width (inc width)
         :x (inc x)
         :y y}

         [_ (_ :guard #(= 0 x)) _ _ _]
         {:height height
         :width (inc width)
         :x x
         :y y}

         [_ _ _ (_ :guard #(= % y)) _]
         {:height (inc y)
         :width width
         :x x
         :y (inc y)}

         [_ _ _ (_ :guard #(= 0 y)) _]
         {:height (inc y)
         :width width
         :x x
         :y y}

         [:down _ _ _ _]
         {:height height
         :width width
         :x x
         :y (dec y)}))

(comment

  (def number-sequence (rest (range)))
  (def rotation-sequence (cycle [:down :right :up :left]))
  (def grid {:height 1
             :width 1
    :x 1
    :y 1})

  (bar :down {:width 10 :height 10 :x 10 :y 10} 22)

  )
