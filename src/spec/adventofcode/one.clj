(ns adventofcode.one
  (:require [clojure.spec.alpha :as s]))


(s/def :one/direction #(re-matches #"[RL][0-9]+" %))
(s/def :one/directions (s/coll-of :one/direction))

(s/def :one/left-or-right (s/or :left #(re-matches #"L")
                                :right #(re-matches #"R")))

(s/def :one/split-direction-result (s/cat :one/left-or-right integer?))

(s/def :one/split-direction-results (s/coll-of :one/split-direction-result))


(s/fdef split-direction
        :args :one/direction
        :ret :one/split-direction-result)

(s/fdef split-directions
        :args :one/directions
        :ret (s/coll-of :one/split-direction-result))

(s/fdef split-with-delimiter
        :args (s/cat :string string? :delimiter string?)
        :ret (s/cat :direction string? :distance string?))
