(ns adventofcode.core
  (:require [clojure.spec.alpha :as s]))

(defn check-input [spec-def input]
  (if (= :clojure.spec.alpha/invalid (s/conform spec-def input))

    ;; We can throw an exception, or return data.
    ;; Depends on how you want to organize control flow.
    ;; (s/explain-data spec-def input)
    (throw (Exception. (with-out-str (s/explain spec-def input))))))
