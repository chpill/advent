(ns advent.eighteen.a1
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]
            [net.cgrand.xforms :as x]))



(def input (->> (java-io/resource "eighteen/a1.txt")
                java-io/file
                xforms-io/lines-in
                (into [] (map #(Integer. %)))))

(reduce + input)
;; => 576



;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;


(loop [seen-frequencies #{}
       current-frequency 0
       input (cycle input)]
  (if (get seen-frequencies current-frequency)
    current-frequency
    (recur (conj seen-frequencies current-frequency)
           (+ current-frequency (first input))
           (rest input))))

;; 77674

