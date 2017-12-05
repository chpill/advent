(ns advent.seventeen.a5
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]))


(def reducible-input (-> (java-io/resource "seventeen/a5.txt")
                         java-io/file
                         xforms-io/lines-in))
(def instructions (into []
              (map #(Integer. %))
              reducible-input))


(defn escape [instructions]
  (let [instructions-length (count instructions)]
    (loop [step         0
           index        0
           instructions instructions]
      (if-not (<= 0 index (dec instructions-length))
        step
        (let [next-index (+ index (get instructions index))]
          (recur (inc step)
                 next-index
                 (update instructions index inc)))))))



(comment
  (escape [0 3 0 1 -3])

  (escape instructions))


(defn escape-2 [instructions]
  (let [instructions-length (count instructions)]
    (loop [step         0
           index        0
           instructions instructions]
      (if-not (<= 0 index (dec instructions-length))
        step
        (let [offset (get instructions index)
              next-index (+ index offset)]
          (recur (inc step)
                 next-index
                 (update instructions index (if (< 2 offset)
                                              dec
                                              inc))))))))


(comment
  (escape-2 [0 3 0 1 -3])

  (escape-2 instructions))
