(ns advent.seventeen.a1
  (:require [net.cgrand.xforms :as x]))


(defn parse [input-string]
  (map #(Integer. (str %)) input-string))

(defn captcha [digits]
  (->> (partition 2 1 [(first digits)] digits)
       (filter #(apply = %))
       (map first)
       (reduce +)))


(defn captcha2 [input-string]
  (let [digits (map #(Character/digit % 10) input-string)
        length (count digits)
       [beginning end] (split-at (/ length 2) digits)]
   (->> (map vector beginning end)
        (filter #(apply = %))
        (map first)
        (reduce +)
        (* 2))))


;; Bonus, using xform/partition

(defn captcha--xform [digits]
  (transduce (comp (x/partition 2 1 [(first digits)])
                   (filter #(apply = %))
                   (map first))
             +
             digits))


