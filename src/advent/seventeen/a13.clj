(ns advent.seventeen.a13
  (:require [clojure.java.io :as io]
            [cuerdas.core :as str]))


(def raw-input (slurp (io/resource "seventeen/a13.txt")))

(defn refine [raw-input]
  (->> (str/lines raw-input)
       (map #(str/split % ": "))
       (map (partial map #(Integer. %)))
       (map (partial zipmap [:depth :scan-range]))))

(def input (refine raw-input))

(defn scan-period [scan-range]
  (* 2 (dec scan-range)))

(defn caught? [{:keys [depth scan-range]}]
  (zero? (mod depth (scan-period scan-range))))

(defn severity [{:keys [depth scan-range]}]
  (* depth scan-range))

(def raw-example
  "0: 3
  1: 2
  4: 4
  6: 4")

(filter caught? (refine raw-example))

(->> input
     ;; (refine raw-example)
     (filter caught?)
     (map severity)
     (apply +))


;; Part 2

(loop [delay 0]
  (if (->> input
           ;; (refine raw-example)
           (map #(update % :depth + delay))
           (some caught?))
    (recur (inc delay))
    delay))
