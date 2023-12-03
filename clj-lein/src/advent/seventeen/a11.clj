(ns advent.seventeen.a11
  (:require [clojure.java.io :as io]
            [cuerdas.core :as str])
  (:import java.lang.Math))

(def input (-> (io/resource "seventeen/a11.txt") slurp str/lines first))

(defn dir->coord [directions]
  (->> (str/split directions ",")
       (map #(case %
               "n"  [1 0]
               "ne" [0.5  1]
               "se" [-0.5 1]
               "s"  [-1 0]
               "sw" [-0.5 -1]
               "nw" [0.5  -1]))))


(defn distance [[x y]]
  (let [lateral-steps (Math/abs y)
        vertical-steps (Math/abs (- (Math/abs x) (/ lateral-steps 2)))]
    (+ lateral-steps vertical-steps)))


(defn distance-from-directions [directions]
  (->> (dir->coord directions)
      (reduce (partial mapv +) [0 0])
      distance))


(distance-from-directions input)

(distance-from-directions "ne,ne,ne") ;; 3 steps away.
(distance-from-directions "ne,ne,sw,sw") ;; 0 steps away (back where you started).
(distance-from-directions "ne,ne,s,s") ;; 2 steps away (se,se).
(distance-from-directions "se,sw,se,sw,sw") ;; 3 steps away (s,s,sw).


;; Part 2

(->> (dir->coord input)
     (reductions (partial mapv +) [0 0])
     (map distance)
     (apply max))


