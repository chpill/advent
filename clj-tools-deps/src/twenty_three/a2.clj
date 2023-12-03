(ns twenty-three.a2
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.java.io :as io]))

(def test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                 Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                 Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                 Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                 Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(comment (parse-long (re-find #"\d+" "Game 1")))

(defn games-maxima [lines]
  (->> lines
       str/split-lines
       (map (fn [line]
              (let [[raw-game raw-cube-sets] (str/split line #":")
                    game-id (parse-long (re-find #"\d+" raw-game))
                    cube-sets (str/split raw-cube-sets #";")]
                [game-id
                 (into {}
                       (comp (map (fn [cube-set] (str/split cube-set #",")))
                             cat
                             (map #(let [[[_ num color]] (re-seq #"(\d+) (green|red|blue)$" %)]
                                     [(keyword color) (parse-long num)]))
                             (x/by-key x/max))
                       cube-sets)])))))

(def thresholds {:red 12 :green 13 :blue 14})

(defn legal-games-id-sum [lines]
  (->> (games-maxima lines)
      (remove (fn [[id maxima]]
                (some (fn [[k threshold]] (< threshold (get maxima k)))
                      thresholds)))
      (map first)
      (reduce +)))

(comment (legal-games-id-sum test-input))

(def input (slurp (io/resource "twenty_three/a2.txt")))

(legal-games-id-sum input)

;; part 2
(defn power-sum [lines]
  (->> (games-maxima lines)
       (map (fn [[_ maxima]](apply * (vals maxima))))
       (reduce +)))

(comment (power-sum test-input)
         (power-sum input))


