(ns twenty-three.a1
  (:require [clojure.string :as str]
            [criterium.core :as crit]))


(defmacro spy [& body]
  `(do ~@(map (fn [local-name] `(def ~local-name ~local-name))
              (keys &env))
       ~@body))





(def input (slurp (io/resource "twenty_three/a1.txt")))


(def test-input "1abc2
                 pqr3stu8vwx
                 a1b2c3d4e5f
                 treb7uchet")

(defn cal [f s]
  (->> s
       str/split-lines
       (map #(->> (f %)
                  ((juxt first last))
                  (apply str)
                  parse-long))
       (reduce +)))

(def cal1 (partial cal #(filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} %)))

(comment (cal1 test-input) := 142
         (cal1 input) := 55607)


(def test-input2 "two1nine
                  eightwothree
                  abcone2threexyz
                  xtwone3four
                  4nineeightseven2
                  zoneight234
                  7pqrstsixteen")

(defn sorted-map-by-value [m]
  (into (sorted-map-by
         (fn [a b]
           (compare (get m a) (get m b))))
        m))

(def alpha->num {"one" 1 "two" 2 "three" 3 "four" 4
                 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(def nums->num (let [ks (sort (vals alpha->num))]
                 (zipmap (map str ks) ks)))


(def regex (re-pattern
            (str "(?=("
                 (str/join "|"
                           (concat (keys (sorted-map-by-value alpha->num))
                                   (keys (sorted-map-by-value nums->num))))
                 ").*)")))


(re-seq (re-pattern "(?=(\\d+\\D+\\d+)).")
         "1abc2abc3abc4abc5")

(re-seq (re-pattern "\\d+\\D+\\d+")
        "1abc2abc3abc4abc5")

(re-seq #"(?=(one|two|three).*)" "twone")

(re-seq #"(?=(one|two).)" "twone")
(re-seq #"(?=(one)|(two)|(three).)" "twone")
(re-seq #"(?=(one)|(two)|(three).*)" "twonethree")


(re-seq #"(?=(^(one|two){1}$))." "two")


(re-seq #"one|two" "twone")
(re-seq #"(?=(one)|(two))." "twone")


(def cal2 (partial cal (fn [s]
                         (->> (re-seq regex s)
                              (keep (let [m (merge alpha->num nums->num)]
                                      #(some m %)))))))

(def regex2 (re-pattern
             (str "(?=("
                  (str/join "|"
                            (concat (keys (sorted-map-by-value alpha->num))
                                    (keys (sorted-map-by-value nums->num))))
                  ")).?")))

(def cal2-2 (partial cal (fn [s]
                         (->> (re-seq regex2 s)
                              (keep (let [m (merge alpha->num nums->num)]
                                      #(some m %)))))))
(cal2 "xtwone3four")
(->> ["twone" "fiveight" "oneight" "sevenine" "eightwo" "eighthree" "nineight"]
     (map #(re-seq regex %)))

(->> ["twoneight"]
     (map #(re-seq regex %)))

["twone" "oneight" "fiveight" "nineight" "sevenine" "eightwo" "eighthree"]

(cal2 "twoneight")
(cal2 "nineight")
(cal2 "nineeight")

(cal2 "jcb82eightwond")

(def cal2-naive (partial cal  #(->> %
                                    (re-seq (re-pattern (str/join "|" (concat (keys alpha->num)
                                                                              (keys nums->num)))))
                                    (map (merge alpha->num nums->num)))))

(let [reg (re-pattern (str/join "|" ["twone" "oneight" "fiveight" "nineight" "sevenine" "eightwo" "eighthree"]))]
  (->> input
      str/split-lines
      (filter #(re-find reg %))
      (map (fn [s]
             [s
              (->> (re-seq regex s)
                   (keep (let [m (merge alpha->num nums->num)]
                           #(some m %))))
              (cal2 s)
              (cal2-naive s)]))))

(cal2-naive input)
(cal2 input)

(comment
  (->> test-input2
       str/split-lines
       (map #(->> %
                  (re-seq (re-pattern (str/join "|" (concat (keys alpha->num)
                                                            (keys nums->num)))))
                  (map (merge alpha->num nums->num))))
       #_(map (juxt first last))))

;; 55309 is too high!

(comment (cal2 test-input2)
         := 281
         (cal2 test-input)
         := 142
         (cal2 input)
         := 55309
         (cal2-2 input)


         (crit/quick-bench
          (cal2 input))

         (crit/quick-bench
          (cal2-2 input))

         )



(let [reg (re-pattern
           (str "(?=("
                (str/join "|"
                          (concat (keys (sorted-map-by-value alpha->num))
                                  (keys (sorted-map-by-value nums->num))))
                ")).?"))]
  (re-seq reg "plop twone"))



(let  [m (merge alpha->num nums->num)
       regex-a (re-pattern
                (str "(?=("
                     (str/join "|"
                               (concat (keys (sorted-map-by-value alpha->num))
                                       (keys (sorted-map-by-value nums->num))))
                     "))."))
       regex-b (re-pattern
                (str "(?=("
                     (str/join "|"
                               (concat (keys (sorted-map-by-value alpha->num))
                                       (keys (sorted-map-by-value nums->num))))
                     ")).?"))]
  (->> input
       str/split-lines
       (keep #(let [a (->> (re-seq regex-a %)
                           (keep (fn [x] (some m x))))
                    b (->> (re-seq regex-b %)
                           (keep (fn [x] (some m x))))]
                (when-not (or (= a b) (= (drop-last a) b))
                  [%  a b])))
       #_(reduce +)))
