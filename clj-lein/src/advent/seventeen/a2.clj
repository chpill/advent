(ns advent.seventeen.a2)


(defn line-diff [line]
  (- (apply max line)
     (apply min line)))

(defn checksum [spreadsheet]
  (->> (map line-diff spreadsheet)
       (apply +)))


;; Part 2

(defn evenly-divisible? [x y]
  (= x
     (* y
        (int (/ x y)))))

(defn extract-evenly-divisible [line]
  (for [x line
        y line
        :when (and (not= x y)
                   (evenly-divisible? x y))]
    (/ x y)))

(defn checksum2 [spreadsheet]
  (->> (mapcat extract-evenly-divisible spreadsheet)
       (apply +)))
