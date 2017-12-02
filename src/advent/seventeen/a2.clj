(ns advent.seventeen.a2)


(defn line-diff [line]
  (- (apply max line)
     (apply min line)))

(defn checksum [spreadsheet]
  (->> (map line-diff spreadsheet)
       (apply +)))
