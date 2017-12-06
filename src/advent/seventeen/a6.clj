(ns advent.seventeen.a6)


(def memory-banks [14 0	15	12	11	11	3	5	1	6	8	4	9	1	8	4])



(defn redistribute [memory-banks]
  (let [max-memory (apply max memory-banks)
        max-index  (.indexOf memory-banks max-memory)
        banks-count (count memory-banks)]

    (loop [memory-banks (assoc memory-banks max-index 0)
           index (-> max-index inc (mod banks-count))
           to-redistribute max-memory]
      (if (zero? to-redistribute)
        memory-banks
        (recur (update memory-banks index inc)
               (-> index inc (mod banks-count))
               (dec to-redistribute))))))


(comment
  (redistribute [0 10 0 10])

  (take 6 (iterate redistribute [0 2 7 0]))

  )


(defn cycle-count [memory-banks]
  (loop [memories #{}
         current-memory-banks memory-banks
         steps 0]
    (if (contains? memories current-memory-banks)
      steps
      (recur (conj memories current-memory-banks)
             (redistribute current-memory-banks)
             (inc steps)))))


(comment
  ;; Should be 5
  (cycle-count [0 2 7 0])

  (cycle-count memory-banks)

  )

;; Part 2

(defn loop-size [memory-banks]
  (let [loop-start
        (loop [memories #{}
               current-memory-banks memory-banks]
          (if (contains? memories current-memory-banks)
            current-memory-banks
            (recur (conj memories current-memory-banks)
                   (redistribute current-memory-banks))))]
    (loop [size 1
          current-memory-banks (redistribute loop-start)]
      (if (= loop-start current-memory-banks)
        size
        (recur (inc size)
               (redistribute current-memory-banks))))))


(comment
  (loop-size [0 2 7 0])

  (loop-size memory-banks)

  )
