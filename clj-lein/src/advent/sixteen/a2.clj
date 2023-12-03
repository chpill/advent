(ns advent.sixteen.a2)


(def forbidden-horizontal-moves
  #{#{3 4} #{6 7}})

(defn follow [position direction]
  (let [candidate-position (+ position (case direction
                                         \U -3 \D 3
                                         \L -1 \R 1))]
    (if (and (< 0 candidate-position 10)
             (not (contains? forbidden-horizontal-moves
                             #{candidate-position position})))
      candidate-position
      position)))


(defn digit [{:keys [position toilet-code]} instruction-line]
  (let [final-position (reduce follow
                               position
                               (name instruction-line))]
    {:position final-position
     :toilet-code (conj toilet-code final-position)}))

(defn find-toilet-code [instruction-lines]
  (:toilet-code (reduce digit
                        {:position 5 :toilet-code []}
                        instruction-lines)))
