(ns advent.seventeen.a3)


(def input 277678)

;; 0 :east
;; 1 :north
;; 2 :west
;; 3 :south

(defn turn [x]
  (mod (inc x) 4))

(defn move [starting-point direction]
  (mapv +
        starting-point
        (get [[1 0] [0 1] [-1 0] [0 -1]] direction)))

(defn step
  ([] {:increment? false
       :step-before-turn 0
       :step-count 0
       :direction 0
       :position [0 0]})
  ([{:keys [increment? step-before-turn step-count direction position] :as data}]
   (let [new-position (move position direction)]
     (if (zero? step-before-turn)
       (let [new-step-count (cond-> step-count
                              increment? inc)]
         {:increment? (not increment?)
          :direction (turn direction)
          :step-before-turn new-step-count
          :step-count new-step-count
          :position new-position})
       (-> data
           (update :step-before-turn dec)
           (assoc :position new-position))))))

(defn abs [x] (Math/abs x))


;; 475
(->> (loop [x (dec input)
            data (step)]
       (if (zero? x)
         data
         (recur (dec x)
                (step data))))
     :position
     (map abs)
     (apply +))


;; Part 2

(defn neighbours-of [x y]
  (for [dx [-1 0 1] dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn adjacent-sum [positions [x y]]
  (let [neighbours (set (neighbours-of x y))]
    (->> positions
         (filter #(contains? neighbours
                             (:position %)))
         (map :value)
         (apply +))))

(defn step-2
  ([] {:increment? false
       :step-before-turn 0
       :step-count 0
       :direction 0
       :positions [{:position [0 0]
                    :value 1}]})
  ([{:keys [increment? step-before-turn step-count direction positions] :as data}]
   (let [new-position (move (-> positions last :position) direction)
         updated-positions (conj positions
                                 {:position new-position
                                  :value (adjacent-sum positions new-position)})]
     (if (zero? step-before-turn)
       (let [new-step-count (cond-> step-count
                              increment? inc)]
         {:increment? (not increment?)
          :direction (turn direction)
          :step-before-turn new-step-count
          :step-count new-step-count
          :positions updated-positions})
       (-> data
           (update :step-before-turn dec)
           (assoc :positions updated-positions))))))


;; 279138
(loop [data (step-2)]
  (let [memory-value (-> data :positions last :value)]
    (if (< input memory-value)
      memory-value
      (recur (step-2 data)))))
