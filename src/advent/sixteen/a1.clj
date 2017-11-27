(ns advent.sixteen.a1)


(def left-of {:north :west
              :west  :south
              :south :east
              :east  :north})


(def right-of (into {}
                    (map (fn [[k v]] [v k]))
                    left-of))


(defn parse-instruction [instruction-sym]
  (let [instruction-str (name instruction-sym)]
    {:turn        (subs instruction-str 0 1)
     :block-count (Integer. (subs instruction-str 1))}))


(defn movements [instruction-list]
  (reduce (fn [acc instruction]
            (let [{:keys [turn block-count]} (parse-instruction instruction)
                  new-direction (get (case turn "L" left-of "R" right-of)
                                     (:direction acc))]
              (-> acc
                  (assoc :direction new-direction)
                  (update new-direction + block-count))))
          {:direction :north
           :north 0
           :east  0
           :south 0
           :west  0}
          instruction-list))

(defn distance [instruction-list]
  (let [{:keys [north east south west]} (movements instruction-list)]
    (+ (java.lang.Math/abs (- north south))
       (java.lang.Math/abs (- east west)))))
