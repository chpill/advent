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

(defn step [acc instruction]
  (let [{:keys [turn block-count]} (parse-instruction instruction)
        new-direction (get (case turn "L" left-of "R" right-of)
                           (:direction acc))

        [normalized-direction movement-sign]
        (case new-direction
          :south [:north -1]
          :west  [:east  -1]
          [new-direction 1])]
    (-> acc
        (assoc :direction new-direction)
        (update normalized-direction + (* movement-sign block-count)))))

(defn total-movement [instruction-list]
  (reduce step
          {:direction :north
           :north 0
           :east  0}
          instruction-list))

(defn distance [{:keys [north east] :as plop}]
  (+ (java.lang.Math/abs north)
     (java.lang.Math/abs east)))

(defn total-distance [instruction-list]
  (distance (total-movement instruction-list)))


(defn accumulate-positions [acc instruction]
  (let [{:keys [turn block-count]} (parse-instruction instruction)
        new-direction (get (case turn "L" left-of "R" right-of)
                           (:direction acc))

        [normalized-direction movement]
        (case new-direction
          :south [:north dec]
          :west  [:east  dec]
          [new-direction inc])]
    (-> acc
        (assoc :direction new-direction)
        (update :positions
                into
                (let [last-position (-> acc :positions last)]
                  (->> (iterate movement
                                (get last-position normalized-direction))
                       (drop 1)
                       (take block-count)
                       (map (fn [v]
                              (assoc last-position
                                     normalized-direction v)))))))))


(defn headquarter-distance [instruction-list]
  (->> (reduce accumulate-positions
                   {:direction :north
                    :positions [{:north 0 :east 0}]}
                   instruction-list)
       :positions
       (reduce (fn [acc location]
                 (if (contains? acc location)
                   (reduced location)
                   (conj acc location)))
               #{})
       distance))
