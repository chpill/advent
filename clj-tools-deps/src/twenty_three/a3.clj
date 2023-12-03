(ns twenty-three.a3
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.java.io :as io]))



(def test-input "467..114..
                 ...*......
                 ..35..633.
                 ......#...
                 617*......
                 .....+.58.
                 ..592.....
                 ......755.
                 ...$.*....
                 .664.598..")

(defn neighbors [[x y]]
  (-> (for [x' (range (dec x) (+ x 2))
            y' (range (dec y) (+ y 2))]
        [x' y'])
      set
      (disj [x y])))

(comment (neighbors [5 5]))

(defn all-coords [s]
  (->> s
      str/split-lines
      (into []
            (comp (map str/trim)
                  (map-indexed (fn [y line]
                                 (map-indexed
                                  (fn [x v]
                                    (when-let [value (case v
                                                       \. nil
                                                       (\1 \2 \3 \4 \5 \6 \7 \8 \9 \0)
                                                       (Character/digit v 10)
                                                       v)]
                                      {:coords [y x]
                                       :value value}))
                                  line)))
                  cat
                  (remove nil?)))))


;; Tweaked clojure.core/partition-by
;; Maybe use try using x/by-key to do something equivalent?
(defn partition-by2
  ([fval fpred]
  (fn [rf]
    (let [a (java.util.ArrayList.)
          pv (volatile! ::none)]
      (fn
        ([] (rf))
        ([result]
           (let [result (if (.isEmpty a)
                          result
                          (let [v (vec (.toArray a))]
                            ;;clear first!
                            (.clear a)
                            (unreduced (rf result v))))]
             (rf result)))
        ([result input]
           (let [pval @pv
                 val (fval input)]
             (vreset! pv val)
             (if (or (identical? pval ::none)
                     (fpred pval val))
               (do
                 (.add a input)
                 result)
               (let [v (vec (.toArray a))]
                 (.clear a)
                 (let [ret (rf result v)]
                   (when-not (reduced? ret)
                     (.add a input))
                   ret))))))))))


(defn sum-of-part-numbers [s]
  (let [{symbols false digits true} (group-by (comp int? :value) (all-coords s))
       digits-by-coords (into {} (x/by-key :coords identity) digits)
       adjacent-digits-coords (->> symbols
                                   (mapcat (fn [{:keys [coords]}]
                                             (keep #(:coords (get digits-by-coords %))
                                                   (neighbors coords))))
                                   set)
       numbers
       (->> digits
            (sort-by :coords)
            (into []
                  (comp (partition-by2 :coords
                                       (fn [[y x] [y' x']]
                                         (and (= y y)
                                              (= x (dec x')))))
                        (map (fn [xs]
                               {:digits-coords (into #{} (map :coords) xs)
                                :number (parse-long (x/str (map :value) xs))})))))]
   (->> numbers
        (keep (fn [num]
                (when (some #(contains? adjacent-digits-coords %)
                            (:digits-coords num))
                  num)))
        (map :number)
        (reduce +))))

(comment (sum-of-part-numbers test-input)
         (sum-of-part-numbers (slurp (io/resource "twenty_three/a3.txt"))))


;; Part 2



(defn sum-of-gear-ratios [s]
  (let [{symbols false digits true} (group-by (comp int? :value) (all-coords s))
      gears (filter #(= \* (:value %)) symbols)

      numbers
      (->> digits
           (sort-by :coords)
           (into []
                 (comp (partition-by2 :coords
                                      (fn [[y x] [y' x']]
                                        (and (= y y)
                                             (= x (dec x')))))
                       (map (fn [xs]
                              {:digits-coords (into #{} (map :coords) xs)
                               :number (parse-long (x/str (map :value) xs))})))))

      number-by-coords (into (sorted-map)
                             (mapcat (fn [{:as num :keys [digits-coords]}]
                                       (map (fn [coords] [coords num]) digits-coords)))
                             numbers)]

  (->> gears
       (keep (fn [{:keys [coords]}]
               (let [adjacent-numbers (set (keep #(get number-by-coords %)
                                                 (neighbors coords)))]
                 (when (= 2 (count adjacent-numbers))
                   (apply * (map :number adjacent-numbers))))))
       (reduce +))))

(comment (sum-of-gear-ratios test-input)
         (sum-of-gear-ratios (slurp (io/resource "twenty_three/a3.txt"))))
