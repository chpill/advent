(ns advent.eighteen.a2
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]
            [net.cgrand.xforms :as x]))



(def input (->> (java-io/resource "eighteen/a2.txt")
                java-io/file
                xforms-io/lines-in
                ))

;; TODO, do it in 2 times?
(->> input
     (reduce (fn [acc checksum]
               (let [freqs (frequencies checksum)]
                 (cond-> acc
                   (some (fn [[k v]] (= 2 v)) freqs)
                   (update :doublets inc)
                   (some (fn [[k v]] (= 3 v)) freqs)
                   (update :triplets inc))))
             {:doublets 0
              :triplets 0})
     vals
     (apply *))

;; => 6642


;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;

;; All input are of 26 characters
(comment (apply = (map count (into [] input))))

(defn one-difference? [i1 i2]
  (= 1
     (->> (map = i2 i1)
          (remove true?)
          count)))

(comment
  (apply one-difference? (take 2 (into [] input)))

  (one-difference? "ab" "aa")
  (one-difference? "aab" "aaa")

  )



(let [checksums (into [] input)
      [c1 c2] (first (for [x checksums
                           y checksums
                           :when (one-difference? x y)]
                       [x y]))
      c1-charset (set c1)]
  (apply str (filter c1-charset c2)))


;; => "cvqlbidheyujgtrswxmckqnap"
