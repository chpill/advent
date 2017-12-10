(ns advent.seventeen.a9
  (:require [clojure.java.io :as io]))


(def input (slurp (io/resource "seventeen/a9.txt")))


(defn filter-garbage
  "Stateful transducer that filters out garbage from a character stream."
  [rf]
  (let [*garbage? (volatile! false)
        *ignore-next-char? (volatile! false)]
    (fn
      ([] (rf))
      ([result] result)
      ([acc c]
       (if @*garbage?
         (if @*ignore-next-char?
           (do (vreset! *ignore-next-char? false) acc)
           (do (case c
                 \> (vreset! *garbage? false)
                 \! (vreset! *ignore-next-char? true)
                 ::no-side-effect)
               acc))
         (if (= \< c)
           (do (vreset! *garbage? true) acc)
           (rf acc c)))))))

(def mostly-garbage ["to<>to"
                     "to<random characters>to"
                     "to<<<<>to"
                     "to<{!>}>to"
                     "to<!!>to"
                     "to<!!!>>to"
                     "to<{o\"i!a,<{i<a>to"])

(comment
  (apply =
         [\t \o \t \o]
         (for [chars mostly-garbage]
           (into [] filter-garbage chars)))

  (into [] filter-garbage "{{<!>},{<!>},{<!>},{<a>}}"))


(reduce (fn [{:keys [depth score] :as acc} c]
          (case c
            \{ (let [new-depth (inc depth)]
                 {:depth new-depth
                  :score (+ score new-depth)})
            \} (update acc :depth dec)))
        {:depth 0 :score 0}
        (into []
              (comp filter-garbage
                    (remove #{\, \newline}))
              input))


;; Part 2

(defn filter-non-garbage
  "Stateful transducer that filters out garbage from a character stream."
  [rf]
  (let [*garbage? (volatile! false)
        *ignore-next-char? (volatile! false)]
    (fn
      ([] (rf))
      ([result] result)
      ([acc c]
       (if @*garbage?
         (if @*ignore-next-char?
           (do (vreset! *ignore-next-char? false) acc)
           (case c
             \> (do (vreset! *garbage? false) acc)
             \! (do (vreset! *ignore-next-char? true) acc)
             (rf acc c)))
         (if (= \< c)
           (do (vreset! *garbage? true) acc)
           acc))))))


(comment
  (for [chars mostly-garbage]
    (vector chars
            (count (into [] filter-non-garbage chars))))
  )


(count (into [] filter-non-garbage input))
