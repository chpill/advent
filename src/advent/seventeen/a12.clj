(ns advent.seventeen.a12
  (:require [clojure.java.io :as java-io]
            [cuerdas.core :as str]
            [net.cgrand.xforms.io :as xforms-io]
            [clojure.set :as set]))


(def reducible-input (-> (java-io/resource "seventeen/a12.txt")
                         java-io/file
                         xforms-io/lines-in))


(def pipe-re #"(\d+) <-> (.*)")


(defn parse-pipe [line]
  (let [[_ program-id programs] (->> line (re-seq pipe-re) first)
        pipes-to (str/split programs ", ")]
    #:program{:id (Integer. program-id)
              :pipes-to (map #(Integer. %) pipes-to)}))


(defn connected-set [{:program/keys [id pipes-to]}]
  (apply hash-set id pipes-to))


(defn extract-connections [lines]
  (into #{}
        (comp (map parse-pipe)
              (map connected-set))
        lines))


(def example (str/lines "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"))

;; not 1959
(defn connected-to [program-id connections]
  (loop [connected-to-target #{program-id}
         connections connections]
   (if-let [new-connections-to-zero (not-empty (filter #(not-empty (set/intersection connected-to-target %))
                                                       connections))]
     (recur (into connected-to-target cat new-connections-to-zero)
            (reduce disj connections new-connections-to-zero))
     connected-to-target)))

(count (connected-to 0 (extract-connections reducible-input)))

;; Part 2

(defn all-groups [connections]
  (let [all-programs (into #{} cat connections)]
    (loop [remaining-programs all-programs
           groups             #{}]
      (if-let [target (first remaining-programs)]
        (let [connected-to-target (connected-to target connections)]
          (recur (reduce disj remaining-programs connected-to-target)
                 (conj groups connected-to-target)))
        groups))))


(count (all-groups (extract-connections reducible-input)))
