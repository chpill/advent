(ns advent.seventeen.a7
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]
            [cuerdas.core :as str]
            [clojure.set :as set]
            [net.cgrand.xforms :as x]))


(def reducible-input (-> (java-io/resource "seventeen/a7.txt")
                         java-io/file
                         xforms-io/lines-in))

(def program-re #"([a-z]+) \((\d+)\)")
(def children-re #" -> (.+)")

(defn parse-program [line]
  (let [[_ name weight] (->> line (re-seq program-re) first)
        children (-> (re-seq children-re line) first second
                     (str/split ", "))]
    #:program{:name name
              :weight (Integer. weight)
              :children children}))


(comment (parse-program "fwft (72) -> ktlj, cntj, xhth")
         #:program{:name "fwft", :weight "72", :children ["ktlj" " cntj" " xhth"]})

(def programs (into []
                    (map parse-program)
                    reducible-input))


(defn root-program-name [programs]
  (let [all-children-names (into #{}
                                (mapcat :program/children)
                                programs)
       program-names (into #{}
                           (map :program/name)
                           programs)]
   (first (set/difference program-names all-children-names))))

(def root-name (root-program-name programs))


;; Part 2

(def name->program
  (into {} (x/by-key :program/name identity) programs))

(def root-program (name->program root-name))

(defn total-weight [root-program]
  (+ (:program/weight root-program)
     (transduce (comp (map name->program)
                      (map total-weight))
                +
                (:program/children root-program))))

(defn find-corrupt-program [root-program expected-weight]
  (let [child->weight (into {}
                           (map (juxt identity (comp total-weight name->program)))
                           (:program/children root-program))
       weights-freq (frequencies (map second child->weight))]
    (if-let [wrong-weight (some (fn [[k v]] (when (= 1 v) k))
                                           weights-freq)]
      (let [expected-weight (-> weights-freq (dissoc wrong-weight) ffirst)]

        (recur (name->program ((set/map-invert child->weight) wrong-weight))
               expected-weight))
      (- expected-weight
         (transduce (comp (map name->program)
                          (map total-weight))
                    +
                    (:program/children root-program))))))

(find-corrupt-program root-program :plop)
