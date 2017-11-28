(ns advent.sixteen.a4
  (:require [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rf]))


(def reg #"([a-z-]+)-(\d+)\[([a-z]{5})\]")


(defn parse [encrypted-room]
  (let [[_ room-name id checksum] (re-find reg encrypted-room)]
    {:room-letters (clojure.string/replace room-name "-" "")
     :id (Integer. id)
     :checksum checksum}))


(defn by-desc-freq-then-inverse-lexic-comparator
  "Compare key value pairs such as those returned by the `frequency` function."
  [a b]
  (let [by-frequency (compare (second b)
                              (second a))]
    (if (zero? by-frequency)
      (compare (first a)
               (first b))
      by-frequency)))


(defn checksum-from-letters [room-letters]
  (transduce (comp (x/by-key identity x/count)
                   (x/into (sorted-set-by by-desc-freq-then-inverse-lexic-comparator))
                   cat
                   (take 5)
                   (map first))
             rf/str
             room-letters))


(defn real-room? [{:keys [room-letters checksum]}]
  (= checksum (checksum-from-letters room-letters)))


(defn sum-real-room-ids [room-list]
  (transduce (comp (map parse)
                   (filter real-room?)
                   (map :id))
             +
             room-list))
