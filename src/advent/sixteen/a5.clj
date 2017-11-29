(ns advent.sixteen.a5
  (:require [digest]))


(def base "wtnhxymk")

(defn password [base]
  (->> (range)
       (map (fn [i] (digest/md5 (str base i))))
       (filter (fn [chk] (= "00000"
                            (subs chk 0 5))))
       (take 8)
       (map #(subs % 5 6))
       (apply str)))

(password base) ;; "2414bc77"
