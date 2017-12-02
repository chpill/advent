(ns advent.seventeen.a2-test
  (:require [advent.seventeen.a2 :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (with-open [reader (io/reader (io/resource "seventeen/a2.txt"))]
             (->> (line-seq reader)
                  (map #(str/split % #"\t"))
                  (map (fn [char] (map #(Integer. %) char)))
                  (mapv vec))))


(t/deftest example
  (t/is (= 18 (sut/checksum [[5 1 9 5]
                             [7 5 3]
                             [2 4 6 8]]))))

(sut/checksum input)


(t/deftest example-2
  (t/is (= 9 (sut/checksum2 [[5 9 2 8]
                             [9 4 7 3]
                             [3 8 6 5]]))))
