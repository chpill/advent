(ns advent.sixteen.a4-test
  (:require [advent.sixteen.a4 :as sut]
            [clojure.test :as t]
            [net.cgrand.xforms.io :as xforms-io]
            [clojure.java.io :as java-io]
            [criterium.core :as criterium]))


(t/deftest grouping-regex
  (let [{:keys [room-letters id checksum]} (sut/parse "aaaaa-bbb-z-y-x-123[abxyz]")]
    (t/is (= "aaaaabbbzyx" room-letters))
    (t/is (= 123 id))
    (t/is (= "abxyz" checksum))))


(t/deftest custom-comparator
  (t/is (= -1 (sut/by-desc-freq-then-inverse-lexic-comparator [\a 5] [\b 4])))
  (t/is (= 1 (sut/by-desc-freq-then-inverse-lexic-comparator [\a 5] [\b 6])))
  (t/is (= 1 (sut/by-desc-freq-then-inverse-lexic-comparator [\b 1] [\a 1])))
  (t/is (= -1 (sut/by-desc-freq-then-inverse-lexic-comparator [\a 1] [\b 1]))))

(t/deftest checksums
  (t/are [encrypted-room expected-checksum]
      (= (-> encrypted-room sut/parse :room-letters sut/checksum-from-letters)
         (-> encrypted-room sut/parse :room-letters sut/checksum-from-letters--xform-version)
         expected-checksum)
    "aaaaa-bbb-z-y-x-123[abxyz]" "abxyz"
    "a-b-c-d-e-f-g-h-987[abcde]" "abcde"))


(t/deftest real-rooms-test
  (t/is (sut/real-room? (sut/parse "aaaaa-bbb-z-y-x-123[abxyz]")))
  (t/is (sut/real-room? (sut/parse "a-b-c-d-e-f-g-h-987[abcde]")))
  (t/is (sut/real-room? (sut/parse "not-a-real-room-404[oarel]")))
  (t/is (not (sut/real-room? (sut/parse "totally-real-room-200[decoy]")))))

(t/deftest example-sum
  (t/is (= 6 (sut/sum-real-room-ids ["aaaaa-bbb-z-y-x-1[abxyz]"
                                     "a-b-c-d-e-f-g-h-2[abcde]"
                                     "not-a-real-room-3[oarel]"]))))

(def reducible-input (-> (java-io/resource "sixteen/a4-input.txt")
                         java-io/file
                         xforms-io/lines-in))

;; (count (into [] input)) => 1066
;; (remove #(re-find sut/reg %) (into [] input)) => ()


;; TODO benchmark
(sut/sum-real-room-ids reducible-input)
(sut/sum-real-room-ids--xform reducible-input)

