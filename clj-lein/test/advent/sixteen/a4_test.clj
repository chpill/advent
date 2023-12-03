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

;; Sanity check on the input
(comment

  (count (into [] reducible-input)) ;; 1066

  (->> (into [] reducible-input)
       (remove #(re-find sut/reg %))) ;; ()

  )


;; Benchmarks with or without transducers for the checksum function
;; It seems that tranducers are slower here... not sure why
(comment
  (criterium/quick-bench
   (sut/sum-real-room-ids reducible-input))
  "Evaluation count : 24 in 6 samples of 4 calls.
   Execution time mean : 28.600073 ms
   Execution time std-deviation : 364.292962 µs
   Execution time lower quantile : 28.309775 ms ( 2.5%)
   Execution time upper quantile : 29.188787 ms (97.5%)
   Overhead used : 6.115135 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 13.8889 % Variance is moderately inflated by outliers "

  (criterium/bench
   (sut/sum-real-room-ids reducible-input))
  "
Evaluation count : 2160 in 60 samples of 36 calls.
Execution time mean : 28.312274 ms
Execution time std-deviation : 226.258612 µs
Execution time lower quantile : 27.964026 ms ( 2.5%)
Execution time upper quantile : 28.744894 ms (97.5%)
Overhead used : 6.115135 ns

Found 1 outliers in 60 samples (1.6667 %)
	low-severe	 1 (1.6667 %)
Variance from outliers : 1.6389 % Variance is slightly inflated by outliers "


  (criterium/benchmark
   (sut/sum-real-room-ids--xform reducible-input))

  "
  Evaluation count : 18 in 6 samples of 3 calls.
  Execution time mean : 46.734310 ms
  Execution time std-deviation : 422.224982 µs
  Execution time lower quantile : 46.364802 ms ( 2.5%)
  Execution time upper quantile : 47.332650 ms (97.5%)
  Overhead used : 6.115135 ns "

  (criterium/bench
   (sut/sum-real-room-ids--xform reducible-input))
  "
Evaluation count : 1320 in 60 samples of 22 calls.
Execution time mean : 46.907792 ms
Execution time std-deviation : 645.295787 µs
Execution time lower quantile : 46.229614 ms ( 2.5%)
Execution time upper quantile : 48.595353 ms (97.5%)
Overhead used : 6.115135 ns

Found 3 outliers in 60 samples (5.0000 %)
	low-severe	 3 (5.0000 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
"


  )

