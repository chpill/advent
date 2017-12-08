(ns advent.seventeen.a8
  (:require [clojure.java.io :as io]
            [net.cgrand.xforms.io :as x-io]
            [net.cgrand.xforms :as x]
            [clojure.string :as str]))


(def reducible-input (-> (io/resource "seventeen/a8.txt")
                         io/file
                         x-io/lines-in))

(def input (x/into []
                   (map #(zipmap [:target/reg :target/op :target/val :condition :condition/reg :condition/op :condition/val]
                                 (str/split % #" ")))
                   reducible-input))

(comment
  (apply = 7 (map count input))
  true
  (apply = "if" (map :condition input))
  true

  (frequencies (map :target/op input))
  {"inc" 499, "dec" 501}

  (frequencies (map :condition/op input))
  {"!=" 168, ">=" 177, ">" 170, "==" 148, "<=" 150, "<" 187}
  )

;; condition is always "if", :target/op is always +


(defn refine-input [lines]
  (x/into []
          (comp (map #(str/split % #" "))
                (map (fn [[target-reg target-op target-val _ condition-reg condition-op condition-val]]
                       {:target-reg target-reg
                        :target-op  ({"inc" + "dec" -} target-op)
                        :target-val (Integer. target-val)
                        :condition-reg condition-reg
                        :condition-op (case condition-op "!=" not=, ">=" >=, ">" >, "==" =, "<=" <=, "<" <)
                        :condition-val (Integer. condition-val)})))
          lines))


(defn compute-instruction [acc {:keys [target-reg target-op target-val
               condition-reg condition-op condition-val]}]
  (if (condition-op (get acc condition-reg 0)
                    condition-val)
    (update acc target-reg (fnil target-op 0) target-val)
    acc))



(def test-input ["b inc 5 if a > 1"
                 "a inc 1 if b < 5"
                 "c dec -10 if a >= 1"
                 "c inc -20 if c == 10"])

(comment
  (reductions compute-instruction
              {}
              (refine-input test-input))

         )

(->> reducible-input
     refine-input
     (reduce compute-instruction {})
     (apply max-key val))


;; part 2
(->> reducible-input
     refine-input
     (reductions compute-instruction {})
     rest  ;; get rid of the empty first element
     (map #(apply max-key val %))
     (apply max-key val))

