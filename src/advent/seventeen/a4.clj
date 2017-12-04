(ns advent.seventeen.a4
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]
            [net.cgrand.xforms :as x]
            [clojure.string :as str]))


(def reducible-input (-> (java-io/resource "seventeen/a4.txt")
                         java-io/file
                         xforms-io/lines-in))


(defn valid-passphrase? [passphrase]
  (let [word-seq (str/split passphrase #" ")]
    (apply distinct? word-seq)))


;; 325
(count (into []
             (filter valid-passphrase?)
             reducible-input))
