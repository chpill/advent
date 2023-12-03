(ns advent.seventeen.a4
  (:require [clojure.java.io :as java-io]
            [net.cgrand.xforms.io :as xforms-io]
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


;; Part 2

(defn valid-passphrase?-2 [passphrase]
  (let [word-seq (str/split passphrase #" ")]
    (apply distinct? (map sort word-seq))))


(comment
  (valid-passphrase?-2 "abcde fghij")
  (valid-passphrase?-2 "abcde xyz ecdab")
  (valid-passphrase?-2 "a ab abc abd abf abj")
  (valid-passphrase?-2 "oiii ioii iioi iiio")
  )


;; 119
(count (into []
             (filter valid-passphrase?-2)
             reducible-input))
