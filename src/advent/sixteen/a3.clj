(ns advent.sixteen.a3)


(defn possible-triangle? [[a b c]]
  (and (< c (+ a b))
       (< b (+ a c))
       (< a (+ b c))))


(defn count-possible-triangles [sides]
  (->> (partition 3 sides)
       (filter possible-triangle?)
       count))
