(def gg #( first
  (reduce (fn [[c n] e]
    (let [n (if (%2 e)
                   (inc n)
                   n)
          c (if (>= n %1) c (concat c [e]))]
      [c n])) [[] 0] %3)))


(println (gg 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23]))

(defn subset [current tot coll]
  (reduce (fn [[coll curr] e]
    (let [coll (if (sequential? e)
             (concat coll (subset curr n e))
             (concat coll [e]))
          curr (if (curr (+ e curr))

