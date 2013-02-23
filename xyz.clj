(defn xyz [col] (mapcat #(conj % (first %))
  (keep next (partition-by identity col))))
(defn xyz2 [col] (flatten
  (filter #(> (count %) 1)
    (partition-by identity col))))
(def c [1 1 2 2 2 3 4 4 5])
(println (xyz c))
(println (xyz2 c))
(println (mapcat #(vec [% (+ % %) ]) [1 2 3 4]))

