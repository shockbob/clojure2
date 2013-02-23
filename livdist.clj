(def sb (fn [s n]
    (filter
       #(= n (count %))
       (reduce
          (fn [ss x] (concat ss (map #(conj % x) ss)))
          [#{}]
         s))))

(println (sb "abcdef" 5))
