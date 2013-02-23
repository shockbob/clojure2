(def gg (fn [x] (let
  [y (fn [coll] (mapcat (fn [z]
    [(count z) (first z)])
        (partition-by identity coll)))]
    (iterate y x))))

(println (rest (take 7 (gg [2 2 2]))))


(def gg (fn [ & q]
  (reify clojure.lang.Seqable
    (seq [z] (distinct q))
    (toString [z] (apply str (interpose ", " (sort q)))) )))

(println (seq (gg 1 2 3 4 4 3)))



