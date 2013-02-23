

(defn inrange [n] (and (< n (/ 2)) (> n (/ 3))))

(defn getallinrange [denom]
  (let [min (dec (int (/ denom 3)))
        max (inc (int (/ denom 2)))]
    (map #(/ % denom) (range min max))))
;(filter inrange (map #(/ % denom) (range min max)))))


(println (count  (distinct (flatten (map #(getallinrange %) (range 2 12001))))))

(println (count (getallinrange 12000)))
