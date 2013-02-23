
(defn totalsize [xsize ysize]
  (apply +
    (for [x (range 1 (inc xsize))
          y (range 1 (inc ysize))] (* (inc (- xsize x))(inc (- ysize y))))))

(defn absdiff [a b]
  (Math/abs (- a b)))


(def allvals (for [x (range 1 100) y (range 1 100)] [(absdiff 2000000 (totalsize x y)) (* x y)]))

(println (second (first (sort-by first allvals))))


