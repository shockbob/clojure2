
(def bignum (* 1000 1000))
(defn numcubes [interiorsize]
  (+ 4 (* 4 interiorsize)))

(defn biggestsinglecube [numcubes]
  (int (/ (- numcubes 4) 4)))

(def cubes (filter (fn [x] (not= 0 x)) (distinct (map biggestsinglecube (range 1 (inc bignum))))))

(defn addlayers [max n]
  (let [start (numcubes n)]
    (take-while (fn [[numcubs sum n]] (and (> n 0)(<= sum max)))
      (iterate (fn [[numcubs sum n]] [(numcubes (- n 2)) (+ sum (numcubes (- n 2))) (- n 2) ] ) [start start n]))))

(def allcubes (reduce (fn [mymap n] (merge-with + mymap (frequencies (map second (addlayers bignum n))))) {} cubes))

(def mapped (map count (map (fn [size] (filter (fn [[k v]] (= v size)) allcubes)) (range 1 11))))

(println mapped)
(println (apply + mapped))
