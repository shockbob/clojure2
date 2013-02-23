
(defn sievepart [max map n]
  (if (get map n true)
    (reduce (fn [map x] (assoc map x false)) map (range (+ n n) max n))
    map))

(defn sieveall [max]
  (reduce (partial sievepart max) {} (range 2 max)))

(def primes (sieveall 100000))
(println (filter (fn [x] (get primes x true)) (range 2 100)))


