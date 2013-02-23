(defn sieve [isprime n max]
  (reduce (fn [map n] (assoc map n false)) isprime (range (+ n n) max n)))

(defn allsieve [max]
  (reduce
    (fn [map n]
      (if (get map n true)
        (sieve map n max)
        map))
      (sorted-map)
      (range 2 max)))

(def bignum (* 50 1000 1000))

(def primes (allsieve (Math/sqrt bignum)))

(def allprimes (filter (fn [prime] (primes prime true)) (range 2 bignum)))

(defn genvalues [func]
  (take-while (partial > bignum) (map func allprimes)))
(def sqrs (genvalues (fn [x] (* x x)) ))
(def cubes (genvalues (fn [x] (* x x x)) ))
(def quads (genvalues (fn [x] (* x x x x)) ))

(def goodlist (for [quad quads cube cubes sqr sqrs
      :let [sum (+ sqr cube quad)]
      :when (< sum bignum)] sum))

(println (count (distinct goodlist)))
