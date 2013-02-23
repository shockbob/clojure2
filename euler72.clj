(require 'primefuncs)
(def bignum (* 1000 1000))
(def primes (allsieve (inc bignum)))
(def phis (map (fn [x] (phi x primes)) (range 2 (inc bignum))))
(println (apply + phis))


