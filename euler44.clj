
(defn pent [n] (/ (* n (- (* 3 n) 1)) 2))

(def bignum 10000)
(def half (/ bignum 2))
(def pents (vec (map pent (range 1 bignum))))
(def pentset (set pents))
(defn goodinds [] (for [i (range 1 half) j (range (inc i) half)
                   :let [sum (+ (pents i) (pents j))
                         diff (- (pents j) (pents i))]
                   :when (and (pentset sum) (pentset diff))
                   ]
  [(- (pents j) (pents i))]))

(println (goodinds))
