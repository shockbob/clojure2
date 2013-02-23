


(defn ll[v]  (let [

sss  (mapcat #(take-while seq (iterate butlast (subvec v %))) (range 0 (count v)))
inc (filter (fn [x] (and (> (count x) 1) (apply < x))) sss)
best (last (sort-by count inc))
]
(if (nil? best)
  []
  best)))


(println (ll [6 5 4 3 2 1]))

(def gg (fn [v]  (let [
c (count v)
i (for [i (range c) j (range (inc i) (inc c))]
   (let [k (subvec (vec v) i j)]
     (if (apply < k)
       k)))
x (apply max (map count i))
]
(if (= x 1)
  []
   (some #(if (= x (count %)) % ) i)))))

(def gg (fn [v]
  (let [cv (count v)]
    (reduce (fn [a i]
      (reduce (fn [a j]
        (let [k (subvec (vec v) i j)
              ck (count k)]
          (if (and (apply < k)(> ck (count a)))
            k
            a))) a (range (+ 2 i) (inc cv)))) [] (range 0 cv)))))



(println (gg [1 2 1 3 4]))