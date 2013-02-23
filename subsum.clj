(def __(fn [& vs]
  (let [
    p2 (iterate #(* 2 %) 1)
    ov (fn [n hm]
          (filter
            (fn [x] (= hm (count (filter #(bit-test x %) (range n)))))
            (range (nth p2 n))))
    subs (fn [v ov]
         (reduce +
          (map-indexed
            (fn [i s] (if (bit-test ov i) s 0))
            v)))

    allsubs (fn [v]
      (let [n (count v)]
      (distinct (mapcat (fn [i] (map (fn [o] (subs v o)) (ov n i))) (range 1 (inc n))))))

    [f & r] (map allsubs vs)
    rests (reduce concat r)
    ]
    (true? (some (fn [e] (= (dec (count vs)) (count (filter #{e} rests)))) f)))))

(println
  (= true  (__ #{-1 1 99}
             #{-2 2 888}
             #{-3 3 7777})) ; ex. all sets have a subset which sums to zero

(= false (__ #{1}
             #{2}
             #{3}
             #{4}))

(= true  (__ #{1}))

(= false (__ #{1 -3 51 9}
             #{0}
             #{9 2 81 33}))

(= true  (__ #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))

(= false (__ #{-1 -2 -3 -4 -5 -6}
             #{1 2 3 4 5 6 7 8 9}))

(= true  (__ #{1 3 5 7}
             #{2 4 6 8}))

(= true  (__ #{-1 3 -5 7 -9 11 -13 15}
             #{1 -3 5 -7 9 -11 13 -15}
             #{1 -1 2 -2 4 -4 8 -8}))

(= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
             #{10 -9 8 -7 6 -5 4 -3 2 -1}))
  )



