
(defn pad [n coll] (vec (concat (repeat (- n (count coll)) 0) coll)))

(defn makepat [ints]
  (let [strs (map #(Integer/toString % 2) ints)
        vecs (map (fn [s] (map #(- (int %) 48) s)) strs)
        maxcount (apply max (map count vecs))
        vecs (map (fn [vec] (pad maxcount vec)) vecs)]
    (vec vecs)))

(defn getverts [vecs]
  (let [
    nr (count vecs)
    nc (count (vecs 0))
    ]

    (for [c (range nc)]
      (for [r (range nr)]
        (get-in vecs [r c])))))

(defn rev [vecs]
  (vec (map (fn [v] (vec (reverse v))) vecs)))
(defn revrev [vecs]
  (vec (reverse (rev vecs))))

(def pats (makepat [1 3 7 7]))
(println pats)
(println (revrev pats))
(println (rev pats))


