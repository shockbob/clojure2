(defn revint [c n]
  (partition (quot (count c) n) (mapcat #(range % (count c) n) (range 0 n))))
(def gg (fn [c n]
  (map (fn [z]
    (map #(nth c %) z))
    (map
      #(range % (count c) n)
      (range 0 n)))))

(println (gg (range 10) 5))

(defn ri [c n] (let [c (vec c) ct (count c)]
(for [i (range 0 n)] (for [j (range i ct n)] (c j)))))

(println (ri (range 9) 3))

(fn [v]  (let [
s (mapcat
     #(take-while seq
       (iterate butlast (subvec v %)))
        (range (count v)))
i (filter #(apply < %) s)
mx (apply max (map count i))
]
(if (= 1 mx)
  []
  (some #(if (= mx (count %)) % ) i))))
