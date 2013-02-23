(defn perfectsq [n] (let [sq (Math/sqrt n)]
  (= sq (int sq))))

(defn leglen1 [b]
  (let [leg (dec b)
        leg2 (/ b 2)
        a2b2 (+ (* leg leg)(* leg2 leg2))]
    a2b2))

(defn leglen2 [b]
  (let [leg (inc b)
        leg2 (/ b 2)
        a2b2 (+ (* leg leg)(* leg2 leg2))]
    a2b2))

(defn correcto1 [b]
    (perfectsq (leglen1 b)))

(defn correcto2 [b]
    (perfectsq (leglen2 b)))

(def first12 (take-while (fn [x] (< x 3333333)) (filter (fn [x] (or (correcto1 x)(correcto2 x))) (iterate inc 1))))
(println first12)
(println (apply + (map (fn [b] (if (correcto1 b)(leglen1 b)(leglen2 b))) first12)))