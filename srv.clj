
(defn min-max [current value]
  (assoc current :min (min value (current :min value)) :max (max value (current :max value))))

(defn min-max-seq [values]
  (reduce (fn [m e] (min-max m e)) {} values))

(println  (min-max-seq [23 2 -4 5 18 19 210]))

(defn min-max-xyzs [xyzs xyzkeys]
  (reduce (fn [m var] (assoc m var (min-max-seq (map var xyzs)))) {} xyzkeys))

(println (min-max-xyzs [{:x 10 :y 20 :z -20} {:x -23 :y 10 :z 400}] [:x :y :z]))

(def randcoord #(- 10 (rand 20)))

(def points
  (take 100 (filter (fn [{x :x y :y z :z}] (< 100 (+ (* x x)(* y y))))
    (repeatedly #(hash-map :x (randcoord) :y (randcoord) :z (randcoord))))))

(defn volume [minmax]
  (let [x (minmax :x) y (minmax :y) z (minmax :z)]
  (* (- (x :max)  (x :min))  (- (y :max)  (y :min)) (- (z :max)  (z :min)))))

(println (min-max-xyzs points [:x :y :z]))

(def splits (vals (group-by (fn [{x :x}] (int x)) points)))

(println (map volume (map (fn [coll] (min-max-xyzs coll [:x :y :z])) splits)))