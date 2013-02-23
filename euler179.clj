(defn partfactors [n]
  (count
    (filter (fn [x] (zero? (mod n x))) (range 2 (Math/sqrt n)))))

(defn perfectsq [n] (let [sq (Math/sqrt n)]
  (= sq (int sq))))



(defn countfactors [n]
  (let [fact (+ 2 (* 2 (partfactors n)))]
    (if (perfectsq n)
      (inc fact)
      fact)))

(def mapped
  (map  
    (fn [x] [x (countfactors x)])
    (range 2 (* 1000))))

(def map2 ( partition-by second mapped))

(def map2 (filter (fn [coll] (< 1 (count coll))) map2))

(println (apply + (map count map2)))



