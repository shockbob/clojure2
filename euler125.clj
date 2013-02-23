(def ten8 (* 1000 1000 100))
(defn isdrome [n]
    (let [s (str n)
          rev (.toString (.reverse (StringBuilder. s)))] (= rev s)))

(defn runningtotal [coll]
  (reverse (reduce (fn [sums value] (concat [(+ (first sums) value)] sums ))
    [0] coll)))

(defn findsums [n totals totmap]
  (some
    (fn [[index tot]]
      (let [indexfound (totmap (long (+ n tot)))]
        (and indexfound (> (- indexfound index) 1))))
    totals))

(defn testIt [values search]
  (let [
    totals (map long (runningtotal values))
    totmap (zipmap totals (iterate inc 0))
    totals (map vector (iterate inc 0) totals)]
    (filter #(findsums % totals totmap ) search)))

(def dromes (filter isdrome (range 1 (inc ten8))))
(def sqrs (map #(long (* % %)) (range 1 10000)))
(def res (testIt sqrs dromes))
(println (apply +  res))
