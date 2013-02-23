(defn sumdigs [n] (apply +
  (map
    (fn [x] (Character/getNumericValue x))
    (str n))))

(defn sumdigsmatch [n]
  (let [powers (take 10 (iterate (fn [x] (* n x)) (* n n)))]
  (filter (fn [x] (= n (sumdigs x))) powers)))

(defn matchers []
  (sort
    (mapcat
      (fn [x] (sumdigsmatch x))
      (range 2 100))))

(time (println (nth (matchers) 29)))