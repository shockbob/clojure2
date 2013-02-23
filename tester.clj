
(defn part1 [n]
  (take-while (fn [x] (>= x 1)) (iterate (fn [x] (quot x 10)) n)))
(defn part2 [n]
  ())

(println (part1 12340))
