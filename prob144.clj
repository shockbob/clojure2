

(defn funcit [val & funcs]
  (map first (iterate (fn [[v index]] [((nth funcs (mod index (count funcs))) v) (inc index)])
    [val 0])))

(println (take 15 (funcit 3 inc dec inc)))

(defn pastrap [n]
  (iterate (fn [x] (concat [(first x)] (map (fn [[a b]] (+ a b)) (partition 2 1 x)) [(last x)]))
    n))

(println (take 5 (pastrap [1])))


