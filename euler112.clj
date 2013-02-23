(defn bouncy [n]
  (let [s (seq (str n))
        sorted (sort s)]
    (and (not= s sorted) (not= s (reverse sorted)))))

(defn solve112 []
  (drop-while
    (fn [[b number]] (not= (/ b number) (/ 99 100)))
    (iterate
      (fn [[b number]]
        (let [newb (if (bouncy number) (inc b) b)]
          [newb (inc number)]))
      [1 1])))

(println (first (solve112)))
