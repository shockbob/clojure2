(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn getsize [cells]
  (let [ [isize jsize] (map #(apply max (map % cells)) [first second])]
    [(inc isize) (inc jsize)]))

(defn tostr [cells isize jsize]
  (let
    [s (set cells)]
    (for [i (range isize)]
      (for [j (range jsize)]
        (if (s [i j])
          "#"
          " ")))))       j





(def gun #{[2 0][2 1][2 2][1 2][0 1]})
(println (tostr gun 3 3))
(println (mapcat neighbours gun))
(println (frequencies (mapcat neighbours gun)))
(def diag #{[0 0][1 1][2 2]})
(def flash #{[2 0][2 1][2 2]})

(println (getsize diag))
(def sets (map sort (take 5 (iterate step gun))))
(println sets)
