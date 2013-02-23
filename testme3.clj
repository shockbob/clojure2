
(defn maxlen [n]
  (reduce (fn [[start len last index] e]
       (if (> last e)
         [start (inc len) e (inc index)]
         [index 0 e (inc index)]))
    [0 0 (first n) 0] n))

(println (maxlen [1 2 3 4 5 3 4 5]))
