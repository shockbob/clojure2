(defn who-won [board]
  (some {[:o :o :o] :o [:x :x :x] :x}
    (partition 3
      (map
        (vec board)
        [0 1 2 3 4 5 6 7 8
         0 3 6 1 4 7 2 5 8
         0 4 8 2 4 6]))))

(defn repl [input index item]
  (concat (take index input) [item] (drop (inc index) input)))

(defn getempties [board]
  (keep-indexed (fn [i a] (if (= a :e) i)) board))

(defn getwinpos [board piece]
    (filter (fn [e] (= piece (who-won (repl board e piece)))) (getempties board)))

(println (getwinpos [:o :o :x
                     :e :e :x
                     :x :o :o] :o))
(println (getwinpos (repeat 9 :e) :x))

(println (getwinpos [:x :x :o
                     :o :e :o
                     :o :x :e] :o))

(println (getwinpos [:x :o :x
                     :e :x :x
                     :o :o :e] :x))
