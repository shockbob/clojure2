
(defn getempties [board]
  (filter (fn [i] (= :e (nth board i))) (range (count board))))

(defn repl [board loc piece]
        (map
          #(if (= % loc)
            piece
            (board %))
          (range (count board) )))

(defn who-won [board] (some {[:o :o :o] :o [:x :x :x] :x}
  (partition 3 (map (vec board) [0 1 2 3 4 5 6 7 8 0 3 6 1 4 7 2 5 8 0 4 8 2 4 6]))))


(defn get-win [board piece]
  (let [e (getempties board)
        newboards (map (fn [e] [e (repl board e piece)]) e)]
    (some (fn [[e newboard]] (if (= piece (who-won newboard)) e)) newboards)))

(println (get-win [:o :e :o :e :e :e :e :e :e] :o ))

(println (repl [1 2 3 4] 2 99))