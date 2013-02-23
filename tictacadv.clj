(defn randmoves [] (shuffle (for [ i (range 3) j (range 3)] [i j])))
(defn emptyboard [] (repeat 3 (repeat 3 :e)))
(defn opponent [piece] ({:o :x :x :o} piece))
(defn getempties [board]
  (for [i (range 3) j (range 3) :when (= :e (nth (nth board i) j))][i j]))
(defn repl [input [i j] item]
  (map-indexed (fn [ii v] (if (= ii i)
    (concat (take j v) [item] (drop (inc j) v))
    v))
    input))
(defn who-won [board]
  (some {[:o :o :o] :o [:x :x :x] :x}
    (partition 3
      (map
        (fn [[i j]] (nth (nth board i) j))
        [[0 0][0 1] [0 2]
         [1 0][1 1][1 2]
         [2 0][2 1][2 2]
         [0 0][1 0][2 0]
         [0 1][1 1][2 1]
         [0 2][1 2][2 2]
         [0 0][1 1][2 2]
         [2 0][1 1][0 2]]))))
(defn getwinpos [board piece]
  (let [e (getempties board)]
     (set (filter (fn [[i j]]
                   (= piece (who-won (repl board [i j] piece))))
                         e))))

(defn makemoves [bd moves pc]
  (reductions (fn [[bd pc] mv]
                     [(repl bd mv pc) (opponent pc) ]) [bd pc] moves))
(defn winningboard []
   (let [ bd (emptyboard)
          piece :x
          moves (randmoves)
         ]
      (take-while (fn [[bd pc]] (nil? (who-won bd)))
          (makemoves bd moves piece))))

;;;(set (getwinpos board piece)))))
(def wb (map last (take 5 (filter #(not= 9 (count %)) (repeatedly winningboard)))))
(println (map (fn [[bd pc]] [(vec (map vec bd)) pc (getwinpos bd pc)]) wb ))



; (def wb (gg nil nil))
;(println wb (count wb))
; (println (gg [[:o :o :e]
;               [:x :e :x]
;               [:x :o :x] ]:o))
; (println (= #{[0 2] [1 1]}(gg [[:o :o :e]
;               [:x :e :x]
;               [:x :o :x] ]:o)))
;
;(println (= #{[1 1]} (gg [[:o :o :x]
;                     [:x :e :x]
;                    [:x :o :o]] :x)))
;
;(println (= #{} (gg (repeat 3 (repeat 3 :e)) :x)))
;
;(println (= #{[1 1]} (gg [[:o :o :x]
;                     [:x :e :x]
;                     [:x :o :o]] :x)))

