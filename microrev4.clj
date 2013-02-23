
(def gg (fn [bd col] (letfn [
(opponent [pc]
  ('{w b b w} pc))

(inrange [k mink maxk]
  (and (<= k maxk)(>= k mink)))

(valid [[i j]]
  (and (inrange i 0 3) (inrange j 0 3)))

(isempty? [board [x y]]
  (not (or (contains? (board 'w) [x y]) (contains? (board 'b) [x y]))))

(is? [board color [x y] ]
  (contains? (board color) [x y]))

(neighbors [[x y]]
  (filter
    valid
    (map
      (fn [[dx dy]] [(+ dx x)(+ dy y)])
      (for [i [-1 0 1] j (if (zero? i) [-1 1] [-1 0 1])]
    [i j]))))

(getdirflips [board color [sx sy] [dx dy] flips]
   (let [newx (+ dx sx)
         newy (+ dy sy)
         flips (concat [[sx sy]] flips)
         ]
     (cond
       (contains? (board color) [newx newy])  (getdirflips board color [newx newy] [dx dy] flips)
       (contains? (board (opponent color)) [newx newy]) flips
       :else  nil)))

(neighbors-of-color [board color [sx sy] ]
 (filter (partial is?  board color) (neighbors [sx sy])))

(getneighbordirection [[sx sy] [nx ny]]
  [(- sx nx)(- sy ny)])

(getempties [board slots]
  (distinct (mapcat
              (fn [oppslot] (filter
                              (fn [xy] (isempty? board xy))
                              (neighbors oppslot)))
              slots)))

(getpossiblemoves [board neighbor-opp color]
  (map (fn [[neigh opps]] [neigh
            (mapcat
              (fn [opp] (let [dir (getneighbordirection opp neigh)]
                (getdirflips board color opp dir [])))
              opps)])
    neighbor-opp))

(getmoves [board color]
    (let [
           opp (opponent color)
           opplocations (board opp)
           empties  (getempties board opplocations)
           withneighs (map (fn [e] [e (neighbors-of-color board opp e)]) empties)
           possiblemoves   (getpossiblemoves board withneighs opp)
           moves (filter (fn [[neigh flips]] (seq flips)) possiblemoves )
           moves (reduce (fn [m [neigh flips]] (assoc m neigh (set flips))) {} moves)]
    moves
  ))
(makemapboard [board]
  (let [mi (for
             [i (range (count board)) j (range (count (board 0)))]
                [[i j] (get-in board [i j])])
        bs (map first (filter (fn [[ind col]] (= col 'b)) mi))
        ws (map first (filter (fn [[ind col]] (= col 'w)) mi))
        ]
   {'w (set ws) 'b (set bs)}))]
(getmoves (makemapboard bd) col))))


;(use 'clojure.set)
;(defn removepiece [board color [x y] ]
;  (assoc board color (difference (board color) #{[x y]} )))
;
;(defn addpiece [board color [x y] ]
;  (assoc board color (union (board color) #{[x y]})))
;
;(defn flip [board color [x y]]
;  (let [bd (addpiece board color [x y])
;        bd (removepiece bd (opponent color) [x y])]
;    bd))
;
;(defn makemove [board color]
;  (let [
;         moves (getmoves board color)
;         [location flips] (last (sort-by (fn [[neigh flips]] (count flips)) moves))
;         board (addpiece board color location)
;         board (reduce (fn [board pc] (flip board color pc)) board flips)
;         ]
;  board))
;
;(defn makeboardstr [board] (interpose "]\n[" (for [i (range 4)] (apply str (for [j (range 4)]
;    (cond
;      (contains? (board :w) [i j]) ":w "
;      (contains? (board :b) [i j]) ":b "
;      :else (str ":e ")))))))
;
;
;(def newboard { :w #{[1 1][2 2]} :b #{[1 2][2 1]}})

;(def board newboard)
;(def boards (take 10 (iterate
;              (fn [[board color]] [(makemove board color) (opponent color)])
;              [board :w])))
;
;(println (replace {\( \[ \) \]} (interpose "\n" (map makeboardstr (map first boards)))))
;

(def __ gg)

(println
(= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
  (__ '[[ e e e e ]
       [ e w b e ]
       [ e b w e ]
       [ e e e e ]] 'w))

(= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
  (__ '[[ e e e e ]
       [ e w b e ]
       [ w w w e ]
       [ e e e e ]] 'b))

(= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
  (__ '[[ e e e e ]
       [ e w b e ]
       [ w w b e ]
       [ e e b e ]] 'w))

(= {[0 3] #{[1 2]}, [3 3] #{[3 2] [2 2] [3 1]}, [2 3] #{[2 2]}}
  (__ '[[ e e e e ]
       [ e w b w ]
       [ w w b e ]
       [ w b b e ]] 'w))
)