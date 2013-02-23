(use 'clojure.set)
(def newboard { :w #{[3 3][4 4]} :b #{[4 3][3 4]}})

(defn opponent [pc]
  ({:w :b :b :w} pc))

(defn removepiece [board color [x y] ]
  (assoc board color (difference (board color) #{[x y]} )))

(defn addpiece [board color [x y] ]
  (assoc board color (union (board color) #{[x y]})))

(defn flip [board color [x y]]
  (let [bd (addpiece board color [x y])
        bd (removepiece bd (opponent color) [x y])]
    bd))

(defn inrange [k mink maxk]
  (and (<= k maxk)(>= k mink)))

(defn valid [[i j]]
  (and (inrange i 0 7) (inrange j 0 7)))

(def directions
  (for [i [-1 0 1] j (if (zero? i) [-1 1] [-1 0 1])]
    [i j]))

(defn isempty? [board [x y]]
  (not (or (contains? (board :w) [x y]) (contains? (board :b) [x y]))))


(defn neighbors [[x y]]
  (filter
    valid
    (map
      (fn [[dx dy]] [(+ dx x)(+ dy y)])
      directions)))

(defn getdirflips [board color [sx sy] [dx dy] flips]
   (let [newx (+ dx sx)
         newy (+ dy sy)
         flips (concat [[sx sy]] flips)]
     (if (isempty? board [newx newy])
       nil
       (if (contains? (board color) [newx newy])
         (getdirflips board color [newx newy] [dx dy] flips)
         flips))))

(defn getflips [board color [sx sy] ]
  (let [ns (filter (fn [[x y]] (isempty? board [x y])) (neighbors [sx sy]))
        dirs (map (fn [[x y]] [(- sy y) (- sx x)]) ns)
        mc (zipmap dirs ns)]
    (filter (fn [[neigh flips]] (seq flips))
      (map (fn [[dir neigh]] [neigh (getdirflips  board color [sx sy] dir [])]) mc))))


(println (neighbors [0 0]))
(println (map (fn [xy] [xy (isempty? newboard xy)]) (neighbors [3 4])))
(println (neighbors [7 7]))
(println (neighbors [7 0]))
(def movesandflips (mapcat (fn [[x y]] (getflips newboard :w [x y])) (newboard :w)))
(println movesandflips)
(println (distinct (map first movesandflips)))
(println (flip newboard :w  [3 4]))
(println (addpiece newboard :w [2 3]))
(println (removepiece newboard :w [3 3]))

