
(defn ptdistsq [[x1 y1][x2 y2]]
  (let [xdist (- x1 x2)
        ydist (- y1 y2)
        dist (+ (* xdist xdist) (* ydist ydist))]
    dist
    ))

(defn goodtri [p1 p2]
  (let [dist1 (ptdistsq p1 p2)
        dist2 (ptdistsq p1 [0 0])
        dist3 (ptdistsq p2 [0 0])]
    (if (or (zero? dist1) (zero? dist2) (zero? dist3))
      false
      (or (= dist1 (+ dist2 dist3))
        (= dist2 (+ dist1 dist3))
        (= dist3 (+ dist1 dist2))))))

(println (goodtri [1 0] [1 1]))

(def ptrange (range 0 51))

(def goodtris (for [x1 ptrange
                    y1 ptrange
                    x2 ptrange
                    y2 ptrange
                    :when (goodtri [x1 y1][x2 y2])]
  [x1 y1 x2 y2]))

(println (/ (count goodtris) 2))


