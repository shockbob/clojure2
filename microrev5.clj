
(def gg (fn [bd col] (letfn [
(op [pc]
  ('{w b b w} pc))

(inr [k mn mx]
  (and (<= k mx)(>= k mn)))

(ok [[i j]]
  (and (inr i 0 3) (inr j 0 3)))

(is? [b c [x y] ]
  (= (get-in b [x y]) c) )
(ise? [b [x y]]
  (is? b 'e [x y]))

(ns [[x y]]
  (filter
    ok
    (map
      (fn [[dx dy]] [(+ dx x)(+ dy y)])
      (for [i [-1 0 1] j (if (zero? i) [-1 1] [-1 0 1])]
    [i j]))))

(flps [b c [sx sy] [dx dy] fs]
   (let [newx (+ dx sx)
         newy (+ dy sy)
         fs (concat [[sx sy]] fs)
         ]
     (cond
       (is? b c [newx newy])   (flps b c [newx newy] [dx dy] fs)
       (is? b (op c) [newx newy]) fs
       :else  nil)))

(ns-of-c [b c [sx sy] ]
 (filter (partial is?  b c) (ns [sx sy])))

(nd [[sx sy] [nx ny]]
  [(- sx nx)(- sy ny)])

(ge [b]
  (filter
    (partial is? b 'e)
    (for [i (range 4) j (range 4)][i j])))

(pms [b neighbor-opp c]
  (map (fn [[neigh opps]] [neigh
            (mapcat
              (fn [opp] (let [dir (nd opp neigh)]
                (flps b c opp dir [])))
              opps)])
    neighbor-opp))

(getmoves [b c]
    (let [ opp (op c)
           es  (ge b)
           withneighs (map (fn [e] [e (ns-of-c b opp e)]) es)
           possiblemoves   (pms b withneighs opp)
           moves (filter (fn [[neigh fs]] (seq fs)) possiblemoves )
           moves (reduce (fn [m [neigh fs]] (assoc m neigh (set fs))) {} moves)]
    moves
  ))]
(getmoves bd col))))

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
