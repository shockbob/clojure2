
(def sets (for [a '(a A) b '(b B) c '(c C) d '(d D)] #{a b c d}))

(defn getallkmap [input]
  (map (fn [s] (hash-map s (contains? input s))) sets))

(def x (String.toLowerCase "xyzabc"))

(def xyz
       #{#{'a 'b 'c}
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})

(defn fetchrect [[nr nc] [sr sc] bigrect]
  (let [rs (take nr (iterate (fn [i] (mod (inc i) 4)) sr))
        cs (take nc (iterate (fn [i] (mod (inc i) 4)) sc))]
    (for  [r rs c cs] (nth (nth bigrect r) c))))

(defn swapper [[a b c d]] [a b d c])
(def xyzs (getallkmap xyz))
(println xyzs)
(def xyzs (map swapper (partition 4 xyzs)))
(def abcs (map swapper (for [i (range 4)] (for [j (range 4)] (nth (nth xyzs j) i)))))
(def pow2 #{2 4 8 16})
(def rects (for [nr (range 5) nc (range 5) :when (contains? pow2 (* nr nc))] [nr nc]))
(println rects)
(println abcs)
(def pairs [#{'a 'A} #{'b 'B} #{'c 'C} #{'d 'D}])
(defn getuniqs [rect]
  (let [bools (reduce (fn [s e] (into s e)) #{} (mapcat keys rect))]
    (flatten (remove next (map (fn [pair] (filter pair bools)) pairs)))))


(defn includes [big small]
  (let [bigset (set big)]
  (and (< (count small) (count big))
       (every? (fn [node] (contains? bigset node)) small))))

(def allrects (for [[nr nc] rects sr (range 4) sc (range 4)] (fetchrect [nr nc] [sr sc] abcs)))

(println (count allrects))
(def truerects (filter (fn [rect] (every? identity (mapcat vals rect))) allrects))
(println "count first true" (count truerects))
(def removed (set (distinct (mapcat (fn [rect] (filter (partial includes rect) truerects)) truerects)) ))
(def truerects (remove removed truerects))
(println "count removed truerects" (count removed) (count truerects))
(println truerects)
(println "removed" removed)
(println (distinct (mapcat getuniqs truerects)))
(println (zipmap truerects (map getuniqs truerects)))
