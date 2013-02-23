(def __ (fn [sqr milli] (letfn [
(subsqr [sqr size si sj]
  (vec (map (fn [i] (subvec (sqr (+ si i)) sj (+ sj size))) (range size))))
(nodups2 [coll seen]
  (if (empty? coll)
    true
    (let [f (first coll)]
    (if (or (nil? f) (contains? seen f))
      false
      (recur (rest coll) (clojure.set/union seen #{f}))))))
(nodups [coll] (nodups2 coll #{}))
(islatin [sqr size]
  (let [flat (flatten sqr)]
      (if (and (nodups flat)
               (every? nodups sqr))
        (let [
          cols (for [j (range size)] (for [i (range size)] (get-in sqr [i j])))
          ]
          (every? nodups cols)))))

(getrange [combo maxcol r size]
    (let [minc (apply max (subvec (:min combo) r (+ r size)))
          maxc (apply min (subvec (:max combo) r (+ r size)))]
      (range minc (min maxccol maxc))))

(countlatins [combo size nr nc]
  (let [
    rr (range (inc (- nr size)))
    maxc (- nc size)
    sisj (for [r rr c (getrange combo maxcol r size) ] [r c])
;    mill1 (System/currentTimeMillis)
    ct (reduce
      (fn [s [r c]]
        (if (get-in sqr [r c])
          (let [sub (subsqr (combo :min) size r c)]
            (if (islatin sub size)
              (concat s [{:sz size :sb sub}])
              s))
          s))
      []
      sisj)
 ;   xx (doall (println "YY" size (- (System/currentTimeMillis) mill1)))
    ]
    ct))
(inccarry [n base]
  (if (= base (inc n))
    [0 1]
    [(inc n) 0]))

(incvec [[fv & rv] [fb & rb]]
  (if (nil? fv)
    nil
    (let [[incv carry] (inccarry fv fb)]
    (if (zero? carry)
      (concat  [incv] rv)
      (concat [incv ] (incvec rv rb))))))


(latsqr [nr nc size combo]
  (reduce (fn [m sz]
      (concat (countlatins combo sz nr nc) m))
       [] (range 2 (inc size))))

(pad [row i maxsize] (take maxsize (concat (repeat i nil) row (repeat nil))))

(combos [sqr]
  (let [
    maxsize (apply max (map count sqr))
    bs (map (fn [row] (inc (- maxsize (count row)))) sqr)
    cts (map count sqr)
    combocount (apply * bs)
    zeros (repeat (count bs) 0)
    all (take combocount (iterate (fn [v] (incvec v bs)) zeros))
    maxes (map #(hash-map :min (vec %1) :max (vec (map + %1 cts))) all)]
    maxes))]
  (let [com (combos sqr)
;        cts (map count sqr)
;        minct (apply min cts)
;        nc (apply max cts)
;        nr (count sqr)
;        size (min minct nr)
;        ms (mapcat #(latsqr nr nc size %) com)
;        ms (set ms)
;        f (frequencies (map :sz ms))]
;    f))))
] com))))

(defn nodups2 [coll seen]
  (if (empty? coll)
    true
    (let [f (first coll)]
    (if (or (nil? f) (contains? seen f))
      false
      (recur (rest coll) (clojure.set/union seen #{f}))))))
(defn nodups [coll] (nodups2 coll #{}))

(println (nodups [1 4 2 3]))

(println
;(= (__ '[[A B C D]
;         [A C D B]
;         [B A D C]
;         [D C A B]])
;   {})
;
;(= (__ '[[A B C D E F]
;         [B C D E F A]
;         [C D E F A B]
;         [D E F A B C]
;         [E F A B C D]
;         [F A B C D E]])
;   {6 1})
;
;(= (__ '[[A B C D]
;         [B A D C]
;         [D C B A]
;         [C D A B]])
;   {4 1, 2 4})
;
;(= (__ '[[B D A C B]
;         [D A B C A]
;         [A B C A B]
;         [B C A B C]
;         [A D B C A]])
;   {3 3})
;
;(= (__ [  [2 4 6 3]
;        [3 4 6 2]
;          [6 2 4]  ])
;   {})
;
;(= (__ [[1]
;        [1 2 1 2]
;        [2 1 2 1]
;        [1 2 1 2]
;        []       ])
;   {2 2})
;
;(= (__ [[3 1 2]
;        [1 2 3 1 3 4]
;        [2 3 1 3]    ])
;   {3 1, 2 2})

(time (__ [[8 6 7 3 2 5 1 4]
        [6 8 3 7]
        [7 3 8 6]
        [3 7 6 8 1 4 5 2]
              [1 8 5 2 4]
              [8 1 2 4 5]] (System/currentTimeMillis)))
  ; {4 1, 3 1, 2 7})
)


;(println size)
;(println (take-while (fn [s] (<= (count s) (count comb))) (map #(Integer/toString % size) (range))))


;(defn tobasen [b n size]
;  (let [st (Integer/toString b n)]
;    (map  #(Character/getNumericValue %) (reverse (take size (concat (reverse st) (repeat \0)))))))
;
;(println (tobasen 23 2 8))

;
;(defn trimsqr [sqr]
;  (let [discard (fn [rows] (drop-while (fn [row] (< (count row) 2)) rows))
;        kept (discard sqr)]
;        (reverse (discard (reverse kept)))))
;
;(padsqr [sqr]
;   (let [mx (max (count sqr) (count (sqr 0)))
;         padrow (- mx (count (sqr 0)))
;         padcol (- mx (count sqr))
;         sqr (vec (map (fn [row] (vec (concat row (repeat padrow nil)))) sqr))]
;     (vec (concat sqr (repeat padcol (vec (repeat mx nil)))))))
;
;(def __ (fn [sqr milli] (letfn [
;(subsqr [sqr size si sj]
;  (vec (map (fn [i] (subvec (sqr (+ si i)) sj (+ sj size))) (range size))))
;(nodups2 [coll seen]
;  (if (empty? coll)
;    true
;    (let [f (first coll)]
;    (if (or (nil? f) (contains? seen f))
;      false
;      (recur (rest coll) (clojure.set/union seen #{f}))))))
;(nodups [coll] (nodups2 coll #{}))
;(islatin [sqr size]
;  (let [flat (flatten sqr)]
;      (if (and (nodups flat)
;               (every? nodups sqr))
;        (let [
;          cols (for [j (range size)] (for [i (range size)] (get-in sqr [i j])))
;          ]
;          (every? nodups cols)))))
;
;(countlatins [sqr size nr nc]
;  (let [
;    rr (range (inc (- nr size)))
;    rc (range (inc (- nc size)))
;    sisj (for [r rr c rc] [r c])
;;    mill1 (System/currentTimeMillis)
;    ct (reduce
;      (fn [s [r c]]
;        (if (get-in sqr [r c])
;          (let [sub (subsqr sqr size r c)]
;            (if (islatin sub size)
;              (concat s [{:sz size :sb sub}])
;              s))
;          s))
;      []
;      sisj)
; ;   xx (doall (println "YY" size (- (System/currentTimeMillis) mill1)))
;    ]
;    ct))
;(inccarry [n base]
;  (if (= base (inc n))
;    [0 1]
;    [(inc n) 0]))
;
;(incvec [[fv & rv] [fb & rb]]
;  (if (nil? fv)
;    nil
;    (let [[incv carry] (inccarry fv fb)]
;    (if (zero? carry)
;      (concat  [incv] rv)
;      (concat [incv ] (incvec rv rb))))))
;
;
;(latsqr [nr nc size sqr]
;  (reduce (fn [m sz]
;      (concat (countlatins sqr sz nr nc) m))
;       [] (range 2 (inc size))))
;
;(pad [row i maxsize] (take maxsize (concat (repeat i nil) row (repeat nil))))
;
;(combos [sqr]
;  (let [
;    maxsize (apply max (map count sqr))
;    bs (map (fn [row] (inc (- maxsize (count row)))) sqr)
;    combocount (apply * bs)
;    zeros (repeat (count bs) 0)
;    all (take combocount (iterate (fn [v] (incvec v bs)) zeros))
;    sqrcombos (map (fn [combo] (map (fn [i row] (vec (pad row i maxsize))) combo sqr)) all)]
;    sqrcombos))]
;  (let [com (map vec (combos sqr))
;        cts (map count sqr)
;        minct (apply min cts)
;        nc (apply max cts)
;        nr (count sqr)
;        size (min minct nr)
;        ms (mapcat #(latsqr nr nc size %) com)
;        ms (set ms)
;        f (frequencies (map :sz ms))]
;    f))))
;
;(defn nodups2 [coll seen]
;  (if (empty? coll)
;    true
;    (let [f (first coll)]
;    (if (or (nil? f) (contains? seen f))
;      false
;      (recur (rest coll) (clojure.set/union seen #{f}))))))
;(defn nodups [coll] (nodups2 coll #{}))