(def xx (fn [setxx] (
  letfn [
    (index-combinations
      [n cnt]
      (lazy-seq
       (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
       iter-comb
       (fn iter-comb [c j]
         (if (> j n) nil
             (let [c (assoc c j (dec (c j)))]
         (if (< (c j) j) [c (inc j)]
             (loop [c c, j j]
               (if (= j 1) [c j]
             (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
       step
       (fn step [c j]
         (cons (rseq (subvec c 1 (inc n)))
         (lazy-seq (let [next-step (iter-comb c j)]
               (when next-step (step (next-step 0) (next-step 1)))))))]
         (step c 1))))

(getstuff [numdigs setsize]
  (index-combinations  numdigs setsize))

(powerset [setxx] (let [
  myset setxx
  sets (reduce (fn [s d](concat (getstuff d (count myset)) s)) #{} (range 1 (count myset)))
  stuff (set (map set
    (map
      (fn [s] (map (vec myset) s)) sets)))]
  (set (concat  [#{}] [setxx] stuff))))
]
(powerset setxx))))

(println (xx #{1 2 3}))

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
	 iter-comb
	 (fn iter-comb [c j]
	   (if (> j n) nil
	       (let [c (assoc c j (dec (c j)))]
		 (if (< (c j) j) [c (inc j)]
		     (loop [c c, j j]
		       (if (= j 1) [c j]
			   (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
	 step
	 (fn step [c j]
	   (cons (rseq (subvec c 1 (inc n)))
		 (lazy-seq (let [next-step (iter-comb c j)]
			     (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(println (index-combinations 3 5))