(def gg (fn [sz st]
(letfn [
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
  (kcombos [st sz]
     (let [
       inds (index-combinations sz (count st))
       ]
       (set (map set
         (map
      (fn [s] (map (vec st) s)) inds))))) ]

  (cond (= 1 sz) (set (map (fn [x] (set [x])) st))
     (> sz (count st)) #{}
      (= sz (count st)) #{st}
     :else (kcombos st sz)))))
;(println (gg 2 #{0 1 2}))
(def gg (fn [f & args]
  (letfn [
    (x [f]
      (let [
        r (f)]
        (if (fn? r)
          (x r)
           r)))]
      (x #(apply f args)))))
(println (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (gg triple 2)))
