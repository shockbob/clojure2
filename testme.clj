
(defn check2 [flat combo]
 (let [ d (distinct (map flat combo))]
            (if (= 1 (count d))
                     (first d))))

(def combos [[0 1 2][3 4 5][6 7 8][0 3 6][1 4 7][2 5 8][0 4 8][2 4 6]])

(defn check [b] (let
      [flat (vec (map (fn [x] (if (= :e x) nil x))(flatten b)))]
           (first (filter identity (distinct (map (fn [combo] (check2 flat combo)) combos))))))

(defn convert [a]
  (let [roms
    {\X 10 \I 1 \V 5 \C 100 \L 50 \D 500
    \M 1000}
    reva (reverse a)
    mynums (map roms reva)
    nums (reduce (fn [[max sum] num]
      (if (<  num max)
          [max (- sum num)]
          [num (+ sum num)])) [0 0] mynums)
 ] (last nums)))

(defn  todigs [n]
      (map (fn [i] (Character/getNumericValue i))
         (seq (Integer/toString n))))
(defn  sumsq [n]
      (reduce +
        (map (fn [i] (* i i ))
              (todigs n))))
(defn func [x]
  (loop [used #{} current x]
    (if (= 1 current)
      true
      (if (used current)
          false
          (recur (conj used current)
             (reduce +
        (map (fn [i] (* i i ))
              (map (fn [x] (Character/getNumericValue x))
         (seq (Integer/toString current))))))))))
(defn convert [a]
    (loop [st (seq a) upcase false retval []]
       (if (empty? st)
         (apply str retval)
        (let [
         next (if upcase
           (Character/toUpperCase (first st))
           (first st))
         upcase (= \- (first st))
         retval (if upcase retval (conj retval next))]
        (recur (rest st) upcase
retval)))))

(defn rot [coll n]
  (let [
    c (if (< n 0) coll (reverse coll))
    an (Math/abs n)
    roted
    (last (take (inc an)
      (iterate
        (fn [a] (concat (rest a) [(first a)])) c)))
    ]
    (if (< n 0)
      roted
      (reverse roted))))


(defn gg [n]
  (if (= 1 n)
    1
    (letfn
      [ (gcd [a b]
        (loop [a a b b]
          (if (zero? b)
            a
            (recur b (mod a b)))))]
      (inc (count
        (filter
          (fn [x] (= 1 (gcd x n)))
          (range 2 n)))))))

(defn biginc [n]
  (let [incs
   (map (fn [i] (take-while (fn [j] (< (n j) (n (inc j)))) (range i (dec (count n))))) (range 0 (count n)))
   mx (apply max (map count incs))
   filt (first (filter (fn [x] (= mx (count x))) incs))
   fixed (if (> (count filt) 1)
      (map (vec n) (concat filt [(inc (last filt))]))
      [])
    ]
    fixed))

 (defn mw [func m & ms]
   (let [
     ms (concat [m] ms)]
     (reduce
       (fn [bigmap key]
         (assoc bigmap key (reduce func (filter identity (map key ms))))) {} (keys m))))

(defn lcm [& nums]
  (let [mn (apply min nums)]
  (first (filter
      (fn [mult]
         (every? (fn [x] (zero? (mod mult x))) nums))
      (iterate (fn [x] (+ x mn)) mn)))))


(defn visit [node]
  (if (nil? node)
    "X"
    (str (visit (first (rest node))) (first node) (visit (last node)))
    ))



(def board  ["      "
        " ##   "
        " ##   "
        "   ## "
        "   ## "
        "      "])

(def deltas (filter (fn [[a b]] (not (and (= 0 a)(= 0 b)))) (for [i (range -1 2) j (range -1 2)] [i j])))

(defn inrange [i mini maxi]
  (and (>= i mini)(< i maxi)))

(defn neighbors [i j isize jsize]
  (let [ij (map (fn [[di dj]] [(+ di i) (+ dj j)]) deltas)                     ]
    (filter (fn [[ni nj]] (and (inrange ni 0 isize)(inrange nj 0 jsize))) ij)))

(defn getneighbors [board i j isize jsize]
  (filter (fn [x] (= x \#)) (map (fn [[ii jj]] (nth (nth board ii) jj)) (neighbors i j isize jsize))))

(defn result [cell n]
  (if (= cell \#)
    (cond (< n 2) " "
           (or ( = n 3) (= n 2)) "#"
           (> n 3) " ")
    (cond (= n 3) "#"
          :else " ")))

(def results (let [isize (count board)
      jsize (count (first board))]
      (map (partial apply str)(partition jsize (for [i (range isize) j (range jsize)]
         (result (nth (nth board i) j) (count (getneighbors board i j isize jsize))))))))

;(println (count results))
;
;(print (= results ["      "
;    " ##   "
;    " #    "
;    "    # "
;    "   ## "
;    "      "]) )

(def gg (fn [a]
  (letfn [
    ( v [n]
  (if (nil? n)
    "X"
    (str
      (v (second n))
      (first n)
      (v (last n)))
    ))]
   (let [
      f  (filter (partial not= \:) (seq (v a))) ]
     (= (reverse f) f)))))

(def tree '(:a (:b nil nil) nil))

(println (gg tree))

