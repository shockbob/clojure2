(require 'clojure.contrib.combinatorics)
(use 'clojure.contrib.combinatorics)

(defn get4digit [func]
  (filter
    (fn [n] (> n 999))
    (take-while (fn [n] (<= n 9999)) (map func (iterate inc 1)))))

(def sqfunc (fn [n] (* n n)))
(def trifunc (fn [n] (/ (* n (inc n)) 2)))
(def pentfunc (fn [n] ( / (* n (dec (* 3 n))) 2)))
(def hexfunc (fn [n] ( * n (dec (* 2 n)))))
(def heptfunc (fn [n] (/ (* n (- (* 5 n) 3)) 2)))
(def optfunc (fn [n] (* n (- (* 3 n) 2))))

(defn last2 [n] (mod n 100))
(defn first2 [n] (int (/ n 100)))

(def funcvec (distinct [trifunc pentfunc sqfunc hexfunc heptfunc optfunc]))

(def allfuncs
  (map get4digit funcvec))


(def perfunc (permutations (range 0 (count funcvec))))

(defn rots [n]
  (take (count n) (iterate (fn [x] (concat (rest x) [(first x)])) n)))

(defn sortedrot [n] (first (sort-by first (rots n))))

(def perm (permutations (range 0 6)))
(def groups (distinct (map sortedrot perm)))

(defn addem [values singlevalue]
    (reduce
      (fn [coll v]
        (if (= (last2 (last v))(first2 singlevalue))
          (concat coll [v] [(concat v [singlevalue])])
          (concat coll [v])))
      []
      values))

(defn addcoll [funcvec coll]
  (let [size (inc (count (first coll)))
     newcoll (reduce (fn [coll node] (addem coll node)) coll funcvec)]
    (filter (fn [subcol] (= size (count subcol))) newcoll)))


(defn testgroup [group]
  (let [restof (rest group)
        startcoll (map (fn [x] [x]) (first group))]
  (reduce (fn [coll funcvec] (addcoll funcvec coll)) startcoll restof)))


(defn testresults [n]
  (= (last2 (last n)) (first2 (first n))))

(defn testwhichgroup [index]
 (let [mapper (map (fn [gpno] (nth allfuncs gpno)) (nth groups index))
       groupres (testgroup mapper)]
   (filter testresults groupres)))

(println (remove empty? (map (fn [index] (testwhichgroup index)) (range 0 (count groups)))))