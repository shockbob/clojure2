(defn allss [x]
  (map (fn [n] [(first n) (last n)])
      (mapcat
        #(butlast (take-while seq (iterate butlast %)))
        (butlast (take-while seq (iterate rest x))))))

(def gg (fn [cl]
  (letfn [
(fin [c cl]
    (first (filter (fn [x] (= (last c) (first x))) cl)))

(fc [s cl]
  (loop [cu s ch [s]]
     (let [n (fin cu cl)]
       (if (nil? n)
         ch
         (recur n (concat ch [(last n)]))))))
(gi [n] (for [i (range n) j (range (inc i) n)] [i j]))
(gs [n]
  (map (fn [[i j]] [(n i) (n j)]) (gi (count n)))) ]
  (set (into #{} (mapcat (fn [x] (gs (vec x)))
    (map flatten (map (fn [x] (fc x cl)) cl))))))))

(def coll [["cat" "man"] ["man" "snake"] ["spider" "cat"]] )

;(def zz (fn [coll] (letfn [
(defn fc [cl]
  (map (fn [s]   (reduce (fn [coll [f l]] (if (= (last s) f) (concat coll [l]) coll))
        s coll)) cl))
(defn allss [x] (let [ct (count x) x (vec x)]
  (for [
    i (range ct)
    j (range (inc i) ct)
   ] [(x i)(x j)])))

(defn zz [coll] (into #{} (mapcat (fn [e] (let [fce (distinct (fc e coll))]
              (if (seq fce)
                  (allss (concat [(first e)] fce))
                  [e]))) coll)))

(println (into #{} (mapcat allss (fc coll))))
;(println (zz coll))
(println (gg coll))

(println ((fn [x y] (+ (Math/abs (- (count x)(count y)))
  (reduce + (map (fn [a b] (if (= a b) 0 1)) x y)))) "kitten" "sitting") )

(def gg (fn [& x]
  (= #{true false} (into #{} x))))

(println (gg true true false true))