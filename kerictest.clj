(require 'clojure.contrib.combinatorics)
(use 'clojure.contrib.combinatorics)

;(def combos [0 1 2 2 3 4 0 3 6 4 5 6])
;
;(defn match15 [perms]
;  (every? #(= 15 (apply + %)) (partition 3 (map perms combos))))
;
;(println (filter match15 (map vec (permutations (range 2 9)))))

(defn letterval [l]
  (let [x (inc (- (int l) (int \A)))] (max 0 x)))

(defn wordval [st]
  (apply + (map letterval st)))  

(defn euler22 []
  (let [strs (slurp "C:\\Users\\BobShock\\names.txt")
        strseq (sort (seq (.split strs ",")))]
    (apply + (map * (iterate inc 1) (map wordval strseq)))))

(println (time (euler22)))

(def fibs (lazy-cat '(0 1) (map + fibs (drop 1 fibs))))

(defn euler25 []
  (count (take-while #(< (count (str %)) 1000) fibs)))

(defn digs [n]
  (if (< n 10)
    [n]
    (concat (digs (int (/ n 10))) [(rem n 10)] )))

(defn fact [n] (apply * (range 2 (inc n))))
(defn sumfacts [n] (apply + (map fact (digs n))))
(defn euler34 [] (filter #(= (sumfacts %) %) (range 3 50000)))

(println (euler34) )
