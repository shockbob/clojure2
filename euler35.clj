(require 'clojure.contrib.combinatorics)
(use 'clojure.contrib.combinatorics)

(defn digs [n]
  (if (< n 10)
    [n]
    (concat (digs (int (/ n 10))) [(rem n 10)] )))

(defn isprime? [n]
 (if (= n 1)
   false
   (if (= n 2)
   true
 (if (even? n)
   false
   (not-any? #(zero? (mod n %)) (range 3 (inc (Math/sqrt n))))))))

(defn getint [digs]
  (reduce #(+ (* 10 %1) %2) 0 digs))

(defn digs [n]
  (if (< n 10)
    [n]
    (concat (digs (int (/ n 10))) [(rem n 10)] )))

(defn rot [l]
  (concat (rest l) [(first l)]))

(defn getrots [digs]
  (take (count digs) (iterate rot digs)))

(defn rotprimes [x]
  (let [digsx (digs x)]
    (and (every? odd? digsx)
      (every? isprime? (map getint (getrots digsx))))))

(defn euler35 []
  (inc (count (filter rotprimes (range 3 1000000)))))

(println (time (euler35)))

(def primes (filter isprime? (range 1000 10000)))
(defn permut [a b]
    (if (not= a b)
      (= (sort (digs a)) (sort (digs b)))
      false))

;(println (permut 1234 3214))
;(println (permut 123 322))

(def groups
  (filter #(= 3 (count %)) (map (fn [outer] (filter #(permut outer %) primes)) primes)))

(def sorted (map sort groups))

(defn goodset [n]
  (= (- (nth n 2) (nth n 1)) (- (nth n 1) (nth n 0))))

(defn euler49 [] (filter goodset sorted))

 (defn sqdigs [n]
   (apply + (map #(* % %) (digs n))))

(println (sqdigs 44))

(defn lastdigit [n]
  (first (drop-while #(not (or (= 89 %) (= 1 %))) (iterate sqdigs n))))

(defn lastdig [n]
  ( if (or (= n 89)(= n 1))
    n
    (lastdig (sqdigs n))))

(defn euler92 []
  (count 
    (filter #(= 89 (lastdig %)) (range 1 10000001))))


(defn getparts [n]
  (let [digsn (digs n)]
    (concat (take (count digsn) (iterate rest digsn))
      (map reverse (take (count digsn) (iterate rest (reverse digsn)))))))

(defn allparts [n] (distinct (map getint (getparts n))))

(defn euler37 []
  (apply + (take 11
    (filter
      #(and (isprime? %) (every? isprime? (allparts %)))
      (iterate inc 10)))))

(def good9s (filter #(= 4 (count (distinct (seq (str %))))) (range 9000 9999)))


(defn match9s [n m] (= (range 1 10) (sort (concat (digs n) (digs m)))))

(defn getgood [] (filter (fn [x] (match9s x (* x 2))) good9s))

(defn euler38 [] (let [g (apply max (getgood))] (apply str [g (* g 2)])))

(println (euler38))
(println (match9s 123 456789))

(defn get3 [n start] (map #(nth n (+ start %)) [0 1 2]))

(def allperms (map getint (filter #(not= 0 (first %)) (permutations  (range 0 10)))))

(def divs [2 3 5 7 11 13 17])

(defn goodmatch43 [n]
  (let [dig (digs n)]
     (every? zero? (map #(mod (getint (get3 dig %1)) %2) (range 1 8) divs))))

(println (goodmatch43 1406357289))

(def euler43 (filter goodmatch43 allperms))
;(println (apply +  euler43))

(defn pentagonal [n]
  (/ (* n (- (* 3 n) 1)) 2) )

(def pents (map pentagonal (range 1 1000)))

(defn ispent? [n] (not (nil? (some #{n} pents))))

(defn x [m] (/ (+ 0.5 (Math/sqrt (- 0.25 ( * 4 1.5 (-  0 m))))) 3.0))

(defn ispent? [m] (let [pm (x m)] (= pm (int pm))))

(defn bothpents [n m]
  (let [mx (max m n) mn (min m n)]
    (and (ispent? (+ m n)) (ispent? (- mx mn)))))




(def sumpents
  (for [ i (range 0 (dec (count pents)))  j (range (inc i) (dec (count pents)))
    :when (bothpents (nth pents i) (nth pents j))] [(nth pents i) (nth pents j)]))

(println (ispent? 12))



;(println sumpents)


(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(println (gcd 12 4))

(defn phi [n]
  (count (filter (fn [x] (= 1 (gcd n x))) (range 1 n))))

(println (phi 9))

(defn euler70check [i] (permut i (phi i)))

(println (for [i (range 2 1000) :when (euler70check i)] i))

(println (phi 87109))
(println (euler70check 87109))

