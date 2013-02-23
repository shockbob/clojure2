(defn sieve [isprime n max]
  (reduce (fn [map n] (assoc map n false)) isprime (range (+ n n) max n)))

(defn allsieve [max]
  (reduce
    (fn [map n]
      (if (get map n true)
        (sieve map n max)
        map))
      (sorted-map)
      (range 2 max)))

(defn mults [n factors]
  (let [num (apply * (map dec factors))
        denom (apply * factors)]
    (/ (* n num) denom)))

(defn primefactors [n primes]
  (mapcat (fn [k]
    (let [modn (mod n k)
          divn (/ n k)]
      (if (and (zero? modn)(primes k true))
        (if (and (not= k divn)(primes divn true))
          [k divn]
          [k])
        )))
    (range 2 (inc (Math/sqrt n)))))


(defn phi [n primes]
  (if (primes n true)
    1
   (mults n (primefactors n primes))))

(defn sorted [a]
  (sort  (str a)))


(defn permut [a b] (= (sorted a) (sorted b)))
(def big (* 1000 1000 ))
(time (def primes (allsieve big)))
(println (phi 21 primes))
;(def phis (filter (fn [n] (permut n (phi n primes))) (range 2 big)))
;(def mapped (map (fn [n] [ n  (/ n (phi n primes))] ) phis))
;(println (first (sort-by second mapped)))
(defn phis [] (reduce
  (fn [[minof minn] n]
    (let [phin (phi n primes)]
      (if (permut n phin)
        (let [divn (/ n phin)]
            (if (< divn minof)
              [divn n]
              [minof minn]))
        [minof minn])))
    [999 nil]
    (range 2 big)))

(time (println (phis)))

