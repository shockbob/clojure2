(d\efn sieve [isprime n max]
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
    (* n (apply * (map (fn [x] (/ (dec x) x)) factors))))

(defn primefactors [n primes]
  (mapcat (fn [k]
    (let [modn (mod n k)
          divn (/ n k)]
      (if (and (zero? modn)(primes k true))
        (if (and (not= k divn)(primes divn true))
          [k divn]
          [k])
        )))
    (range 2 (inc (int (Math/sqrt n))))))


(defn phi [n primes]

  (if (primes n true)
   (dec n)
   (mults n (primefactors n primes))))

(println (phi 36 (allsieve 60)))