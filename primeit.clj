
(defn isprime? [n]
 (if (= n 1)
   false
   (if (= n 2)
   true
 (if (even? n)
   false
   (not-any? #(zero? (mod n %)) (range 3 (inc (Math/sqrt n))))))))


(defn rev[x] (apply str (reverse (str x))))

(defn todigs [n] (map #(Character/getNumericValue %) (str n)))

(defn rev [n] (apply str (reverse n)))

(defn revint [x] (Integer/parseInt (rev (str x))))

(defn oddbutnot5 [n] (and (odd? n) (not= 5 n)))
(defn allodds [n] (every? oddbutnot5 (todigs n)))

(def revs (filter #(and (isprime? %) (isprime? (revint %))) (range 10000 99999)))

;(println (take 10 revs))
(println (todigs 1234112))
(def bottomandtop (filter allodds revs))

(println (count bottomandtop))
