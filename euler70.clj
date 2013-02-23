(ns com.res.test
    ;(:import )
    ;(:require )
)
(defn isprime? [n]
 (if (= n 2)
   true
 (if (even? n)
   false
   (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n)))))))

(defn primefactors [n primes])
(defn primefactors [n primes]
  (filter #(zero? (mod n %)) primes))

(defn mults [n]
  (reduce #(* %1 (/ (dec %2) %2)) 1 n))


(def bignum 10000000)
(def primes
  (filter isprime? (range 2 (/ bignum 2))))

(defn myprimes [n]
  (take-while (partial > (/ n 2)) primes))

(defn phi [n]
  (if (isprime? n)
    1
   (* n (mults (primefactors n (myprimes n))))))

(defn sorted [a]
  (sort (seq (str a))))


(defn permut [a b] (= (sorted a) (sorted b)))

(def biglist (filter
      (fn [i]  (permut i (phi i))) (range 2 bignum)))


;(def biglist (for [i (range 1 bignum)
;      :let [myprimes (take-while (partial > (inc (Math/sqrt i))) primes)
;           phii (phi i myprimes)]
;      :when (if (isprime? i) false (permut i (phi i primes)))
;      ] [(/ (phi i primes) i) i] ))

(println (sort-by first (map (fn [x] [(/ (phi x) x) x]) biglist)))

(println (phi 21))
;(println (sort-by (fn [a b] (first a) < (first b)) biglist))
