
(defn isprime? [n]
 (if (= n 2)
   true
 (if (even? n)
   false
   (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n)))))))

(defn square2 [x] (* 2 (* x x )))
(def oddcomposites (filter (fn [x] (not (isprime? x))) (iterate (fn [x] (+ x 2)) 1)))

(defn solve46 []
  (let [primes (filter isprime? (range 2 10000))
        squares (map square2 (range 1 1000))
        primexsqrs  (for [p primes s squares] (+ p s))
        prods (apply hash-set (distinct primexsqrs))]
    (first (remove prods oddcomposites))))

(println (solve46))

