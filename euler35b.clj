(defn isprime? [n]
 (if (= n 2)
   true
 (if (even? n)
   false
   (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n)))))))

(defn toint [seqn]
  (reduce (fn [sum value] (+ value (* sum 10))) 0 seqn))

(defn todigs [n] (map (fn [c] (Character/getNumericValue c)) (str n)))

(defn getrots [seqn]
  (take (count seqn)
    (iterate
      (fn [d] (conj (rest d) (first d)))
      seqn)))

(defn getrots [n]
  (let [digs (todigs n)]
    (map toint (getrots digs))))

(println (getrots [1 2 3]))
(println (toint [1 2 3 5]))


