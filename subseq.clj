
(defn gt [x]
(let [
  ss (mapcat #(take-while seq (iterate butlast %)) (take-while seq (iterate rest x)))
  ss (filter #(and (apply < %) (> (count %) 1)) ss)
  ss (reduce #(if (> (count %2) (count %)) %2 %) [] ss)]
ss))

(println (gt [5 6 1 3 2 7]))

#(let [cv (count %)]
    (reduce (fn [a i]
      (reduce (fn [a j]
        (let [k (subvec (vec %) i j)]
          (if (and (apply < k)(> (count k) (count a)))
            k
            a)))
            a
            (range (+ 2 i) (+ 1 cv))))
        []
        (range cv)))