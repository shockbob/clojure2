(defn dn [c n]
  (filter identity
(map #(if (not= (dec n) %2) %) c (cycle (range n)))))


(def gg (fn [e c] (butlast (mapcat (fn [a] [a e]) c))))

(println (gg 0 [1 2 3]))

(def gg (fn [x] (let [
cap (fn [x] (str (.toUpperCase (str (first x))) (apply str (rest x))))
p (vec (.split x "-"))]
  (apply str (concat (first p) (map cap (rest p)))))))

(println (gg "abc"))
