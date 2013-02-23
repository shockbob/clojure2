


(def gg (fn [n c]
  (first (reduce (fn [[r p] e] (let [p (conj p e)]
    (if (= n (count p))
      [(conj r p) []]
      [r p]))) [[] []] c))))

(def gg (fn [x]
   (letfn [(isbin [x]
      (and x
      (= 3 (count x))
      (every? isbin (keep identity (rest x)))))]
     (isbin x))))

(println (gg '(:a (:b nil nil) nil)))

;(println (gg 3 [1 2 3 4 5 6 7]))

(def gg (fn [a b] (letfn [(it [a b]
   (if (or (empty? a)(empty? b))
     nil
     (concat [(first a)(first b)] (it (rest a)(rest b)) )))]
  (it a b))))

(println (gg [1 2 3 8][4 5 6]))