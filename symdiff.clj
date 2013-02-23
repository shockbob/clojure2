(def gg (fn [a b] (set (map first  (filter (fn [x] (= 1 (count x)))
  (partition-by identity (sort (concat (vec a) (vec b)))))))) )

(def hh (fn [a b]
      (set (filter #(= [nil %] (sort [(a %)(b %)])) (concat a b)))))

(println (hh #{1 2 3 4 5 6} #{1 3 5 7}))


(def ii (fn [a b]
  (set (remove #(not (a %)) b))))

(println (ii #{1 2 3} #{2 3 4}))

;{:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4])

(defn lastprob [x] (let [
hm (apply hash-map (partition-by keyword?  x))
m (reduce (fn [m [k v]] (assoc m (last k) v)) {} hm)
e (mapcat butlast (filter #(> (count %) 1) (keys hm)))
]
  (reduce (fn [m1 e1] (assoc m1 e1 [])) m e)))


(println (lastprob [:a 1 2 3 :b :c 4 5 6 :e :d 3 2 :f 4]))






