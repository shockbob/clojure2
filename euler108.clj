(def maxsize (range 2 400))

(defn getrange [n] (range (inc n) (inc (* 2 n))))

(defn isinvert [x] (= (long (/ x)) (/ x)))

(defn getgoodstuff [n]
  (let [ni (/ n)]
    ((comp count filter) (fn [x] (isinvert (- ni (/ x)))) (getrange n))))

;(println (getgoodstuff (apply * (range 1 11))))
(println (take 10 (reverse (sort-by first (map (fn [x] [(getgoodstuff x) x]) (range 2 2000))))))



