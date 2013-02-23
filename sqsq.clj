(defn nearsq [n]
  (first (drop-while
    (fn [x] (> n (* x x)))
      (iterate inc 0))))

(defn dosq [start end]
  (take-while
    (fn [x] (<= x end))
    (iterate (fn [x] (* x x)) start)))

(defn rot [[i j]] [j (- 0 i)])

(defn dirs [[di dj]] (take 4 (iterate rot [di dj])))

(defn pad [n x] (apply str (concat x (repeat (- n (count x)) "*") )))

(defn __ [a b]
  (if (= a b)
    [(str a)]
  (let [
    st (apply str (dosq a b))
    len (nearsq (count st))
    padded (pad (* len len) st)
    sqsize (dec (* 2 len))
    sj (quot sqsize 2)
    [di dj] [ 1 1]
    si (if (even? len) (dec sj) sj)
    start (if (even? len) 2 1)
    inds (getinds [di dj] [si sj] (range start len 2))
    [i j] (last inds)
    [ni nj] [(- i di)(- j dj)]
    zipped (zipmap inds padded)
    result (for [i (range sqsize)]
              (apply str (for [j (range sqsize)]
                          (zipped [i j] \space))))
    ]
    [inds [ni nj] result])))

(println (__ 2 4))
(println (dirs [1 1]))
(println
(= (__ 2 2) ["2"])

(= (__ 2 4) [" 2 "
             "* 4"
             " * "])

(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])

(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])

(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])

(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])
)
