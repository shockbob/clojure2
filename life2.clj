(fn [b]
  (let [
t \# s \




(letfn [

(q [i a b]
  (and (>= i a)(< i b)))

(g [b i j is js]
  (reduce +
    (map (fn [[ii jj]]
      (if (and (q ii 0 is) (q jj 0 js))
        (( b ii) jj)
        0))
         (map (fn [[di dj]]
      [(+ di i) (+ dj j)]) ds))))

(r [c n]
  ({ [1 3] t
     [1 2] t
     [0 3] t} [c n] s ))]


(let [b (vec (map #(vec (replace {s 0 t 1} %)) b))
      is (count b)
      js (count (b 0))]
      (map (partial apply str)
        (partition js
          (for [i (range is)
                j (range js)]
         (r ((b i) j)
           (g b i j is js)))))))))
