(def gg (fn [a]
  (letfn [
     (v [n]
  (if (seq n)
    (vec [(v (second n)) (first n) (v (last n))])))]
     (let [
       f (v a) ]
     f))))

(def xx (gg [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]]))
(println xx)
(println (= (flatten xx) (reverse (flatten xx))))


(def gg #(letfn [
  (v [t i]
  (if (seq t)
    (+ ((first t) i) (min (v (rest t) (inc i) )
(v (rest t) i )))
    0
    ))]
  (v % 0)) )

(println (gg [     [3]
            [2 4]
           [1 9 3]
          [9 9 2 4]
         [4 6 6 7 8]
        [5 7 3 5 1 4]]))



#(letfn [
  (v [t i l]
  (if (= l (count t))
    0
    (+ ((t l) i) (min (v t (inc i) (inc l))
(v t i (inc l))))))]
  (v % 0 0))