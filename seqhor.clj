
(def gg
(fn [t c]
((fn u [[f & r] cr]
  (lazy-seq
    (cond
       (coll? f)
          (let [uf (u f cr)
                tp (reduce + (flatten uf))]
              (concat [uf] (u r (+ cr tp) )))
       (not (nil? f))
            (if (>= t (+ cr f))
              (cons f (u r (+ cr f)))))))
c 0)))


(println (interpose "," (upto [1 [2 [3 4]] 5 6] 0 12)))
