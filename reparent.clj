(def __ (fn [n t] (letfn [
(fin [m n]
  (first (filter #(= (:i %) n) m)))

(pz [r p]
      (map (fn [e]
        (if (coll? e)
          (pz e (first r))
          {:p p :i e :c (map first (rest r))}))
        r))

(rp [l n m]
  (if (not (nil? n))
    (let [
      mn (fin m n)
      pc (keep identity (concat (mn :c) [(mn :p)] )) ]
      (concat [n]
          (keep identity (map (fn [e] (if (not= l e)
                           (rp n e m)))
           pc))))))]
  (let [m (flatten (pz t nil))]
    (rp nil n m)))))

(println

  (= '(n)
     (__ 'n '(n)))

  (= '(a (t (e)))
     (__ 'a '(t (e) (a))))

  (= '(e (t (a)))
     (__ 'e '(a (t (e)))))

  (= '(a (b (c)))
     (__ 'a '(c (b (a)))))

  (= '(d
        (b
          (c)
          (e)
          (a
            (f
              (g)
              (h)))))
    (__ 'd '(a
              (b
                (c)
                (d)
                (e))
              (f
                (g)
                (h)))))

  (= '(c
        (d)
        (e)
        (b
          (f
            (g)
            (h))
          (a
            (i
            (j
              (k)
              (l))
            (m
              (n)
              (o))))))
     (__ 'c '(a
               (b
                 (c
                   (d)
                   (e))
                 (f
                   (g)
                   (h)))
               (i
                 (j
                   (k)
                   (l))
                 (m
                   (n)
                   (o))))))

)
