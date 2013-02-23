
(fn [e] (letfn [
(el [l ev mp]
  (map (fn [f]
     (if (seq? f) (ev f mp) f))  l))

(ef [[f & r] mp]
  (let [op ({'/ / '+ + '* * '- -} f)
        r (replace mp r)]
      (apply op (el r ef mp))))]
  (fn [mp] (ef e mp))))


;(defn evl [l]
;  (reduce (fn [e] (if (isop e)
;                      (doop e (rest l))
;    ))))
