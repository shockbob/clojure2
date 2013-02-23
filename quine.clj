(def x ((fn [x]
       (str x (list (quote quote) x)))
      (quote
         (fn [x]
           (str x (list (quote quote) x))))))


(def y  (str '((fn [x]
       (str x (list (quote quote) x)))
      (quote
         (fn [x]
           (str x (list (quote quote) x)))))))

(println x (class x))
(println y (class y))
