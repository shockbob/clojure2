(def gg (fn [a b]
(letfn [
(ds [a b]
  (reduce + (map #(if (= % %2) 0 1) a b)))
(dd [s b]
  (let [g (doall (println s b))]
    (if (and s b)
      (if (empty? b)
        (count s)
        (if (= (first s)(first b))
          (dd (rest s)(rest b))
          (inc (dd s (rest b)))))
      (if  (empty? s)
        (count b)))))

(d [a b]
  (if (= (count a)(count b))
    (ds a b)
    (if (> (count a)(count b))
      (dd (vec b) (vec a))
      (dd (vec a) (vec b) ))))]

(d a b))))


(println (gg "kitten" "Sitting" ))

