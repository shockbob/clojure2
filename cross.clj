
(def gg (fn [w c]  (letfn [
(s [c]
   (mapcat #(partition-by #{\#} %)
     (map #(remove #{\space} %) c)))
(v [c] (map
  #(map (fn [w] (nth w %)) c) (range (count (nth c 0)))))
(m [s w]
  (and (= (count s)(count w)
         (count
           (filter
             (fn [[sc wc]] (or (= sc wc) (= wc \_)))
             (partition 2 (interleave s w)))))))]
    (not (not-any? #(m % w)
      (concat (s c) (s (v c))))))))


(println (gg "joy" ["c _ _ _"
                    "d _ # e"
                    "r y _ _"]))



