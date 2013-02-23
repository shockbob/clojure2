
(defn cvalue [i]
  (if (zero? (mod i 3))
    (/ (* 2 i) 3)
    1))

(defn nextvalue [[n d i]]
  (let  [c (cvalue i)]
    [d (+ n (* c d)) (dec i)]
  ))

(defn todigs [n] (map #(Character/getNumericValue %) (str n)))

(def lastthing (take-while (fn [[n d i]] (>= i -1)) (iterate nextvalue [2 1 100])))

(println (todigs 1233223))

(println (apply + (todigs (first (last lastthing)))))