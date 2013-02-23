

(def x (slurp "matrix.txt"))

(def lines (map #(.trim % ) (seq (.split x "\n"))))


(println (first lines))
(println (first (map int (vec (.split (first lines) ",")))))
(println (first intlines))
