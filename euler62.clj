(defn sorted [n] (sort (str n)))

(defn cubed [x] (* x x x))

(defn solve62 []
  (let [cubes (map cubed (range 1 10000))
        groupedbysorted (group-by sorted cubes)
        contains5 (filter (fn [val] (= (count val) 5)) (vals groupedbysorted))]
    (apply min (apply concat contains5))))

(println (solve62))



