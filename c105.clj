(def gg (fn [x y]
  (letfn [
    (f [current level value]
    (if ((set current) value)
      level
      (let [
        n (filter identity
          (mapcat
            (fn [i] [(if (even? i) (/ i 2) nil) (+ i 2) (* i 2)])
            current))]
        (f n (inc level) value))))]
    (f [x] 1 y))))

   (def gg
#(inc (count (take-while (fn [v] (nil? ((set v) %2)))
      (iterate (fn [v]
                (mapcat
                      (fn [z] (flatten [ (if (even? z) (/ z 2) []) (+ z 2) (* z 2)])) v)) [%])))))

(println (gg 1 1))

(println (gg 9 12))
