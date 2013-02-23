(defn rot [i c]
  (let [
    f (if (neg? i) #(concat [(last %)] (butlast %)) #(concat (rest %) [(first %)]))
    i (Math/abs i)]
  (reduce (fn [a b] (f a)) c (range 0 i))))
(def gg (fn [i c]
  (let [
    f (if (neg? i) #(concat [(last %)] (butlast %)) #(concat (rest %) [(first %)]))]
  (last (take (inc (Math/abs i)) (iterate f c))))))
(println (gg 6 [1 2 3 4 5])) ;'(2 3 4 5 1))

(defn ss [i c]
  (let [t (count c)
        [l r] (split-at (mod i t) (range t))]
    (map #((vec c) %) (concat r l))))

(defn ss [i c]
  (let [t (count c)
        i (mod i t)]
    (map #((vec c) %) (concat (range i t)(range 0 i)))))

(println (ss 6 [1 2 3 4 5]))



