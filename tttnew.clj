(def gg (fn [b pc]
  (letfn  [
    (getwin [b] (some {[:o :o :o] :o [:x :x :x] :x}
        (partition 3
          (map
          (vec (flatten b))
           [0 1 2 3 4 5 6 7 8
        0 3 6 1 4 7 2 5 8
        0 4 8 2 4 6]))))
    (getempt [b] (keep identity (map-indexed (fn [i e] (if (= e :e) i)) b)))
    (repl [b i pc] (concat (take i b) [pc] (drop (inc i) b)))]
    (let [b (flatten b)
          es (getempt b)
      wins (filter (fn [e] (if (= pc (getwin (repl b e pc))) e)) es)]
    (set (map (fn [w] [(quot w 3) (mod w 3)]) wins))))))

(def bd [[:o :e :e]
           [:o :x :o]
           [:x :x :e]])

(println (gg bd :x))

(defn m [f c] (lazy-seq
    (if (seq c)
      (concat [(f (first c))](m f (rest c)) ))))

(println (take 10 (m inc (range))))


