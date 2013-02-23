
(defn topercent [freqmap]
     (let [len (apply + (vals freqmap))]
       (reduce (fn [map [k v]] (assoc map k (/ v (double len)))) {} freqmap)))

(def s4 (range 1 5))
(def all4sides  (for
      [a s4 b s4 c s4 d s4 e s4 f s4 g s4 h s4 i s4] (+ a b c d e f g h i)))

(def perc4 (topercent (frequencies all4sides)))

(def s6 (range 1 7))

(def all6sides (for [a s6 b s6 c s6 d s6 e s6 f s6] (+ a b c d e f)))

(def perc6 (topercent (frequencies all6sides)))

(def allgames (range 9 37))

(defn probless [perc value]
  (apply + (filter identity (map perc (range 1 value)))))

(println (apply + (map (fn [value] (* (probless perc6 value)(perc4 value))) allgames)) )