(defn toints [str]
  (map
    #(Integer/parseInt %)
    (.split (.trim str) ",")))

(defn getindex [i j size]
  (if (or (< i 0) (< j 0) (> i (dec size) (> j (dec size))))
    nil
    (+ j (* i size))))

(defn getdistance [distancemap size {i :i j :j  value :value}]
  (let [index1 (getindex (dec i) j size)
        index2 (getindex i (dec j) size)
        distances (filter identity (map distancemap [index1 index2]))]
    (if (empty? distances)
      value
      (+ value (apply min distances)))))

(defn mapped [size intlines]
  (for [i (range 0 size) j (range 0 size)]
    {:i i :j j :value (nth (nth intlines i) j)}))

(defn getdistances [sorted-by-distance size]
  (reduce
      (fn [distancemap minimap]
        (let [distance (getdistance distancemap size minimap)]
          (assoc distancemap (getindex (minimap :i) (minimap :j) size) distance)))
      {}
      sorted-by-distance))

(defn test83 []
  (let [
    intlines (map toints (.split (slurp "matrix.txt") "\n"))
    size (count (nth intlines 0))
    mapped (mapped size intlines)
    sorted-by-distance (sort-by (fn [{i :i j :j}] (+ i j)) mapped)
    distances (getdistances sorted-by-distance size)]
    (distances (getindex (dec size) (dec size) size))))

(defn conj2 ([coll x] (conj2 coll x []))
  ([ coll x firstpart]
    (if (empty? coll)
      (conj firstpart x)
      (if (> (val (first coll)) (val x))
        (concat firstpart [x] coll)
        (conj2 (rest coll) x (conj firstpart (first coll)))))))


(defn conjme [coll x]
    (let [ {t true f false} (group-by (fn [[k v]] (> (val x) v)) coll)]
         (concat t [x] f)))


(def m (zipmap (take 10000 (iterate inc 1)) (repeatedly #(int (rand 20)))))

(time (def zz (reduce #(conj2 %1 %2) [] m)))
(time (println (count zz)))
(time (println (= (vals zz)(sort (vals zz)))))