(defn toints [str]
  (map
    #(Integer/parseInt %)
    (.split (.trim str) ",")))

(defn getindex [row col size]
  (if (or (< row 0) (< col 0) (> row (dec size)) (> col (dec size)))
    nil
    [row col]))

(defn sign [delta]
  (if (< delta 0)
    -1
    1))

(defn getdistance [distancemap size {row :row col :col  value :value} targetrow]
  (if (zero? col)
    (if (= targetrow row)
      value
      (let [sign (sign (- targetrow row))
            index (getindex (+ sign row) col size)]
        (+ value (distancemap index))))
    (let [index1 (getindex (dec row) col size)
          index2 (getindex row (dec col) size)
          index3 (getindex (inc row) col size)
          distances (filter identity (map distancemap [index1 index2 index3]))]
      (if (empty? distances)
        value
        (+ value (apply min distances))))))

(defn buildcells [size intlines]
  (for [row (range 0 size) col (range 0 size)]
    {:row row :col col :value (nth (nth intlines row) col)}))

(defn getdistances [sorted-by-distance size targetrow]
  (reduce
    (fn [distancemap cell]
      (let [distance (getdistance distancemap size cell targetrow)
            index (getindex (cell :row) (cell :col) size)]
        (assoc distancemap index distance)))
    {}
    sorted-by-distance))

(defn diff [a b]
  (let [d (- a b)]
    (if (> d 0)
      d
      (- 0 d))))

(defn distance [leftrow {row :row col :col}]
  (+ (diff leftrow row) col))

(defn getdists [leftrow cells size]
  (let [
    sorted-by-distance (sort-by (partial distance leftrow) cells)
    distances (getdistances sorted-by-distance size leftrow)]
    distances))

(defn mindistance [leftrow cells size]
  (let [
    sorted-by-distance (sort-by (partial distance leftrow) cells)
    distances (getdistances sorted-by-distance size leftrow)]
    (apply min (map #(distances (getindex % (dec size) size)) (range 0 size)))))

(defn testdists [intlines]
  (let [
    size (count (nth intlines 0))
    cells (buildcells size intlines)]
    (getdists 3 cells size)))

(defn test82 [intlines]
  (let [
    size (count (nth intlines 0))
    cells (buildcells size intlines)]
    (for [leftrow (range 0 size) ]
      (mindistance leftrow cells size))))

(def intlines (map toints (.split (slurp "matrix.txt") "\n")))
; (println (test82))
(def intlines
  [[5 5 5 5 5]
   [5 0 0 0 5]
   [5 0 5 0 5]
   [0 0 5 0 5]
   [5 5 5 0 0]])
(def size (count (nth intlines 0)))
(println (testdists intlines))
(println (partition size
  (map second
    (sort-by
      (fn [[[row col] val]] (+ col (* row size))) 
      (testdists intlines)))))

(println (test82 intlines))
