(def float-array-class (class (float-array 1)))
(defprotocol Matrix
  (matrix-data [x])
  (matrix-height [x])
  (matrix-width [x])
  (matrix-add [x] [x y] [x y & args])
  (get-cell [x r] [x r c]))

(deftype Matrix2D [data height width]
  Matrix
  (matrix-data [x] data)
  (matrix-height [x] height)
  (matrix-width [x] width)
  (matrix-add [x] x)
  (matrix-add [x y]
              (let [x-data (matrix-data x)
                    y-data (matrix-data y)
                    z-data (amap ^floats x-data index z-data (+ (aget
^floats x-data index) (aget ^floats y-data index)))]
                (Matrix2D. z-data (matrix-width x) (matrix-height
x))))
  (matrix-add [x y & args] (reduce matrix-add (matrix-add x y) args))
  (get-cell [x r] (get-cell x r 0))
  (get-cell [x r c] 1))

(defn matrix [data height width]
  (if (instance? float-array-class data)
    (Matrix2D. data height width)
    (Matrix2D. (float-array data) height width)))

(def one (matrix (range 10) 5 2))
(def two (matrix-add one one))

(doseq [r (range (matrix-height one))]
  (doseq [c (range (matrix-width one))]
    (print (get-cell one r c) " "))
  (println))

(doseq [r (range (matrix-height two))]
  (doseq [c (range (matrix-width two))]
    (print (get-cell two r c) " "))
  (println))

