(defmulti tos :Ob)
(defn line [p1 p2] {:Ob :line :p1 p1 :p2 p2})
(defn circle [cent rad] {:Ob :circle :cent cent :rad rad})
(defmethod tos :line [l] (str "Line:" (l :p1) (l :p2)))
(defmethod tos :circle [c] (str "Circle:" (c :cent) (c :rad)))
(println (tos (circle [2 3] 3.3)))
(println (tos (line [1 1][0 0])))
(println ((get-method tos :line) (line [1 2][3 4]) ))






