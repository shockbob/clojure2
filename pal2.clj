
(defn buildeven [n]
  (let [strn (str n)]
    (Integer/parseInt (str strn (apply str (reverse strn))))))
(defn buildodd [n m]
  (let [strn (str n)]
    (Integer/parseInt (str strn m (apply str (reverse strn))))))
(defn nextpaleven [n]
  (let [countn (count (str n))
        halflen (/ countn 2)
        firsthalf (Integer/parseInt (apply str (take halflen (str n))))
        strhalf (str firsthalf)
        guess1 (buildeven firsthalf)
        guess2 (buildeven (inc firsthalf))

        ]
    (if (> guess1 n)
      guess1
      guess2)))

(defn nextpalodd [n]
  (let [countn (count (str n))
        halflen (quot countn 2)
        firsthalf (Integer/parseInt (apply str (take halflen (str n))))
        midpart (Integer/parseInt (str (nth (str n) halflen)))
        guess1 (buildodd firsthalf midpart)
        guess2 (buildodd firsthalf (inc midpart))
        ]
    (if (> guess1 n)
      guess1
      guess2)))

(defn nextpal [n]
  (let [strn (str n)
        nextn (if (even? (count strn))
                   (nextpaleven n)
                    (nextpalodd n))
        nextn (if (*= (count (str n)))))

(println (map nextpalodd [135 120 321 446] ))
