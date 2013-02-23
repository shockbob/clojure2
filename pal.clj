
(defn make10 [n]
  (Math/pow 10 n))

(defn nextpal [n]
    (if (even? (count (str n)))
      (let [firsthalf (.substring  (/ countstrn 2) strn) )
            i (Integer/parseInt r)
            cb (count (str i))
            i (inc i)
            s (str i)]
        (if (> (count s) cb)
          (nextpal (make10  c))
          (Integer/parseInt (str s (apply str (reverse s))))))
      (let [half (quot c 2)
            r (apply str (take half s))
            mid (Character/getNumericValue (nth s half))
            rev (apply str (reverse r))
            try (Integer/parseInt (str r mid rev) )
            try2 (Integer/parseInt (str r (inc mid) rev))
;            x (doall (println r mid try try2))
            ]
           (if (> try n)
             try
             try2))
      )))

(defn ispal [n]
  (= (reverse (str n)) (seq (str n))))

;(println (filter ispal [121 132 144 111 222]))
;
;(println (take 5 (filter ispal (iterate inc 135))))
;
;(println (take 5 (iterate nextpal 135)))
;
(println (nextpal 99))

(println (make10 2))
