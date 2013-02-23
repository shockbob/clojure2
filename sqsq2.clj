(def __ (fn [a b]
(let [pats { 1 [0] 2 [1 5 7 3] 3 [12 18 22 16 10 6 2 8 14]
            4 [17 25 31 23 15 9 3 11 19 27 33 39 45 37 29 21]}
      sqs (take-while (fn [x] (<= x b)) (iterate (fn [x] (* x x)) a))
      sqs (apply str sqs)
      sqlen (int (Math/ceil (Math/sqrt (count sqs))))
      padstr (repeat (- (* sqlen sqlen) (count sqs)) \*)
      sqs (str sqs (apply str padstr))
      sqmap (zipmap (pats sqlen) sqs)
      fullsize (+ (dec sqlen) sqlen)
      newstr (map (fn [i] (sqmap i \space)) (range (* fullsize fullsize)))]
      (map (partial apply str) (partition fullsize newstr)))))

(println
(= (__ 2 2) ["2"])

(= (__ 2 4) [" 2 "
             "* 4"
             " * "])

(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])

(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])

(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])

(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])
  )

