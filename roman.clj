;digit x y z k =
;  [[x],[x,x],[x,x,x],[x,y],[y],[y,x],[y,x,x],[y,x,x,x],[x,z]] !!
;  (fromInteger k - 1)
;
;toRoman :: Integer -> String
;toRoman 0 = ""
;toRoman x | x < 0     = error "Negative roman numeral"
;toRoman x | x >= 1000 = 'M' : toRoman (x - 1000)
;toRoman x | x >= 100  = digit 'C' 'D' 'M' q ++ toRoman r where
;  (q,r) = x `divMod` 100
;toRoman x | x >= 10   = digit 'X' 'L' 'C' q ++ toRoman r where
;  (q,r) = x `divMod` 10
;toRoman x             = digit 'I' 'V' 'X' x

(defn digit [x y z k]
  (
    [[x],[x,x],[x,x,x],[x,y],[y],[y,x],[y,x,x],[y,x,x,x],[x,z]] (dec k)))

(defn divmod [x d]
  [(quot x d) (rem x d)])

(defn toromx [x]
  (cond
    (= x 0) ""
    (< x 0) "ERROR"
    (>= x 1000)  (concat [\M] (toromx (- x 1000)))
    (>= x 100)
       (let [[q r] (divmod x 100)]
       (concat (digit \C \D \M q) (toromx r)))
    (>= x 10)
    (let [[q r] (divmod x 10)]
      (concat (digit \X \L \C q) (toromx r)))
    :else (digit \I \V \X x)))
(defn torom [x] (apply str (toromx x)))
(def answers {
"I"  1 "XXX" 30 "IV" 4 "CXL" 140 "DCCCXXVII"  827 "MMMCMXCIX"  3999 "XLVIII"  48 })
(fn [x]
(letfn [
(todigs [n]
      (map (fn [i] (Character/getNumericValue i))
         (seq (Integer/toString n))))

(toroman [x]
  (let   [tens [\I \X \C \M ]
          fives [\V \L \D]
          ms [[] [1] [1 1] [1 1 1] [1 5] [5] [5 1] [5 1 1] [5 1 1 1] [1 10]]
          digs (todigs x)  ]
     (apply str (flatten (map-indexed (fn [idx dig]
        (let [pow (dec (- (count digs) idx))]
          (map (fn [pd] (cond (= pd 1) (tens pow)
                           (= pd 5) (fives pow)
                           (= pd 10) (tens (inc pow)))) (ms dig)))) digs)))))]
  (toroman x)))
(def gg
(fn [r]
(apply str
  (let   [st (str r) i "IXCM" v "VLD" x "XCM"
          ms [[] [i] [i i] [i i i] [i v] [v] [v i] [v i i] [v i i i] [i x]]]
        (reverse (flatten (map-indexed (fn [pow dig]
          (map #((vec %) pow) (ms (- (int dig) 48))))
             (reverse st))))))))

(def answers {
"I"  1 "XXX" 30 "IV" 4 "CXL" 140 "DCCCXXVII"  827 "MMMCMXCIX"  3999 "XLVIII"  48 })

(println (filter (fn [[rom arabic ans result]] (not result)) (map (fn [[k v]] [k v (gg v) (= k (gg v))]) answers)))



