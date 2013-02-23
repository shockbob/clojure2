(def s (slurp "c:\\users\\bobshock\\cipher1.txt"))

(def crypttext (map #(Integer/parseInt %) (.split s ",")))

(defn decrypt [text key]
  (map #(bit-xor (nth text %) (nth key (mod % (count key))))
    (range 0 (count text))))

(def aInt (int \a))
(def zInt (int \z))
(def allchars (range aInt (inc zInt)))

(defn toStr [n]
  (apply str (map char n)))

(def passwords (for [i allchars j allchars k allchars] [i j k]))

(def decrypted
  (for [password passwords]
    [password (toStr (decrypt crypttext password))]))

(def goodtext
  (filter
    (fn [text] (> (.indexOf (second text) "the") 0))
    decrypted))

;(println (interpose \newline goodtext))
;(print (toStr [48 49 50 ]))

(def password [103 111 100])

(println (apply +  (decrypt crypttext password)))
