
(def words (vec (remove empty? (seq (.split (slurp "words.txt") "[\",]")))))
;(def words ["abc" "def"])
(defn isana [a b]
  (and (not= a b)(= (sort a)(sort b))))

(defn getanagrams [words]
  (let [len (count words)]
    (for [a (range 0 len) b (range (inc a) len) :when (isana (nth words a)(nth words b))]
      [(nth words a)(nth words b)])))

(defn perfectsq [x] (let [sq (Math/sqrt x)] (= x (int x))))



(defn hasdups [a] (not= (sort a) (sort (distinct a))))

(def anas (getanagrams words))

(def dups (filter hasdups (map first anas)))

(println (frequencies (map count (map first anas))))

(def groups (group-by (comp count first) anas))

(println (groups 9))

(defn sqrt [n] (int (Math/sqrt n)))

(def foursqs (map (fn [x] (str (* x x))) (range (sqrt 1000) (sqrt 9999))))

(println (getanagrams foursqs))

(defn getpattern [a]
  (let [mapped (zipmap (iterate inc 0)(str a))
        nextmap (reduce (fn [mymap [k v]] (assoc mymap v (conj (get mymap v []) k))) (sorted-map) mapped)
    ] nextmap))


(defn matcher [a mapa b mapb]
  (if (empty? a)
    true
    (if (= (mapa (first a)) (mapb (first b)))
      (matcher (rest a) mapa (rest b) mapb)
      false)))



(defn patsmatch [astr bstr cval dval]
  (let [pata (getpattern astr)
        patb (getpattern bstr)
        patc (getpattern cval)
        patd (getpattern dval)]
    ))

(println (matcher "care" (getpattern "care") "1296" (getpattern 1296)))

(println (patsmatch "care" "race" 1296 9216))
(println dups)


