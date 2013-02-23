

(def graph #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]})
(def graph #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]})
(defn nodes [graph]
  (distinct (flatten (vec graph))))

(defn findconnectednodes [node graph nodes]
   (concat (map first (filter (fn [connector] (= node (last connector))) graph))
     (map last (filter (fn [connector] (= node (first connector))) graph))))

(defn getallconnected [node graph nodes]
  (letfn [(doit [current found]
    (if (contains? found current)
       found
       (let [
         connected (findconnectednodes current graph nodes)
         newfound (set (conj found current))
         ]
         (set (mapcat
           (fn [connect]
             (doit connect newfound)) connected)))))]
    (doit node #{})))



;(println (findconnectednodes 1 graph (nodes graph)))

;(println (getallconnected 4 graph (nodes graph))



(def gg (fn [& funcs]
  (fn [& args] (map (fn [func] (apply func args)) funcs))))

;(println ((gg + max min) 2 3 5 1 6 4))


 (def gg (fn [& funcs](
    fn [& args]
      (let [x (apply (first funcs) args)]
         (reduce (fn [orig func]
          (func orig)) x (rest funcs))))))


(def gg (fn [x]
  (let [ft (str (first (str x)))]
     (cond (= ft "{") :map
           (= ft "[") :vector
           (= ft "#") :set
           :else :list))))



(defn nextindex [n]
  (if (empty? n)
    nil
   (let [z (mod (inc (first n)) (count n))]
     (if (zero? z)
       (cons 0 (nextindex (rest n)))
       (cons z (rest n))))))

(defn validlevel [n]
  (if (= 1 (count n))
    true
    (let [ diff (- (second n) (first n))]
      (if (or (= -1 diff)(zero? diff))
         (validlevel (rest n))
          false))))


(defn levels [n]
  (filter validlevel (let [ct (reduce * (range 1 (inc n)))]
    (take ct (iterate nextindex (repeat n 0))))))

(defn todigs [base x]
  (map
       (fn [x] (Character/getNumericValue (char x)))
           (vec (Integer/toString x base))))

(defn getcombos [length numdigs]
   (take-while (fn [x] (<= (count x) length))

          (map (partial todigs numdigs)
               (range 0 100))))

(defn pad [length vector]
  (if (>= (count vector) length)
    vector
    (flatten [(repeat (- length (count vector)) 0) vector])))

;(println (filter (fn [x] (= (count x) (count (distinct x))))
;  (map (fn [x] (pad x 2)) (getcombos 4 1))))

;(defn distance [a b]
;  (if (= (count a)(count b))
;    (- (count a) (count (filter (fn [[x y]] (= x y)) (zipmap a b))))
;    ) )
;
;(println (distance "cat" "ace"))

(println (pad 5 [1 2 3]))
(defn getstuff [numdigs setsize] (set (map set (take-while (fn [x] (= (count x) numdigs))
  (filter (partial apply distinct?)
    (map (partial pad numdigs)
      (map (partial todigs setsize) (iterate inc 0))))))))

(defn powerset [setxx] (let [
  myset setxx
  sets (reduce (fn [s d](concat (getstuff d (count myset)) s)) #{} (range 1 (count myset)))
  stuff (map set (map (fn [s] (map (vec myset) s)) sets))]
  (set [ #{} setxx stuff])))





;(println (xx #{1 :a}))

(defn diffsame [a b]
    (if (not= (count a) (count b))
      -999
      (count (filter (fn [[x y]] (not= x y)) (zipmap a b)))))

(defn diffdiffx [small big]
  (if (= (first small)(first big))
    (diffdiffx (rest small)(rest big))
    (zero? (diffsame small (rest big)))))

(defn diffdiff [a b]
  (let [ [small big] (sort-by count [a b])]
    (diffdiffx small big)))

(defn diffcheck [ a b]
  (or (diffdiff a b)(= 1 (diffsame a b))))


(defn match [a coll chain]
  (cond
    (nil? a) nil
    (empty? coll) chain
    :else
    (let [good (filter (partial diffcheck a) coll)]
      (loop [good good]
        (if (empty? good)
          nil
          (let [g (first good)]
            (let [m (match g (remove #{g} coll) (concat chain [g]))]
              (if (nil? m)
                (recur (rest good))
                m))))))))

(def x ["hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"])
(println (diffdiff (first x)(second x)))
(println ["match" (map  (fn [e] (match e (remove #{e} x) [e])) x)])





