
(def __ (fn [a b]
  (let [
    pows2 (iterate #(* 2 %) 1)
    pad (fn [c sz] (concat (repeat (- sz (count c)) \0) c))
    onevecs (fn [n howmany]
       (let [nums (map #(Integer/toString % 2) (range (nth pows2 n)))]
         (map (fn [c] (pad c n))
            (filter (fn [num] (= howmany (count (filter (fn [d] (= d \1) ) num)))) nums))))

    removes (fn [st onevec]
        (keep identity (map (fn [s o] (if (not= o \1) s)) st onevec)))

    getallrems (fn [st diff]
       (map
         (fn [rem] (removes st rem))
         (onevecs (count st) diff)))

    diffsame (fn [a b]
       (count (filter (partial apply not=) (map vector a b))))]
  (if (= (count a)(count b))
    (diffsame a b)
    (let [[small big] (sort-by count [a b])]
          (if (zero? (count small))
             (count big)
          (let [
              diff (- (count big)(count small))
              smalls (getallrems big diff)]
              (+ diff (apply min (map (partial diffsame small) smalls))))))))))

(def __ (fn df [a b]
  (cond
    (empty? b) (count a)
    (empty? a) (count b)
    :else (let [[a1 & an] a
          [b1 & bn] b]
      (if (= a1 b1)
        (df an bn)
        (inc (min (df a bn) (df an bn) (df an b))))))))


(def zz (fn [a b]
  (let [
 sb (fn [s n]
    (filter
       #(= (- (count s) n) (count %))
       (reduce
          (fn [ss x] (concat ss (map #(conj % x) ss)))
          [[]]
         s)))
    ds (fn [a b]
      (count (filter (partial apply not=) (map vector a b))))
    [s b] (sort-by count [a b])
     cs (count s)
     cb (count b)
     diff (- cb cs)
     ss (sb b diff)]
   (+ diff (apply min (map (partial ds s) ss))))))

(println
  (= (__ "kitten" "sitting") 3)
  (= (__ "closure" "clojure") (__ "clojure" "closure") 1)
  (= (__ "" "123456") 6)
  (= (__ "xyx" "xyyyx") 2)
  (= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)
  (= (__ [1 2 3 4] [0 2 3 4 5]) 2)
  (= (__ '(:a :b :c :d) '(:a :d)) 2)
  (= (__ "ttttattttctg" "tcaaccctaccat") 10)
  (= (__ "gaattctaatctc" "caaacaaaaaattt") 9)
)
