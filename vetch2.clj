

(defn getall [sets]
  (map
    set
    (distinct
      (map
        (fn [e] (map symbol [(.toLowerCase (str e)) (.toUpperCase (str e))]))
        (apply clojure.set/union sets)))))


(defn makesets [syms]
  (reduce (fn [current symset]
       (let [[low up] (vec symset)]
             (mapcat (fn [e] [(concat [up] e) (concat [low] e)]) current)))
         [[]] syms))


(def test1 (getall [#{'a 'B} #{'A 'b 'd}]))

(println test1)

(println (makesets test1))
