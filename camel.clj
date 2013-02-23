



(defn camel [x]
  (apply str (first (reduce (fn [[r u] e]
               (let [n (if u (char (- (int e) 32)) e)
                     u (= e \- )
                     r (if u r (concat r [n]))]
                 [r u])) [[] false] x))))

(println (camel "abc-def"))

(def gg (fn [x] (let [
p (vec (.split x "-"))]
  (apply str (concat (first p)
    (mapcat #(concat [(char (- (int (first %)) 32))] (rest %))
   (rest p)))))))

(println (gg "abc-def"))