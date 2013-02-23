(fn [a]
    (loop [st (seq a) upcase false retval []]
       (if (empty? st)
         (apply str retval)
        (let [
         next (if upcase
           (Character/toUpperCase (first st))
           (first st))
         upcase (= \- next)
         retval (if upcase retval (conj retval next))]
        (recur (rest st) upcase
retval)))))

(def camel (fn [x]
  (apply str (first (reduce (fn [[r u] e]
    (let [n (if u (Character/toUpperCase e) e)
          u (= \- e)
          r (if u r (conj r n))]
        [r u])) [[] false] x)))))

(println (camel "abc----def"))
