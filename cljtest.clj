(defn my-zipmap [keys vals]
  (loop [my-map {}
         my-keys keys
         my-vals vals]
    (if (and (seq my-keys) (seq my-vals))
      (recur (assoc my-map (first my-keys) (first my-vals))
             (rest my-keys)
             (rest my-vals))
      my-map)))
(println (my-zipmap [:a :b :c] [1 2 3]))

