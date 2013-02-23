(defn __ [coll]
  (letfn [
    (split1 [[f & r]]
      (reduce
        (fn [c e]
          (if (<= e (inc (last c)))
            (conj c e)
            (conj c nil e)))
        [f]
        r))]
    (map (juxt first last)
      (remove #{[nil]}

        (partition-by nil?
          (split1 (sort coll)))))))

(println (__ [1 2 3 5 6 8]))





