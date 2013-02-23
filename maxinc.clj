
(reduce (fn [[maxstart maxlen last] [e]
  (if (> e last)
    [maxstart (inc maxlen) e]
     )))
