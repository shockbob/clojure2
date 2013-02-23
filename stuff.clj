
(defn getuniques [bools pairs]
  (flatten (remove next (map (fn [pair] (filter pair bools)) pairs))))

(println (getuniques #{'a 'b 'A 'c 'C 'D 'd} [#{'a 'A}#{'b 'B}#{'c 'C}#{'d 'D}]))


