


(def gg (fn [graph] (letfn [
(graphnodes [graph]
  (distinct (flatten graph)))

(findconn [graph a]
      (distinct (filter (fn [x] (not= a x))
        (flatten (filter (fn [[f l]] (or (= f a) (= l a))) graph)))))

(makeconnmap [graph]
  (let [gn (graphnodes graph)]
    (zipmap gn (map (partial findconn graph) gn))))

(findallconn [node graphmap visited]
  (if (visited node)
    visited
    (last (sort-by count
      (map (fn [neighbor] (findallconn neighbor graphmap (set (concat visited [node]))))
        (graphmap node))))
      ))
](let [graphmap (makeconnmap graph)
       gn (graphnodes graph)]
  (and (= (count (distinct graph)) (count graph))
       (= (count (findallconn (first gn) graphmap #{})) (count gn)))))))

(println (gg  [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]])