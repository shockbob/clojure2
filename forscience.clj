(def gg (fn [i] (letfn [
(ir [k mink maxk]
  (and (< k maxk)(>= k mink)))
(v [[i j] [maxi maxj]]
  (and (ir i 0 maxi) (ir j 0 maxj)))
(ic [bd [r c]]
  (contains? (bd :ch) [r c]))
(ie [bd [r c]]
  (nil? (some (fn [key] (contains? (bd key) [r c])) (keys bd))))
(ns [[r c] {nr :nr nc :nc}]
  (filter
    (fn [[r c]] (v [r c] [nr nc]))
    (map
      (fn [[drow dcol]] [(+ drow r)(+ dcol c)])
      [[1 0][0 1][-1 0][0 -1]])))
(fb [i nr nc ch]
   (set (for [r (range nr) c (range nc) :when (= ch (get-in i [r c]))] [r c])))
(mb [i]
  (let [
         nr (count i)
         nc (count (nth i 0))
         ch (fb i nr nc \C)
         mouse (fb i nr nc \M)
         blocks (fb i nr nc \#)
         ]
    {:nr nr :nc nc :ch ch 0 mouse :blocks blocks}))

(fp [bd i]
  (let [is (bd i)]
    (if (seq is)
      (let [allns (mapcat (fn [xy] (ns xy bd)) is)
            ch (some (fn [nh] (ic bd nh)) allns)
            es (filter (fn [nh] (ie bd nh)) allns)]
      (if ch
        true
        (if (seq es)
          (let [nb (assoc bd (inc i) (set es))]
                (fp nb (inc i)))))))))]
       (true? (fp (mb i) 0)))))

(println (gg ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"]))
