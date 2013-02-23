(def gg (fn [b]
  (let [

ds (partition 2 [
  -1 -1
  -1 1
  -1 0
  0 1
  0 -1
  1 1
  1 -1
  1 0])]


(letfn [

(inr [i mn mx]
  (and (>= i mn)(< i mx)))

(getnbr [b i j is js]
  (filter #{\#}
    (map (fn [[ii jj]]
      (if (and (inr ii 0 is) (inr jj 0 js))
        (nth (nth b ii) jj)))
         (map (fn [[di dj]]
      [(+ di i) (+ dj j)]) ds))))

(res [cell n]
  ({ [\# 3] "#"
     [\# 2] "#"
     [\  3] "#"} [cell n] " "))]


(let [is (count b)
      js (count (first b))]
      (map (partial apply str)
        (partition js
          (for [i (range is)
                j (range js)]
         (res (nth (nth b i) j)
           (count (getnbr b i j is js)))))))))))


(println (= (gg ["      "
        "      "
        "  ### "
        " ###  "
        "      "
        "      "])
   ["      "
    "   #  "
    " #  # "
    " #  # "
    "  #   "
    "      "]))
