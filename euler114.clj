
;def cache = new long[51]
;Arrays.fill(cache,-1)
;cache[3] = 1

(defn count [gap cache]
  (if (< gap 3)
    0
    (if (cache gap)
      (cache gap)
      (let [total 0]
        (for [len (range 3 (inc gap))
              :let [total (+ total ]]))))
;def count(gap, cache) {
;  if (gap < 3) return 0
;  if (cache[gap] != -1) return cache[gap]

;  def total = 0L
;  for (len in 3..gap) {
;    total += gap - len + 1
;    def maxpos = gap - len + 1
;    for (pos in 0..maxpos) {
;      total += count(gap - len - pos - 1, cache)
;    }
;  }
;  cache[gap] = total
;}
;
;21
;def answer = 1 + count(50, cache)

