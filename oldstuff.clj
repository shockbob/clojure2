

(defn getDiag [board index]
  (if (= index 0)
     (map #(board %) [0 4 8])
     (map #(board %) [2 4 6])))

(defn solvedDiag [board which]
  (solved (getDiag board which)))

(defn solvedDiags [board]
    (map #(solvedDiag board %) (range 2)))

(defn solvedRow [board row]
   (solved (getRow board row)))

(defn solvedCol [board col]
   (solved (getCol board col)))

(defn solvedRows [board]
  (map #(solvedRow board %) (range 3)))

(defn solvedCols [board]
  (map #(solvedCol board %) (range 3)))



(defn getCol [board col] (map #(board %) [col (+ 3 col) (+ 6 col)]))

(defn getRow [board row]
  (let [start (* 3 row)]
    (map #(board %) [start (inc start) (+ 2 start)])))

(defn solvedAll [board]
  (concat (solvedRows board) (solvedDiags board) (solvedCols board)))

(defn chooseAndMove [player1 player2 allMoves board even]
  (if even
    (let [chosen (chooseMove player1 player2 allMoves board)
          next (move chosen player1 board)]
      next)
    (let [chosen (chooseMove player2 player1 allMoves board)
          next (move chosen player2 board)]
      next)
    ))

(defn addChild [board child]
  (assoc board :children (conj (board :children []) child)))

(defn ponegame [player1 player2]
  (reduce (fn [boards i]
    (let [board (first boards)
          solved (isSolved board)]
      (if solved
        boards
        (let [even (even? i) allMoves (validMoves board)
              next (chooseAndMove player1 player2 allMoves board even)]
          (concat [next] boards)))))
    [newBoard] (range 9)))