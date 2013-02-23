(defprotocol P
  (foo [x])
  (class [x])
  ( [x])
  (bar-me [x] [x y]))

(deftype Foo [a b c]
  P
  ( [x] true)
  (foo [x] a)
  (class [x] true)
  (bar-me [x y] (+ c y)))

(println  (Foo. 1 2 3))

