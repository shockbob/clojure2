
(def fname ["bob" "sally" "pat"])
(def lname ["jones" "smith" "flores"])
(def suffix "@appleschool.edu")
(println (map str fname (repeat ".") lname (repeat suffix)))



