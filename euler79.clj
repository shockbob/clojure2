(def lines (slurp "c:\\users\\bobshock\\keylog.txt"))
(def logins (map #(.trim %) (.split lines "\n")))

(def conc (comp distinct concat))

(defn addkey [map [a b c]]
    (merge-with conc map {a [] b [a] c [a b]}))

(def bigmap (reduce (fn [map string] (addkey map string)) {} logins))

(println (sort-by (fn [[key val]] (count val)) bigmap))
