(ns examples.hello
    (:gen-class))

(defn -main
  [greetee]
  (println (str "Hello " greetee "!")))

(compile 'examples.hello)