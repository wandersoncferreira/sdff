(ns sdff.arity)

(def arity-table
  (atom {}))


(defn restrict-arity
  [proc nargs]
  (swap! arity-table assoc proc nargs)
  proc)


(defn naive-compute-arity
  "This SO has some interesting discussion around computing arity in Clojure.
https://stackoverflow.com/questions/1696693/clojure-how-to-find-out-the-arity-of-function-at-runtime"
  [proc]
  (let [m (first (.getDeclaredMethods (class proc)))
        p (.getParameterTypes m)]
    (alength p)))


(defn get-arity
  [proc]
  (if-let [n (get proc @arity-table)]
    n
    (naive-compute-arity proc)))
