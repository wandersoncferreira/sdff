(ns sdff.dsl.spread-combine)

;;; sneaky way to define the arity of the function

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

(defn spread-combine-v1
  [h f g]
  (let [n (get-arity f)]
    (fn [& args]
      (h (apply f (take n args))
         (apply g (drop n args))))))


(defn spread-combine-v2
  "Version using appropriate arity decorated."
  [h f g]
  (let [n (get-arity f)
        m (get-arity g)]
    (let [t (+ n m)]
      (defn the-combination [& args]
        (h (apply f (take n args))
           (apply g (drop n args))))
      (restrict-arity the-combination t))))


(defn spread-combine-v3
  "Version using assert to ensure we have the desired arity."
  [h f g]
  (let [n (get-arity f)
        m (get-arity g)]
    (let [t (+ n m)]
      (defn the-combination [& args]
        (assert (= (count args) t))
        (h (apply f (take n args))
           (apply g (drop n args))))
      (restrict-arity the-combination t))))


(comment

  ((spread-combine-v3 list
                      (fn [x y] (list 'foo x y))
                      (fn [u v w] (list 'bar u v w)))
   'a 'b 'c 'd 'e)
;; => ((foo a b) (bar c d e))
  )
