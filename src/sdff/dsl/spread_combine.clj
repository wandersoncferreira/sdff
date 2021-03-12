(ns sdff.dsl.spread-combine
  (:require [sdff.dsl.arity :as arity]))

;;; sneaky way to define the arity of the function

(defn spread-combine-v1
  [h f g]
  (let [n (arity/get-arity f)]
    (fn [& args]
      (h (apply f (take n args))
         (apply g (drop n args))))))


(defn spread-combine-v2
  "Version using appropriate arity decorated."
  [h f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (let [t (+ n m)]
      (defn the-combination [& args]
        (h (apply f (take n args))
           (apply g (drop n args))))
      (arity/restrict-arity the-combination t))))


(defn spread-combine-v3
  "Version using assert to ensure we have the desired arity."
  [h f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (let [t (+ n m)]
      (defn the-combination [& args]
        (assert (= (count args) t))
        (h (apply f (take n args))
           (apply g (drop n args))))
      (arity/restrict-arity the-combination t))))


(comment

  ((spread-combine-v3 list
                      (fn [x y] (list 'foo x y))
                      (fn [u v w] (list 'bar u v w)))
   'a 'b 'c 'd 'e)
;; => ((foo a b) (bar c d e))
  )
