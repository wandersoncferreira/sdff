(ns sdff.dsl.combinators.06-curry-argument 
  (:require [sdff.arity :as arity]))

(defn list-insert
  [lst index value]
  (if (= index 0)
    (cons value lst)
    (cons (first lst) (list-insert (rest lst) (- index 1) value))))

(defn curry-argument
  [i]
  (fn [& args]
    (fn [f]
      (assert (= (count args) (- (arity/get-arity f) 1)))
      (fn [x]
        (apply f (list-insert args i x))))))


(comment
  
 ((((curry-argument 2) 'a 'b 'c)
   (fn [x y z w] (list 'foo x y z w)))
  'd)
;; => (foo a b d c)
)
