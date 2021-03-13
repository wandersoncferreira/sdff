(ns sdff.dsl.combinators.04-spread-apply
  "To be able to abstract the pattern of `spread-combine` and
  `parallel-combine` we need to be able to return multiple values
  from the combination of F and G"
  (:require [sdff.arity :as arity]
            [sdff.scheme-bridge :as scheme]))


(defn spread-apply
  [f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (let [t (+ n m)]
      (defn the-combination-spread-apply [& args]
        (assert (= (count args) t))
        (scheme/values (apply f (take n args))
                       (apply g (drop n args))))
      (arity/restrict-arity the-combination-spread-apply t))))


(defn compose
  [f g]
  (let [m (arity/get-arity g)]
    (defn the-composition-v3 [& args]
      (scheme/call-with-values
       (fn [] (apply g args))
       f))
    (arity/restrict-arity the-composition-v3 m)))


(defn spread-combine
  [h f g]
  (compose h (spread-apply f g)))


(comment

  ((spread-combine
    list
    (fn [x y] (list 'foo x y))
    (fn [u v w] (list 'bar u v w)))
   'a 'b 'c 'd 'e)
  ;; => ((foo a b) (bar c d e))

  )


;;; further generalization of `spread-apply` to enable both f and g to return multiple values
;;; not using `let-values`, as regular `let` can make it work as expected.

(defn spread-apply-g
  [f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (let [t (+ n m)]
      (defn the-combination-spread-apply [& args]
        (assert (= (count args) t))
        (let [fv (apply f (take n args))
              gv (apply g (drop n args))]
          (apply scheme/values (scheme/append fv gv))))
      (arity/restrict-arity the-combination-spread-apply t))))

(defn spread-combine-g
  [h f g]
  (compose h (spread-apply-g f g)))


(comment
  
 ((spread-combine-g list
                    (fn [x y] (scheme/values x y))
                    (fn [u v w] (scheme/values u v w)))
  'a 'b 'c 'd 'e)
 
 ;; => (a b e d c)


)
