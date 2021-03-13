(ns sdff.dsl.combinators.07-permute-arguments 
  (:require [sdff.arity :as arity]
            [sdff.scheme-bridge :as scheme]))


(defn make-permutation
  [permspec]
  (defn the-permuter [lst]
    (map (fn [p] (scheme/list-ref lst p)) permspec))
  the-permuter)


(defn permute-arguments
  [& permspec]
  (let [permute (make-permutation permspec)]
    (fn [f]
      (defn the-combination-permute [& args]
        (apply f (permute args)))
      (let [n (arity/get-arity f)]
        (assert (= n (count permspec)))
        (arity/restrict-arity the-combination-permute n)))))

(comment
  
 (((permute-arguments 1 2 0 3)
   (fn [x y z w] (list 'foo x y z w)))
  'a 'b 'c 'd)
;; => (foo b c a d)
)
