(ns sdff.dsl.05-discard-argument
  (:require [sdff.arity :as arity]))

(defn list-remove
  [lst index]
  (keep-indexed #(if (not= %1 index) %2) lst))

(defn discard-argument
  [i]
  (assert (pos-int? i))
  (fn [f]
    (let [m (+ (arity/get-arity f) 1)]
      (defn the-combination-discard [& args]
        (assert (= (count args) m ))
        (apply f (list-remove args i)))
      (assert (< i m))
      (arity/restrict-arity the-combination-discard m))))


(comment
  
 (((discard-argument 2)
   (fn [x y z] (list 'foo x y z)))
  'a 'b 'c 'd)
;; => (foo a b d)

 )
