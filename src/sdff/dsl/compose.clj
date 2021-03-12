(ns sdff.dsl.compose
  (:require [clojure.test :as t]
            [sdff.dsl.arity :as arity]))

(defn compose
  [f g]
  (fn [& args] (f (apply g args))))

(comment

  ((compose (fn [x] (list 'foo x))
            (fn [x] (list 'bar x)))
   'z)
  ;; => (foo (bar z))
  )


;;; exercise 2.1. Arity repair
;;; 1. check their components to make sure that the arities are compatible
;;; 2. combination constructed checks that it is given the correct number of arguments when called
;;; 3. combination advertises its arity correctly to get-arity

(defn compose-ex2-1
  [f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (assert (>= n 1))
    (defn the-combination-compose [& args]
      (assert (= (count args) m))
      (f (apply g (take m args))))
    (arity/restrict-arity the-combination-compose m)))


(t/deftest verify-arity-constraints
  (t/testing "correct expected case"
    (t/is
     (= ((compose-ex2-1 (fn [x] (list 'foo x))
                        (fn [y] (list 'bar y)))
         'z)
        '(foo (bar z)))))

  (t/testing "wrong number of args passed in runtime."
    (t/is (thrown? AssertionError
                   ((compose-ex2-1 (fn [x] (list 'foo x))
                                   (fn [y] (list 'bar y)))
                    'z 'x 'w))))

  (t/testing "if arity of F is not at least 1, throw assertion error. We don't know how many items G is going to return."
    (t/is (thrown? AssertionError
                   ((compose-ex2-1 (fn [] (list 'foo))
                                   (fn [y] (list 'bar y))) 'z)))))
