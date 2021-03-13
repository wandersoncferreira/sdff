(ns sdff.dsl.parallel-combine
  "We can arrange to use two functions in parallel, then combine their
  results with specified combiner function. The `parallel-combine` is
  our combiner function."
  (:require [clojure.test :as t]
            [sdff.dsl.arity :as arity]
            [sdff.scheme-bridge :as scheme]))


(defn parallel-combine-v1
  [h f g]
  (fn [& args]
    (h (apply f args) (apply g args))))


(comment

  ((parallel-combine-v1 list
                        (fn [x y z] (list 'foo x y z))
                        (fn [u v w] (list 'bar u v w)))
   'a 'b 'c)
 ;; => ((foo a b c) (bar a b c))
  )



;;; my version of "parallel" using futures

(defn bk-parallel-combine
  [h f g]
  (fn [& args]
    (let [res-f (future (apply f args))
          res-g (future (apply g args))]
      (h @res-f @res-g))))

(comment

  ((bk-parallel-combine list
                        (fn [x y z] (list 'foo x y z))
                        (fn [u v w] (list 'bar u v w)))
   'a 'b 'c)
 ;; => ((foo a b c) (bar a b c))
  )

;;; exercise 2.1
;;; 1. check their components to make sure that the arities are compatible
;;; 2. combination constructed checks that it is given the correct number of arguments when called
;;; 3. combination advertises its arity correctly to get-arity

(defn parallel-combine-ex2-1
  [h f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (assert (= n m))
    (defn the-combination-parallel-combine [& args]
      (assert (= (count args) n))
      (h (apply f args)
         (apply g args)))
    (arity/restrict-arity the-combination-parallel-combine n)))


(t/deftest verify-arity-constraints
  (t/testing "correct expected case"
    (t/is
     (= ((parallel-combine-ex2-1 list
                                 (fn [x y z] (list 'foo x y z))
                                 (fn [u v w] (list 'bar u v w)))
         'a 'b 'c)
        '((foo a b c) (bar a b c)))))

  (t/testing "provide F and G with different arities."
    (t/is (thrown? AssertionError
                   ((parallel-combine-ex2-1 list
                                               (fn [x] (list 'foo x))
                                               (fn [u v] (list 'bar u v)))
                    'a 'b))))

  (t/testing "F and G with same arity, but different nargs at runtime"
    (t/is (thrown? AssertionError
                   ((parallel-combine-ex2-1 list
                                               (fn [x y] (list 'foo x y))
                                               (fn [u v] (list 'bar u v)))
                    'a 'b 'c)))))

;;; exercise 2.3.
;;; reformulate `parallel-combine` to be a composition of two parts and to allow the parts to return multiple values.

(defn compose
  "From spread_apply exercise."
  [f g]
  (let [m (arity/get-arity g)]
    (defn the-composition-v3 [& args]
      (scheme/call-with-values
       (fn [] (apply g args))
       f))
    (arity/restrict-arity the-composition-v3 m)))

(defn parallel-apply
  [f g]
  (let [n (arity/get-arity f)
        m (arity/get-arity g)]
    (assert (= n m))
    (defn the-combination-parallel-combine [& args]
      (assert (= (count args) n))
      (let [fv  (apply f args)
            gv (apply g args)]
        (apply scheme/values (scheme/append fv gv))))
    (arity/restrict-arity the-combination-parallel-combine n)))

(defn parallel-combine-ex2-3
  [h f g]
  (compose h (parallel-apply f g)))

;;; new tests returning values
(t/deftest verify-arity-constraints
  (t/testing "correct expected case"
    (t/is
     (= ((parallel-combine-ex2-3 list
                                 (fn [x y z] (scheme/values x y z))
                                 (fn [u v w] (scheme/values u v w)))
         'a 'b 'c)
        '(a b c a b c))))

  (t/testing "provide F and G with different arities."
    (t/is (thrown? AssertionError
                   ((parallel-combine-ex2-3 list
                                               (fn [x] (scheme/values x))
                                               (fn [u v] (scheme/values u v)))
                    'a 'b))))

  (t/testing "F and G with same arity, but different nargs at runtime"
    (t/is (thrown? AssertionError
                   ((parallel-combine-ex2-3 list
                                               (fn [x y] (scheme/values x y))
                                               (fn [u v] (scheme/values u v)))
                    'a 'b 'c)))))

