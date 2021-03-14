(ns sdff.dsl.wrappers.units 
  (:require [clojure.test :as t]
            [sdff.dsl.combinators.01-compose :as lib.comb]))

(def gas-constant 8.3144621)

(def pi (* 4 (Math/atan 1)))

(defn gas-law-volume
  [pressure temperature amount]
  (/ (* amount gas-constant temperature) pressure))

(defn sphere-radius
  [volume]
  (Math/pow (/ volume (* 4/3 pi)) 1/3))

(defn make-unit-conversion
  [f g]
  (fn
    ([] g)
    ([t] (f t))))

(defn unit:invert
  [g]
  (fn [t] ((g) t)))

(def fahrenheit-to-celsius
  (make-unit-conversion (fn [f] (float (* 5/9 (- f 32))))
                        (fn [c] (float (+ (* c 9/5) 32)))))

(def celsius-to-kelvin
  (let [zero-celsius 273.15]
    (make-unit-conversion (fn [c] (float (+ c zero-celsius)))
                          (fn [k] (float (- k zero-celsius))))))

(def pound-to-newton
  (make-unit-conversion (fn [p] (* p 4.4482216))
                        (fn [n] (/ n 4.4482216))))

(def inch-to-meter
  (make-unit-conversion (fn [i] (* i 0.0254))
                        (fn [m] (/ m 0.0254))))

(def psi-to-nsm
  "My compose is working pair-wise only for now."
  (lib.comb/compose
   (lib.comb/compose pound-to-newton
                     (unit:invert inch-to-meter))
   (unit:invert inch-to-meter)))


(defn- more-or-less
  [v1 v2]
  (and (< (- v2 0.1) v1)
       (> (+ v2 0.1) v1)))

(t/deftest unit-conversion-test
  (t/is (== (fahrenheit-to-celsius -40) -40))
  (t/is (== (fahrenheit-to-celsius 32) 0))
  (t/is (more-or-less (pound-to-newton 10) 44.48))
  (t/is (== ((unit:invert fahrenheit-to-celsius) 20) 68))
  (t/is (more-or-less ((lib.comb/compose celsius-to-kelvin fahrenheit-to-celsius) 80) 299.816))
  (t/is (more-or-less ((unit:invert inch-to-meter)
                       (sphere-radius
                        (gas-law-volume
                         (psi-to-nsm 14.7)
                         ((lib.comb/compose celsius-to-kelvin fahrenheit-to-celsius) 68) 1)))
                      7.0496)))

