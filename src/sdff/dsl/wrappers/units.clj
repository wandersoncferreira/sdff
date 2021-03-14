(ns sdff.dsl.wrappers.units 
  (:require [clojure.test :as t]
            [sdff.dsl.combinators.01-compose :as lib.comb]))

(def gas-constant 8.3144621)

(def pi (* 4 (Math/atan 1)))

(def expt (fn [v e] (Math/pow v e)))

(def div /)

(defn gas-law-volume
  [pressure temperature amount]
  (/ (* amount gas-constant temperature) pressure))

(defn sphere-radius
  [volume]
  (expt (/ volume (* 4/3 pi)) 1/3))

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

;;; specialization wrappers

(defn unit:*
  [u1 u2]
  (make-unit-conversion (lib.comb/compose u2 u1)
                        (lib.comb/compose (unit:invert u1)
                                          (unit:invert u2))))

(defn unit:div
  [u1 u2]
  (make-unit-conversion (fn [v1] (* v1 (/ (u1 1) (u2 1))))
                        (fn [o1] (* o1 (/ ((unit:invert u1) 1)
                                          ((unit:invert u2) 1))))))

(defn unit:expt
  "A bit bad bc my lib.comb/compose only works pair-wise."
  [u n]
  (cond
    (= n 2) (unit:* u u)
    (> n 2) (reduce
             (fn [final-conv _u]
               (unit:* final-conv _u))
             (unit:* u u)
             (repeat (- n 2) u))))

(def ^:private lookup-table-for-unit-conversions
  (atom {}))

(defn register-unit-conversion!
  [from to procedure]
  (let [from-to-key (str from "->" to)
        to-from-key (str to "->" from)]
    (swap! lookup-table-for-unit-conversions assoc from-to-key procedure)
    (swap! lookup-table-for-unit-conversions assoc to-from-key (unit:invert procedure))))

;;; register the unit conversions

(register-unit-conversion! 'celsius 'kelvin celsius-to-kelvin)
(register-unit-conversion! 'fahrenheit 'celsius fahrenheit-to-celsius)
(register-unit-conversion! 'fahrenheit 'kelvin (unit:* fahrenheit-to-celsius celsius-to-kelvin))

(register-unit-conversion! '(div pound (expt inch 2))
                           '(div newton (expt meter 2))
                           (unit:div pound-to-newton (unit:expt inch-to-meter 2)))

(register-unit-conversion! '(expt inch 3) '(expt meter 3) (unit:expt inch-to-meter 3))

;;; ----

(defn make-converter
  [specific implicit]
  (let [lookup-key (str specific "->" implicit)
        converter (get @lookup-table-for-unit-conversions lookup-key)]
    (if converter
      converter
      (make-unit-conversion (fn [v1] v1)
                            (fn [o1] o1)))))

(defn unit-specializer
  [procedure implicit-output-unit & implicit-input-units]
  (defn specializer
    [specific-output-unit & specific-input-units]
    (let [output-converter (make-converter implicit-output-unit specific-output-unit)
          input-converts (map make-converter specific-input-units implicit-input-units)]
      (defn specialized-procedure
        [& arguments]
        (output-converter
         (apply procedure
                (map (fn [converter argument]
                       (converter argument))
                     input-converts
                     arguments))))
      specialized-procedure))
  specializer)

(def make-specialized-gas-law-volume
  (unit-specializer
   gas-law-volume
   '(expt meter 3)
   '(div newton (expt meter 2))
   'kelvin
   'mole))

(def conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3)
   '(div pound (expt inch 2))
   'fahrenheit
   'mole))


(t/deftest dsl-final-test
  (t/is (more-or-less
         (sphere-radius (conventional-gas-law-volume 14.7 68 1))
         7.04962459601831)))

(sphere-radius (conventional-gas-law-volume 14.7 68 1))
;; => 7.04962459601831
