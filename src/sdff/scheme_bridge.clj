(ns sdff.scheme-bridge)

;;; implement call-with-values, and values - some ideas from here
;;; https://stackoverflow.com/questions/16674214/how-to-implement-call-with-values-to-match-the-values-example-in-r5rs


(let [magic [:magic]]
  (defn magic? [x]
    (and (= 2 (count x)) (= (first x) (first magic))))


  (defn values [& args]
    (if (and (seq args) (empty? (rest args)))
      (first args)
      (conj magic args)))
  

  (defn call-with-values
    [producer consumer]
    (let [x (producer)]
      (if (magic? x)
        (apply consumer (first (rest x)))
        (consumer x))))
  )


;;; interesting explanation about Multivalue design in Scheme
;;; https://stackoverflow.com/questions/45379116/what-is-the-benefit-to-use-let-values-instead-of-let
