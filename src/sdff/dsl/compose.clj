(ns sdff.dsl.compose)

(defn compose
  [f g]
  (fn [& args] (f (apply g args))))

(comment

  ((compose (fn [x] (list 'foo x))
            (fn [x] (list 'bar x)))
   'z)
  ;; => (foo (bar z))
  )
