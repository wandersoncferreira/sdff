(ns sdff.dsl.parallel-combine
  (:require [clojure.core.async :as a]))

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
