(ns sdff.dsl.abstracting-domain.monolithic
  (:require [clojure.walk :as walk]))

;; (def piece {:color "red" :position ['x 'y] :king? false})

(def color-current-player "red")

(def board-dimension 8)

(def initial-pos-red-pieces
  #{[0 0] [2 0] [4 0] [6 0]
    [1 1] [3 1] [5 1] [7 1]
    [0 2] [2 2] [4 2] [6 2]})

(def initial-pos-white-pieces
  #{[1 5] [3 5] [5 5] [7 5]
    [0 6] [2 6] [4 6] [6 6]
    [1 7] [3 7] [5 7] [7 7]})

(defn make-board
  []
  (let [board (into {}
                    (for [x (range board-dimension)
                          y (range board-dimension)
                          :let [piece {:position [x y] :king? false}]]
                      (assoc {} (str x y)
                             (cond
                               (contains? initial-pos-red-pieces [x y]) (assoc piece :color "red")
                               (contains? initial-pos-white-pieces [x y]) (assoc piece :color "white")
                               :else
                               piece))))]
    (assoc board :color-next-move color-current-player)))


(defn current-pieces
  "gets a list of the pieces belonging to the current player."
  [board]
  (->> (filter (fn [[_ piece]]
                 (= (:color piece) color-current-player))
               board)
       (map second)))

(defn is-position-on-board?
  "tests whether the given `coords` specify a position on `board`."
  [coords board]
  (let [n (-> board first count)
        x (first coords)
        y (second coords)]
    (and
     (and (pos-int? x) (<= x n))
     (and (pos-int? y) (<= y n)))))


(defn board-get
  "gets the piece that is at the position specified by `coords`. If
there is no piece in that position it returns false"
  [coords board]
  (let [x (first coords)
        y (second coords)]
    (get board (str x y))))


(defn position-info
  "described that occupies the position `coords` in `board`.

  If the position is empty the value is :unoccupied;

  If it contains one of the current player's pieces the valie is
  :occupied-by-self.

  If it contains on opponent's piece the valie is :occupied-by-opponent."
  [coords board]
  (let [piece (board-get coords board)]
    (cond
      (empty? piece) :unoccupied
      (= (:color piece) color-current-player) :occupied-by-self
      :else
      :occupied-by-opponent)))


(defn is-position-unoccupied?
  [coords board]
  (= :unoccupied (position-info coords board)))


(defn is-position-occupied-by-self?
  [coords board]
  (= :occupied-by-self (position-info coords board)))


(defn is-position-occupied-by-opponent?
  [coords board]
  (= :occupied-by-opponent (position-info coords board)))


(defn piece-coords
  [piece]
  (:position piece))


(defn should-be-crowned?
  [piece]
  (and (not (:king piece))
       ;; home row
       true))


(defn crown-piece
  [piece]
  (assoc piece :king? true))


(defn possible-directions
  [piece]
  (let [[x y] (:position piece)
        all-diags [[(- x 1) (+ y 1)]
                   [(+ x 1) (+ y 1)]
                   [(- x 1) (- y 1)]
                   [(+ x 1) (- y 1)]]]
    (filter (fn [[x y]]
              (not (or (< x 0) (< y 0)
                       (> x board-dimension) (> y board-dimension))))
            all-diags)))

(defn step-to
  "gets the piece after `step` is taken."
  [step])


(defn step-board
  "gets the board after `step` is taken."
  [step])


(defn make-simple-move
  "gets a step that moves `piece` to `new-coords` on `board`."
  [new-coords piece board])


(defn make-jump
  "gets a step that moves piece to `new-coords` on `board` and removes
the opponent's piece at `jumped-coords`."
  [new-coords jumped-coords piece board])


(defn replace-piece
  "gets a step that replaces `old-piece` with `new-piece` on `board`."
  [new-piece old-piece board])


(defn path-contains-jumps?
  "tests whether any of the steps in `path` are jumps."
  [path])

(defn- error
  [msg]
  (throw (ex-info msg {})))

(defn coords+
  [coords direction])

(defn try-step
  "what simple steps are possible from a given starting point in a given direction"
  [piece board direction path]
  (let [new-coords (coords+ (piece-coords piece) direction)]
    (and (is-position-on-board? new-coords board)
         (case (position-info new-coords board)
           :unoccupied (and (not (path-contains-jumps? path))
                            (cons (make-simple-move
                                   new-coords
                                   piece
                                   board)
                                  path))

           :occupied-by-opponent (let [landing (coords+ new-coords direction)]
                                   (and (is-position-on-board? landing board)
                                        (is-position-unoccupied? landing board)
                                        (cons (make-jump landing
                                                         new-coords
                                                         piece
                                                         board)
                                              path)))

           :occupied-by-self false
           (error "Unknown position info")))))

(defn- filter-map
  "is equivalent to (filter (lambda (x) x) (map proc lst ...))"
  [proc lst]
  (letfn [(truthy? [v] (if v true false))]
    (filter truthy? (map proc lst))))


(def ^:private append-map mapcat)


(defn compute-next-steps
  "list of possible next paths."
  [piece board path]
  (filter-map (fn [direction]
                (try-step piece board direction path))
              (possible-directions piece)))


(defn evolve-jumps
  [paths]
  (append-map (fn [path]
                (let [step (first path)
                      paths (compute-next-steps (step-to step)
                                                (step-board step)
                                                path)]
                  (if (empty? paths)
                    (list paths)
                    ;; continue jumping if possible
                    (evolve-jumps paths))))
              paths))


(defn evolve-paths
  [piece board]
  (let [paths (compute-next-steps piece board '())
        jumps (filter path-contains-jumps? paths)]
    (if (empty? jumps)
      paths
      (evolve-jumps jumps))))


(defn crown-kings
  [paths]
  (map (fn [path]
         (let [piece (step-to (first path))]
           (if (should-be-crowned? piece)
             (cons (replace-piece (crown-piece piece)
                                  piece
                                  (step-board (first path)))
                   path)
             path)))
       paths))


(defn mandate-jumps
  [paths]
  (let [jumps (filter path-contains-jumps? paths)]
    (if (empty? jumps)
      paths
      jumps)))


(defn generate-moves
  [board]
  (crown-kings
   (mandate-jumps
    (append-map (fn [piece]
                  (evolve-paths piece board))
                (current-pieces board)))))
