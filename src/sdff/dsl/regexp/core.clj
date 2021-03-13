(ns sdff.dsl.regexp.core 
  (:require [clojure.test :as t]))

(defn r:dot
  []
  ".")

(defn r:bol
  []
  "^")

(defn r:eol
  []
  "$")

(defn r:seq
  "Treat a given set of regular-expression fragments as a self-contained element."
  [& exprs]
  (str "\\(" (apply str exprs) "\\)"))

(def ^:private chars-needing-quoting
  #{\. \[ \\ \^ \$ \*})

(defn- list->string
  [lst]
  (apply str lst))

(defn r:quote
  [string]
  (r:seq
   (list->string
    (mapcat (fn [_char]
              (if (contains? chars-needing-quoting _char)
                (list \\ _char)
                (list _char)))
            string))))

(defn- pair?
  "Ugh!"
  [exprs]
  (>= (count exprs) 2))

(defn r:alt
  [& exprs]
  (if (pair? exprs)
    (apply r:seq
           (cons (first exprs)
                 (mapcat (fn [expr] (list "\\|" expr))
                         (rest exprs))))
    (r:seq (first exprs))))

(def ^:private append concat)

(defn r:repeat
  [_min _max expr]
  (apply r:seq
         (append (repeat _min expr)
                 (cond
                   (not _max) (list expr "*")
                   (= _max _min) '()
                   :else
                   (repeat (- _max _min) (r:alt expr ""))))))

(def ^:private string->list vec)

(defn- bracket
  [string proc]
  (list->string
   (append '(\[)
           (proc (string->list string))
           '(\]))))

(def ^:private chars-needing-quoting-in-brackets
  #{\] \^ \-})

(defn- quote-bracketed-contents
  [members]
  (defn- optional [_char]
    (if (contains? members _char) (list _char) '()))
  (append (optional \])
          (remove
           (fn [c]
             (contains? chars-needing-quoting-in-brackets c))
           members)
          (optional \^)
          (optional \-)))

(defn- lset=
  [lst1 lst2]
  (= (set lst1) (set lst2)))

(defn r:char-from
  [string]
  (case (count string)
    0 (r:seq)
    1 (r:quote string)
    (bracket string
             (fn [members]
               (if (lset= '(\- \^) members)
                 '(\- \^)
                 (quote-bracketed-contents members))))))

(defn r:char-not-from
  [string]
  (bracket string
           (fn [members]
             (cons \^ (quote-bracketed-contents members)))))

;;; test cases

(defn something-large-that-might-cause-seizures-in-the-reader?
  [string]
  (= 116 (count string)))

(t/deftest regexp-tests
  (t/is (= (r:seq (r:quote "a") (r:dot) (r:quote "c"))
           "\\(\\(a\\).\\(c\\)\\)"))

  (t/is (= (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))
           "\\(\\(foo\\)\\|\\(bar\\)\\|\\(baz\\)\\)"))

  (t/is (something-large-that-might-cause-seizures-in-the-reader?
         (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog"))))))

;;; lets print the corresponding `grep` command and use cut and paste to run it in a shell
;;; let's write some functions to help us running in Bourne Shell

(defn bourne-shell-quote-string
  [string]
  (list->string
   (append (list \')
           (mapcat (fn [_char]
                     (if (= _char \')
                       (list \' \\ _char \')
                       (list _char)))
                   (string->list string))
           (list \'))))

(defn bourne-shell-grep-command-string
  [expr filename]
  (str "grep -e "
       (bourne-shell-quote-string expr)
       " "
       filename))

(defn write-bourne-shell-grep-command
  [expr filename]
  (println (bourne-shell-grep-command-string expr filename)))


(write-bourne-shell-grep-command (r:alt (r:quote "deftest") (r:quote "defn")) "core.clj")
;; grep -e '\(\(deftest\)\|\(defn\)\)' core.clj
