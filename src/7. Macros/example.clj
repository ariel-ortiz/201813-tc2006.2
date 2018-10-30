(defmacro LET
  "Mimics Clojure's let macro, but only allows defining
  one binding."
  [[var expr] body]
  (list
    (list
      'fn
      [var]
      body)
    expr))

(defmacro COMMENT
  "Ignores x, which doesn't get evaluated, and returns nil."
  [& x]
  nil)

;(defmacro debug
;  [expr]
;  (list
;    'let
;    ['value expr]
;    (list 'print "Debug ")
;    (list 'print (list 'quote expr))
;    (list 'print " : ")
;    (list 'println 'value)
;    'value))

(defmacro debug
  "Prints and returns the result of evaluating expr. Serves
  as a debugging aid."
  [expr]
  `(let [value# ~expr]
     (println "Debug" ~(list 'quote expr) ":" value#)
     value#))
