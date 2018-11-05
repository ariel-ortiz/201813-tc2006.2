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

(defmacro AND
  ([] true)
  ([x] x)
  ([a b & exprs]
   `(let [temp# ~a]
      (if temp#
        (AND ~b ~@exprs)
        temp#))))

(defn find-between
  "Returns a sequence of elements in lst
  contained exclusively between start and end."
  [start end lst]
  (->> lst
       (drop-while #(not= start %))
       rest
       (take-while #(not= end %))))

(defmacro IF
  "Provides a conditional statement that is syntactically
   a bit more similar to those found in languages like
   Pascal or Fortran. It has the following form:

   (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
  [condition & exprs]
  `(if ~condition
     (do ~@(find-between :THEN :ELSE exprs))
     (do ~@(find-between :ELSE :THEN exprs))))
