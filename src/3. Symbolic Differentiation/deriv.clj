;;;; Class Exercise: Symbolic Differentiation
;;;; Full description in:
;;;;     http://webcem01.cem.itesm.mx:8005/s201811/tc2006/symbolic_differentiation.html

(defn variable?
  "Is e a variable?"
  [e]
  (symbol? e))

(defn same-variable?
  "Are v1 and v2 the same variable?"
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn sum?
  "Is e a sum?"
  [e]
  (and (list? e)
       (= 3 (count e))
       (= '+ (first e))))

(defn augend
  "Returns augend of the sum e."
  [e]
  (nth e 1))

(defn addend
  "Returns addend of the sum e."
  [e]
  (nth e 2))

(defn make-sum
  "Construct the sum of a1 and a2."
  [a1 a2]
  (cond

    (= a1 0)
    a2

    (= a2 0)
    a1

    (and (number? a1) (number? a2))
    (+ a1 a2)

    :else
    (list '+ a1 a2)))

(defn deriv
  "Take as arguments an algebraic expression exp
  and a variable var and returns the derivative
  of the expression with respect to the variable."
  [exp var]
  (cond

    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var) 1 0)

    (sum? exp)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var))))
