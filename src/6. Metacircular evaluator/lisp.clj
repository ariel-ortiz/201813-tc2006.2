;;; Metacircular evaluator

(import 'clojure.lang.IFn)

(deftype Closure
  [env params body]

  IFn

  (invoke
    [self args]
    ($eval body (merge env (zipmap params args))))

  (applyTo
    [self args]
    (self args)))

; Helper functions
(defn third
  [lst]
  (nth lst 2))

(defn fourth
  [lst]
  (nth lst 3))

(defn $eval
  [expr env]
  (cond

    ; 1. Variable referebces
    (symbol? expr)
    (if (contains? env expr)
      (get env expr)
      (throw (RuntimeException. (str "Unbound variable: " expr))))

    ; 2. Special forms
    (list? expr)
    (case (first expr)

      nil
      ()

      quote
      (second expr)

      if
      (if ($eval (second expr) env)
        ($eval (third expr) env)
        ($eval (fourth expr) env))

      lambda
      (->Closure env (second expr) (third expr))

      ; default clause: function invocation
      (apply ($eval (first expr) env)
             (map #($eval % env) (rest expr))))

    ; 3. Everything else evaluates to itself
    :else
    expr))
