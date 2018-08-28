(defn compose
  "Returns a new function that represents f(g(x))."
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (+ x 3))
(defn f2 [x] (* x x))
(def f3 (compose f1 f2))
(def f4 (compose f2 f1))
(def f5 (compose f3 f4))

(defn whatever
  [a b c d]
  (+ (* 3 a) (* 2 b) c (* d d)))

(defn whatever'
  [a]
  (fn [b]
    (fn [c]
      (fn [d]
        (+ (* 3 a) (* 2 b) c (* d d))))))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))