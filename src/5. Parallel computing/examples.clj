(defn fibo
  [n]
  (if (< n 2)
    n
    (+ (fibo (- n 1))
       (fibo (- n 2)))))

(time (doall (pmap fibo '(42 42 42 42))))