; Simple exercises using Clojure

(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn f2c
  "Takes f degrees Fahrenheit and converts them
  to degrees Celsius."
  [f]
  (/ (* (- f 32) 5) 9))

(defn sign
  "Takes an integer value n. It returns -1 if n
  is negative, 1 if n is positive greater than
  zero, or 0 if n is zero."
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))

(defn roots
  "Returns a vector containing the two possible roots
   that solve a quadratic equation given its three
   coefficients a, b, and c."
  [a b c]
  (let [t1 (- b)
        t2 (sqrt (- (* b b) (* 4 a c)))
        t3 (* 2 a)]
    [(/ (+ t1 t2) t3)
     (/ (- t1 t2) t3)]))

(defn bmi
  "Computes the body mass index of a person."
  [weight height]
  (let [BMI (/ weight (expt height 2))]
    (cond
      (< BMI 20) 'underweight
      (< BMI 25) 'normal
      (< BMI 30) 'obese1
      (< BMI 40) 'obese2
      :else      'obese3)))

(defn fact
  "Computes the factorial of n."
  [n]
  (if (< n 2)
    1
    (*' n (fact (dec n)))))

(defn fact-loop
  "Computes the factorial of n using loop/recur."
  [n]
  (loop [i      1
         result 1]
    (if (> i n)
      result
      (recur (inc i) (* result i)))))

(def zero ())
(defn is-zero [n] (= n zero))
(defn add1 [n] (cons () n))
(defn sub1 [n] (rest n))

(defn add
  [a b]
  (if (is-zero a)
    b
    (add1 (add (sub1 a) b))))

(defn sub
  [a b] ;;; a >= b
  (if (is-zero b)
    a
    (sub1 (sub a (sub1 b)))))

(defn mul
  [a b]
  (if (is-zero a)
    zero
    (add b (mul (sub1 a) b))))

(defn is-less
  [a b]
  (if (is-zero a)
    (if (is-zero b)
      false
      true)
    (if (is-zero b)
      false
      (is-less (sub1 a) (sub1 b)))))

(defn delete
  "Remove all occurrences of x in lst, which
  might contain nested lists."
  [x lst]
  (cond
    (empty? lst)
    ()

    (list? (first lst))
    (conj
      (delete x (rest lst))
      (delete x (first lst)))


    (= x (first lst))
    (delete x (rest lst))

    :else
    (conj
      (delete x (rest lst))
      (first lst))))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(deftest test-fact
  (is (= 1 (fact 0)))
  (is (= 1 (fact 1)))
  (is (= 6 (fact 3)))
  (is (= 120 (fact 5))))

(deftest test-fact-loop
  (is (= 1 (fact-loop 0)))
  (is (= 1 (fact-loop 1)))
  (is (= 6 (fact-loop 3)))
  (is (= 120 (fact-loop 5))))

(run-tests)