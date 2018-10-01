(use 'clojure.test)
(use 'clojure.math.numeric-tower)

;==========================================================
(defn suffixes
  "Returns a list with all the possible suffixes of lst."
  [lst]
  (take (inc (count lst)) (iterate rest lst)))

(defn binary->decimal
  "Returns the equivalent decimal value of the series
  of binary digits contained in lst."
  [lst]
  (reduce (fn [accum digit] (+ digit (* 2 accum))) 0 lst))

(defn range-of-evens
  "Returns a list with every even integer i, where
  start <= i <= end. The resulting list is in ascending
  order."
  [start end]
  (filter even? (range start (inc end))))

(defn list-of-symbols?
  "Returns true if all elements in lst are symbols,
  false otherwise."
  [lst]
  (every? symbol? lst))

(defn invert-pairs
  "Takes a list of vectors with two elements and returns
  a new list with the same vectors but with its two
  elements inverted."
  [lst]
  (map #(vector (second %) (first %)) lst))

(defn pack
  "Returns a list in which consecutive repeated elements
  of lst are placed in separate sublists."
  [lst]
  (partition-by identity lst))

(defn compress
  "Returns a list in which consecutive repeated elements of
  lst are replaced with a single copy of the element."
  [lst]
  (map first (pack lst)))

(defn encode
  "Returns a list in which consecutive duplicated elements of
  lst are encoded as vectors [n e], where n is the number of
  duplicates of the element e."
  [lst]
  (map #(vector (count %) (first %))
       (pack lst)))

(defn encode-modified
  "Works the same as encode, but if an element has no duplicates
  it is simply copied into the resulting list."
  [lst]
  (map #(if (= 1 (first %))
          (second %)
          %)
       (encode lst)))

(defn decode
  "It returns the decoded version of lst, as created by the
  function encode-modified."
  [lst]
  (mapcat #(if (vector? %)
             (repeat (first %)
                     (second %))
             (list %))
          lst))

;==========================================================
(deftest test-suffixes
  (is (= '(())
        (suffixes ())))
  (is (= '((a) ())
        (suffixes '(a))))
  (is (= '((a b) (b) ())
        (suffixes '(a b))))
  (is (= '((a b c d) (b c d) (c d) (d) ())
        (suffixes '(a b c d))))
  (is (= '((a b c d e f g) (b c d e f g)
            (c d e f g) (d e f g)
            (e f g) (f g)
            (g) ())
        (suffixes '(a b c d e f g)))))

(deftest test-binary->decimal
  (is (= 0
        (binary->decimal ())))
  (is (= 1
        (binary->decimal '(1))))
  (is (= 2
        (binary->decimal '(1 0))))
  (is (= 5
        (binary->decimal '(1 0 1))))
  (is (= 8
        (binary->decimal '(1 0 0 0))))
  (is (= 42
        (binary->decimal '(1 0 1 0 1 0))))
  (is (= 63
        (binary->decimal '(1 1 1 1 1 1))))
  (is (= 24601
        (binary->decimal '(1 1 0 0 0 0 0 0 0 0 1 1 0 0 1)))))

(deftest test-range-of-evens
  (is (= ()
        (range-of-evens 10 0)))
  (is (= '(10)
        (range-of-evens 10 10)))
  (is (= '(-4 -2 0 2 4)
        (range-of-evens -4 5)))
  (is (= '(2 4 6 8 10)
        (range-of-evens 1 10)))
  (is (= '(-4 -2 0 2 4 6)
        (range-of-evens -5 7)))
  (is (= '(-6 -4 -2 0 2 4 6)
        (range-of-evens -6 7)))
  (is (= '(-6 -4 -2 0 2 4 6 8)
        (range-of-evens -6 8)))
  (is (= '(-50 -48 -46 -44 -42 -40 -38
            -36 -34 -32 -30 -28 -26 -24
            -22 -20 -18 -16 -14 -12 -10
            -8 -6 -4 -2 0 2 4 6 8 10 12
            14 16 18 20 22 24 26 28 30 32
            34 36 38 40 42 44 46 48 50)
        (range-of-evens -50 50))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
    (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
        (invert-pairs '([January 1][February 2][March 3])))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
        (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
        (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
        (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
        (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
        (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

;==========================================================
(run-tests)
