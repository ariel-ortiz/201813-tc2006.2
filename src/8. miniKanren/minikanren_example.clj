(require '[clojure.core.logic :as l])

(l/defne lasto
  "Logical function that succeeds if the last
  element of lst is x."
  [lst x]
  ([[x] x])
  ([[_head . tail] x]
   (lasto tail x)))

(l/defne dupo
  "Logical function that succeeds if every element in
  lst is duplicated in result."
  [lst result]
  ([[] []])
  ([[head . tail] [head head . temp]]
   (dupo tail temp)))

(l/defne reverseo
  "Logical function that succeeds if the the reverse of
  lst is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (l/fresh [temp]
     (l/appendo temp [head] result)
     (reverseo tail temp))))

(l/defne twino
  "Logical function that succeeds if lst is a sequence
  with two equal elements."
  [lst]
  ([[x x]]))

(l/defne anti-twino
  "Logical function that succeeds if lst is a sequence
  with two distinct elements."
  [lst]
  ([[x y]]
   (l/!= x y)))

(l/defne enlisto
  "Logical function that succeeds if each element of lst
  is contained within its own list inside result."
  [lst result]
  ([[] []])
  ([[head . tail] [[head] . temp]]
   (enlisto tail temp)))