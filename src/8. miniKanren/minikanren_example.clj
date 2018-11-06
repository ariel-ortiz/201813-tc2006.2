(require '[clojure.core.logic :as l])

(l/defne lasto
  "Logical function that succeeds if the last
  element of lst is x."
  [lst x]
  ([[x] x])
  ([[_head . tail] x]
   (lasto tail x)))
