;; Note: ClojureScript requires that all macros be defined
;; in a different namespace from which they are used.
(ns chess.macros)

;; Like for, but it creates a vector.
(defmacro forv [& more]
  `(into [] (for ~@more)))
