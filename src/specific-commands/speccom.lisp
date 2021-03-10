;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package knowledge)

(eval-when (:compile-toplevel :load-toplevel :execute)

	(define-generic-function test ((cardinality :classic)))
)

(define-method test ((ind cardinality))
  (format t "~%Here we have an cardinality!"))




