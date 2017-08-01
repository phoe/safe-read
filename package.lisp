;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:safe-read
  (:use #:cl :trivial-garbage)
  (:export #:safe-read
	   #:with-temp-package 
	   #:*max-input-size*))
