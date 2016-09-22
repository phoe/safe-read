;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SECURE-READ
;;;; © Michał "phoe" Herda 2016
;;;; test.lisp

(in-package #:secure-read)

;;;; CONDITION-KEY test
(let ((condition (make-condition 'error)))
  (assert (string= 'error (condition-key condition))))

;;;; WITH-TEMP-PACKAGE test
(let ((*package* (find-package "COMMON-LISP")))
  (with-temp-package
    (assert (not (eq *package* (find-package "COMMON-LISP")))) 
    (assert (search "TEMP-PKG-" (package-name *package*)))))

;;;; SAFE-READTABLE test
(let ((*readtable* %safe-readtable%))
  (flet ((errors (string) (signals malformed-input (read-from-string string)))
	 (oerrors (string) (signals (and error (not malformed-input))
			     (read-from-string string)))
	 (generate (char) (coerce (list #\# char) 'string)))
    (let* ((sharpsign-chars '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
			      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
			      #\# #\' #\( #\) #\* #\= #\\ #\| #\+ #\- #\.))
	   (sharpsign-strings (mapcar #'generate sharpsign-chars))) 
      (mapcar #'oerrors '("\"" "(" ")" "#")) 
      (mapcar #'errors (list* "'" ";" "`" "," sharpsign-strings))
      (eq 'test (read-from-string "#:test")))))
