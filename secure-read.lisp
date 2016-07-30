;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SECURE-READ
;;;; © Michał "phoe" Herda 2016
;;;; secure-read.lisp

;; Lisp reader hackery below. Beware.
(in-package #:secure-read)

;; Exported conditions and parameters
(define-condition incomplete-input () ())
(define-condition malformed-input (error) ())
(define-condition input-size-exceeded (error) ())
(defparameter *max-input-size* (* 128 1024))

;; Utility functions
(defun condition-key (condition)
  (intern (string (type-of condition)) (find-package :keyword)))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun cat (&rest strings) (apply #'concatenate 'string strings)))

;; Buffers for streams
(defvar *stream-buffers* (make-hash-table))
(defun buffer-of (stream)
  (check-type stream stream)
  (or (gethash stream *stream-buffers*) ""))
(defun (setf buffer-of) (new-value stream)
  (setf (gethash stream *stream-buffers*) new-value))

;; Utility macro - temporary packages
(defmacro with-temp-package (&body body) 
  (let* ((package-name
	   (gensym (cat "TEMP-PKG-"
			(format nil "~S" (local-time:now))
			"-")))
	 (gensym (gensym)))
    `(let ((,gensym (make-package ',package-name)))
       (unwind-protect (let ((*package* ,gensym))
			 ,@body)
	 (delete-package ,gensym)))))

;; Utility macro - creating a safe readtable at compile-time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter %safe-readtable% (copy-readtable))
  (defparameter %max-safe-char% 256)
  (let ((*readtable* %safe-readtable%))
    (flet ((signal-malformed-input (stream ignore)
	     (declare (ignore stream ignore))
	     (error 'malformed-input))) 
      (dotimes (i %max-safe-char%)
	(let* ((char (code-char i))
	       (macro-char (get-macro-character char)))
	  (unless (or (null char)
		      (eql char #\()
		      (eql char #\))
		      (eql char #\")
		      (null macro-char))
	    (set-macro-character char #'signal-malformed-input))))
      (set-macro-character #\: #'signal-malformed-input))))

;; Main exported function
(defun safe-read (&optional (stream *standard-input*))
  (let ((buffer (buffer-of stream)))
    (handler-case
	(if (string= "" buffer)
	    (%safe-read-no-buffer stream)
	    (%safe-read-buffer stream))
      (incomplete-input ()
	(values nil :incomplete-input))
      (error (error)
	(setf (buffer-of stream) "")
	(values nil (condition-key error))))))

;; Handler-case and macro-wrapper for safe reading
(defmacro safe-read-handler-case (&body body)
  (let ((gensym (gensym)))
    `(with-temp-package
       (handler-case
	   (let* ((*readtable* %safe-readtable%)
		  (,gensym (progn ,@body)))
	     (setf (buffer-of stream) "")
	     (values ,gensym nil))
	 (end-of-file ()
	   (setf (buffer-of stream) (cat (buffer-of stream) line (string #\Newline)))
	   (signal (make-condition 'incomplete-input)))
	 (malformed-input (error)
	   (signal error))))))

;; Safe read - buffer
(defun %safe-read-no-buffer (stream)
  (let ((line (read-limited-line stream nil)))
    (safe-read-handler-case
      (read-from-string line))))

;; Safe read function - no buffer
(defun %safe-read-buffer (stream)
  (let* ((line (read-limited-line stream t))
	 (buffer (buffer-of stream))
	 ;;(line (cat line buffer))
	 )
    (safe-read-handler-case
      (read-from-string (cat buffer line)))))

;; Reading from string with a maximum size limit
(defun read-limited-line (stream &optional buffer-p)
  (let* (result-status
	 (line-read
	   (with-output-to-string (result)
	     (do ((char-counter 0)
		  (char (read-char stream nil #\Nul)
			(read-char stream nil #\Nul)))
		 ((member char '(#\Newline #\Nul))
		  (setf result-status (if (eql char #\Newline) :newline :eof)))
	       (cond
		 ((and (null buffer-p) (= char-counter 0) (char/= char #\())
		  (signal (make-condition 'malformed-input)))
		 ((< *max-input-size* (incf char-counter))
		  (signal (make-condition 'input-size-exceeded)))
		 (t
		  (princ char result)))))))
    (values line-read result-status)))
