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

(defvar *max-input-size* (* 128 1024))

;; Utility functions
(defun condition-key (condition)
  (intern (string (type-of condition)) (find-package :keyword)))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Backspace #\Tab 
                 #\Linefeed #\Page #\Return #\Rubout)))

(defun trim-leading-whitespace (string)
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab 
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-left-trim whitespace string)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun cat (&rest strings) (apply #'concatenate 'string strings)))

;; Buffers for streams
(defvar *stream-buffers* (make-weak-hash-table :weakness :key))

(defun buffer-of (stream)
  (check-type stream stream)
  (or (gethash stream *stream-buffers*) ""))

(defun (setf buffer-of) (new-value stream)
  (if (or (null new-value) (string= new-value ""))
      (remhash stream *stream-buffers*)
      (setf (gethash stream *stream-buffers*) new-value)))

;; Utility macro - temporary packages
(defmacro with-temp-package (&body body)
  (let* ((now (format nil "~S" (local-time:now)))
	 (package-name (gensym (cat "TEMP-PKG-" now "-")))
	 (package-var (gensym)))
    `(let ((,package-var (make-package ',package-name :use nil)))
       (unwind-protect (let ((*package* ,package-var))
			 ,@body)
	 (delete-package ,package-var)))))

;; Utility macro - creating a safe readtable at compile-time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter %safe-readtable% (copy-readtable))
  (defparameter %max-safe-char% 256)
  (let ((*readtable* %safe-readtable%))
    (flet ((signal-malformed-input (stream char)
	     (declare (ignore stream char))
	     (error 'malformed-input))
	   (eat-colon (stream char)
	     (declare (ignore char))
	     (if (eq #\: (read-char-no-hang stream))
		 (read stream)
		 (error 'malformed-input))))
      (dotimes (i %max-safe-char%)
	(let* ((char (code-char i))
	       (macro-char (get-macro-character char)))
	  (unless (or (null char)
		      (member char '(#\( #\) #\"))
		      (null macro-char))
	    (set-macro-character char #'signal-malformed-input))))
      (set-macro-character #\: #'signal-malformed-input)
      (set-macro-character #\# #'eat-colon))))

;; Main exported function
(defun safe-read (&optional (stream *standard-input*))
  (let ((buffer (buffer-of stream)))
    (handler-case
	(if (string= "" buffer)
	    (safe-read-no-buffer stream)
	    (safe-read-buffer stream))
      (incomplete-input ()
	(values nil :incomplete-input))
      (end-of-file (e)
        (error e))
      (error (error)
	(setf (buffer-of stream) "")
        ;;(format t "[!] SAFE-READ: ~S~%" (condition-key error))
        ;;(values nil (condition-key error))
        (error error)))))

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
           (unless (string= line "")
             (setf (buffer-of stream) (cat (buffer-of stream) line (string #\Newline))))
           (signal (make-condition 'incomplete-input)))
         (malformed-input (error)
           (setf (buffer-of stream) "")
           (signal error))))))

;; Safe read - buffer
(defun safe-read-no-buffer (stream)
  (let ((line (trim-leading-whitespace (read-limited-line stream))))
    (safe-read-handler-case
      (read-from-string line))))

;; Safe read function - no buffer
(defun safe-read-buffer (stream)
  (let* ((buffer (buffer-of stream))
	 (line (read-limited-line stream (length buffer))))
    (safe-read-handler-case
      (read-from-string (cat buffer line)))))

;; Reading from string with a maximum size limit
(defun read-limited-line (&optional (stream *standard-input*) (buffer-length 0))
  (with-output-to-string (result)
    (handler-case
        (do ((char-counter buffer-length)
             (char (read-char stream) (read-char stream)))
            ((member char '(#\Newline #\Nul)))
          (cond ((and (= 0 buffer-length) (= 0 char-counter) (whitespace-p char))
                 nil)
                ((and (= 0 buffer-length) (= 0 char-counter) (char/= #\( char))
                 (signal (make-condition 'malformed-input)))
                ((< *max-input-size* (incf char-counter))
                 (signal (make-condition 'input-size-exceeded)))
                (t (princ char result))))
      (end-of-file ()))))

