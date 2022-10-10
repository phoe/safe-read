;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READ
;;;; © Michał "phoe" Herda 2016
;;;; safe-read.lisp

;; Lisp reader hackery below. Beware.
(in-package #:safe-read)

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
         (package-name (gensym (uiop:strcat "TEMP-PKG-" now "-")))
         (package-var (gensym))
         (use (if (eq (first body) :use-list)
                  (prog1 (second body)
                    (setf body (cddr body))))))
                    
    `(let ((,package-var (or (find-package ',package-name)
                             (make-package ',package-name :use ,use))))
       (unwind-protect (let ((*package* ,package-var)) ,@body)
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
(defun safe-read (&optional (stream *standard-input*) use-list (interactive t))
  (cond (interactive
         (let ((buffer (buffer-of stream)))
           (handler-case
               (if (string= "" buffer)
                   (safe-read-no-buffer stream use-list)
                 (safe-read-buffer stream use-list))
             (incomplete-input ()
               (values nil :incomplete-input))
             (end-of-file (e)
               (error e))
             (error (error)
               (setf (buffer-of stream) "")
               (error error)))))
        (t
         (safe-read-non-interactive stream use-list))
        ))

;; Handler-case and macro-wrapper for safe reading
(defmacro safe-non-interactive-read-handler-case (&body body)
  (let ((gensym (gensym)))
    `(with-temp-package ,@(if (eq (first body) :use-list)
                              (prog1 (list :use-list (second body))
                                (setf body (cddr body))))
       (let* ((*readtable* %safe-readtable%))
         (progn
           ,@body)))
    ))

(defun safe-read-non-interactive (stream use-list)
  (let ((line (trim-leading-whitespace (read-limited-line stream))))
    (safe-non-interactive-read-handler-case :use-list use-list
      (read-from-string line))))


;; Handler-case and macro-wrapper for safe reading
(defmacro safe-read-handler-case (&body body)
  (let ((gensym (gensym)))
    `(with-temp-package ,@(if (eq (first body) :use-list)
                              (prog1 (list :use-list (second body))
                                (setf body (cddr body))))
       (handler-case
           (flet ((clear-buffer (e)
                    (declare (ignore e))
                    (setf (buffer-of stream) "")))
             (handler-bind ((malformed-input #'clear-buffer))
               (let* ((*readtable* %safe-readtable%)
                      (,gensym (progn ,@body)))
                 (setf (buffer-of stream) "")
                 (values ,gensym nil))))
         (end-of-file ()
           (unless (string= line "")
             (setf (buffer-of stream)
                   (uiop:strcat (buffer-of stream) line (string #\Newline))))
           (signal (make-condition 'incomplete-input)))))))

;; Safe read - no buffer
(defun safe-read-no-buffer (stream &optional use-list)
  (let ((line (trim-leading-whitespace (read-limited-line stream))))
    (safe-read-handler-case :use-list use-list
      (read-from-string line))))

;; Safe read - buffer
(defun safe-read-buffer (stream &optional use-list)
  (let* ((buffer (buffer-of stream))
         (line (read-limited-line stream (length buffer))))
    (safe-read-handler-case :use-list use-list
      (read-from-string (uiop:strcat buffer line)))))

;; Reading from string with a maximum size limit
(defun read-limited-line (&optional (stream *standard-input*) (buffer-length 0))
  (with-output-to-string (result)
    (let ((char-counter buffer-length) char)
      (loop
        (setf char (read-char-no-hang stream nil :eof))
        (cond ((null char)
               (return))
              ((eq char #\Newline)
               (return))
              ((and (eq char :eof) (= 0 char-counter))
               (error 'end-of-file :stream stream))
              ((and (eq char :eof) (/= 0 char-counter))
               (return))
              ((and (= 0 buffer-length) (= 0 char-counter) (whitespace-p char))
               nil)
              ((and (= 0 buffer-length) (= 0 char-counter) (char/= #\( char))
               (error (make-condition 'malformed-input)))
              ((< *max-input-size* (incf char-counter))
               (error (make-condition 'input-size-exceeded)))
              (t (princ char result)))))))
