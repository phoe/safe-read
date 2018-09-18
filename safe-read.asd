;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; safe-read-list.asd

(asdf:defsystem #:safe-read
  :description "A variant of READ secure against internbombing, excessive input
and macro characters."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "BSD 2-clause"
  :depends-on (#:local-time
               #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "safe-read")))

(asdf:defsystem #:safe-read/test
  :description "Test suite for SAFE-READ"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "BSD 2-clause"
  :depends-on (:safe-read)
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :safe-read :test)))
