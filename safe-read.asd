;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; safe-read-list.asd

(asdf:defsystem #:safe-read
  :description "A variant of READ secure against internbombing, excessive input and macro characters."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 2-clause"
  :depends-on (#:local-time
               #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "safe-read")))

