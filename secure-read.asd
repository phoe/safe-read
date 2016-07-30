;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; secure-read-list.asd

(asdf:defsystem #:secure-read
  :description "A variant of READ secure against internbombing, excessive input and macro characters."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 2-clause"
  :depends-on (#:local-time)
  :serial t
  :components ((:file "package")
               (:file "secure-read")))

