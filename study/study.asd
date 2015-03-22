;;;; study.asd




(asdf:defsystem #:study
  :description "Describe study here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:qt)
  :serial t
  :components ((:file "package")
               (:file "study")))

