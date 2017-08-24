;;;; neuro-recovery.asd

(asdf:defsystem #:neuro-recovery
  :description "Recover neuroshima materials from the internet"
  :author "Mateusz Malisz"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:drakma
               #:cl-html5-parser
               #:css-selectors
               #:css-selectors-simple-tree
               #:cl-interpol
               #:alexandria
               #:serapeum
               #:lparallel
               #:puri
               #:log4cl)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "neuro-recovery")))
