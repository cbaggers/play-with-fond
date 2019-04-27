;;;; play-with-fond.asd

(asdf:defsystem #:play-with-fond
  :description "Describe play-with-fond here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cepl.sdl2
               #:rtg-math
               #:rtg-math.vari
               #:cl-fond)
  :components ((:file "package")
               (:file "cl-fond")
               (:file "cl-fond2")))
