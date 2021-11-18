(asdf:defsystem #:cl-asteroids
  :description "A Common Lisp Asteroids Clone"
  :author "Rodrigo Correia <https://github.com/drigoor>"
  :license "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-game-spell)
  :components ((:file "package")
               (:file "shape")
               (:file "game")
               (:file "main")))
