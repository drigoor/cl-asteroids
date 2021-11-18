(in-package #:cl)


(defpackage #:cl-asteroids
  (:nicknames #:asteroids)
  (:use #:cl
        #:cl-raylib
        #:cl-game-spell)
  (:export #:run))
