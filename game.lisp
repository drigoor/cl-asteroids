(in-package #:cl-asteroids)


(defmacro define-game-object (name)
  `(progn
     (define-class ,name (actable drawable shape))
     (define-constructor ,name)))


(defun generate-player-points ()
  (<-v2  20   0
        -20  15
        -10   0
        -20 -15
         20   0))


(define-game-object player)


(defun spawn-player (location)
  (make-player :location location
               :orientation-speed 0.07
               :velocity (v2 0 0) ;; speed -> x, direction -> y
               :orientation 0.0
               :orientation-speed 0.0
               :thickness 2.0
               :color +lightgray+
               :points (generate-player-points)))


(defmethod act ((this player))
  (with-slots (orientation orientation-speed) this
    (when (or (is-key-down +key-a+)
              (is-key-down +key-left+))
      (decf orientation orientation-speed))
    (when (or (is-key-down +key-d+)
              (is-key-down +key-right+))
      (incf orientation orientation-speed))))


(defconstant +asteroid-num-points+ 10)
(defconstant +asteroid-rad+ 15)
(defconstant +asteroid-rad-plus+ 4)
(defconstant +asteroid-rad-minus+ 6)
(defconstant +asteroid-max-vel+ 0.5)
(defconstant +asteroid-min-vel+ 0.1)
(defconstant +asteroid-max-rot+ 0.03)


(defun v2-from-speed-and-direction (speed direction)
  (v2 (* speed (cos direction))
      (* speed (sin direction))))


(defun generate-asteroid-points (scale)
  (let ((result nil))
    ;; first point
    (push (v2 (/ +asteroid-rad+ scale) 0)
          result)
    ;; midle points
    (dotimes (index (- +asteroid-num-points+ 2)) ; index will start in 1 (see 1+ bellow)
      (let ((speed (/ (random-between (- +asteroid-rad+ +asteroid-rad-minus+)
                                      (+ +asteroid-rad+ +asteroid-rad-plus+))
                      scale))
            (direction (* (1+ index)
                          (/ (* +pi+ 2)
                             +asteroid-num-points+))))
        ;; (push (vector2-scale (vec2-from-speed-and-direction speed direction) size)
        (push (v2-from-speed-and-direction speed direction)
              result)))
    ;; last point
    (push (v2 (/ +asteroid-rad+ scale) 0)
          result)
    result))


(define-game-object asteroid)


(defun spawn-asteroid (location scale)
  (make-asteroid :location location
                 :orientation-speed 0.07
                 :velocity (v2 (random-between +asteroid-min-vel+ +asteroid-max-vel+) ;; speed -> x, direction -> y
                               (random-between 0 (* 2 +pi+)))
                 :orientation (random-between 0 +pi+)
                 :orientation-speed (- (* (random 2.0) (* 2 +asteroid-max-rot+)) +asteroid-max-rot+)
                 :thickness 2.0
                 :color +lightgray+
                 :points (generate-asteroid-points scale)))


(defmethod act ((this asteroid))
  )


(define-scene game)


(defmethod draw ((this game))
  (clear-background +raywhite+))


(defmethod init ((this game))
  (spawn-player (v2 (/ (get-screen-width) 2) (/ (get-screen-height) 2)))
  (spawn-asteroid (v2 100 200) 0.5)
  (spawn-asteroid (v2 100 300) 0.6)
  (spawn-asteroid (v2 100 400) 0.8))
