(in-package #:cl-asteroids)


(defun v2-from-speed-and-direction (speed direction)
  (v2 (* speed (cos direction))
      (* speed (sin direction))))


(defun get-vector-components (v2)
  (let ((speed (v2-x v2))
        (direction (v2-y v2)))
    (v2-from-speed-and-direction speed direction)))


(defun move-location-by-velocity (shape)
  (with-slots (location velocity) shape
    (v2-add location (get-vector-components velocity))))


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
               :velocity (v2 0 0) ;; speed -> x, direction -> y
               :aceleration 0.05
               :deceleration 0.01
               :orientation 0.0
               :orientation-speed 0.07
               :thickness 2.0
               :color +lightgray+
               :points (generate-player-points)))


(defun comp-to-vector (v2)
  (let* ((x (v2-x v2))
         (y (v2-y v2))
         (magnitude (sqrt (+ (* x x) (* y y))))
         (direction (keep-angle-in-range (atan y x))))
    (v2 magnitude direction)))


(defun add-vectors (v2-1 v2-2)
  (comp-to-vector (v2-add (get-vector-components v2-1)
                          (get-vector-components v2-2))))


(defun player-thrust (player)
  (with-slots (velocity orientation aceleration) player
    (let ((aceleration-vector (v2 aceleration orientation)))
      (setf velocity (add-vectors velocity aceleration-vector)))))


(defmethod act ((this player))
  (when (or (is-key-down +key-w+)
              (is-key-down +key-up+))
    (player-thrust this))
  (with-slots (orientation orientation-speed) this
    (when (or (is-key-down +key-a+)
              (is-key-down +key-left+))
      (decf orientation orientation-speed)
      (setf orientation (keep-angle-in-range orientation)))
    (when (or (is-key-down +key-d+)
              (is-key-down +key-right+))
      (incf orientation orientation-speed)
      (setf orientation (keep-angle-in-range orientation))))
  ;; update movement (equal to asteroids...) or better, generic to shape
  (with-slots (location velocity deceleration) this
    (let ((speed (v2-x velocity))
          (direction (v2-y velocity)))
      (decf speed deceleration)
      (when (< speed 0.0)
        (setf speed 0.0))
      (setf velocity (v2 speed direction))
      (setf location (v2-wrap (move-location-by-velocity this))))))


(defconstant +asteroid-num-points+ 10)
(defconstant +asteroid-rad+ 15)
(defconstant +asteroid-rad-plus+ 4)
(defconstant +asteroid-rad-minus+ 6)
(defconstant +asteroid-max-vel+ 0.5)
(defconstant +asteroid-min-vel+ 0.1)
(defconstant +asteroid-max-rot+ 0.03)


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
                 :velocity (v2 (random-between +asteroid-min-vel+ +asteroid-max-vel+) ;; speed -> x, direction -> y
                               (random-between 0 (* 2 +pi+)))
                 :aceleration 0.0 ;; not in use
                 :deceleration 0.0 ;; not in use
                 :orientation (random-between 0 +pi+)
                 :orientation-speed (- (* (random 2.0) (* 2 +asteroid-max-rot+)) +asteroid-max-rot+)
                 :thickness 2.0
                 :color +lightgray+
                 :points (generate-asteroid-points scale)))


(defmethod act ((this asteroid))
  (with-slots (location orientation orientation-speed) this
    (incf orientation orientation-speed)
    (setf location (v2-wrap (move-location-by-velocity this)))))


(define-scene game)


(defmethod draw ((this game))
  (clear-background +raywhite+))


(defmethod init ((this game))
  (spawn-player (v2 (/ (get-screen-width) 2) (/ (get-screen-height) 2)))
  (spawn-asteroid (v2 100 200) 0.5)
  (spawn-asteroid (v2 100 300) 0.6)
  (spawn-asteroid (v2 100 400) 0.8))
