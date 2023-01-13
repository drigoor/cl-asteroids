(in-package #:cl-asteroids)


(define-class shape ()
  location
  velocity ;; speed -> x, direction -> y
  aceleration
  deceleration
  orientation
  orientation-speed
  thickness
  color
  points
  life-timer)


(define-constructor shape)


(defmethod draw ((this shape))
  (with-slots (location orientation thickness color points) this
    (let ((start nil))
      (dolist (pt points)
        (let ((end (v2-rotate pt orientation)))
          (when start
            (draw-line-ex (v2-add location start)
                          (v2-add location end)
                          thickness
                          color))
          (setf start end))))
    (draw-circle-v location thickness +red+)))
