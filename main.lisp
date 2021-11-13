(in-package #:asteroids)


(define-scene game)


;; (defmethod init ((this game))
;;   )


;; (defmethod uninit ((this game))
;;   )


(defun vec2 (x y)
  (make-vector2 :x (coerce x 'single-float) :y (coerce y 'single-float)))


(defmethod draw ((this game))
  (clear-background +raywhite+)
  (draw-circle-v (vec2 100 100) 3.0 +red+))


;; (defmethod act ((this game))
;;   )


(defun run (width height title)
  (let ((scene (make-game)))
    (with-scene scene
      (with-window (width height title)
        (set-target-fps 60)
        (init scene)
        (loop
          (when (window-should-close)   ; dectect window close button or ESC key
            (uninit scene)
            (return))
          (act scene)
          (with-drawing
            (draw scene)))))))


;; (asteroids:run 800 600 "Asteroids")
