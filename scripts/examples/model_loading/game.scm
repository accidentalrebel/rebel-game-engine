(define *basic-shader*)

;; Setup the first person camera extension
(include-relative "../../extensions/fpcam")

(define (init)
  (set! *basic-shader* (shader:create "shaders/basic.vs" "shaders/basic.fs")))

(define (update)
  (fpcam:update))

(define (render)
  (window:clear '(0.1 0.1 0.1))
  (window:swap)
  )
