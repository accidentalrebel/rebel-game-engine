(define g-spr 0)
(define g-current-pos 0)
(define g-current-color 0)

(define (init)
  (set! g-spr (create-sprite "assets/textures" "tile.png"))
  (set! g-current-pos (make-vec3 400.0 300.0 0.0))
  (set! g-current-color (make-vec3 1.0 0.0 1.0))
  #t)

(define (draw)
  (draw-sprite g-spr g-current-pos 50.0 50.0)
  #t)
