(define g-spr 0)

(define (init)
  (set! g-spr (create-sprite "assets/textures" "tile.png"))
  #t)

(define (draw)
  (draw-sprite g-spr 50.0 50.0)
  )
