(define g-spr 0)
(define g-current-pos 0)
(define g-current-color 0)

(define (init)
  (set! g-spr (create-sprite "assets/textures" "tile.png"))
  (set! g-current-pos (make-vec3 400.0 300.0 0.0))
  (set! g-current-color (make-vec3 1.0 0.0 1.0))
  
  #t)

(define (draw)
  (if (is-key-down KEY_COMMA)
      (vec3-set-y g-current-pos
  		  (+ (vec3-get-y g-current-pos) 1)))
  (if (is-key-down KEY_O)
      (vec3-set-y g-current-pos
  		  (- (vec3-get-y g-current-pos) 1)))
  
  (if (is-key-down KEY_E)
      (vec3-set-x g-current-pos
  		  (+ (vec3-get-x g-current-pos) 1)))
  (if (is-key-down KEY_A)
      (vec3-set-x g-current-pos
  		  (- (vec3-get-x g-current-pos) 1)))

  (draw-sprite g-spr g-current-pos 50.0 50.0 g-current-color)

  #t)
