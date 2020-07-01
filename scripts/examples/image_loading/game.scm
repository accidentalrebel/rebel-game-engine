(define (init)
  (set! *sprite* (sprite:create 1.0 1.0 1.0))

  ;; TODO; Not specifying the third parameter for material:load_texture should default to "texture_diffuse"
  ;; TODO; Consider changing the third parameter to an Enum
  (sprite:add_texture *sprite*
   		      (material:load_texture "assets/textures/" "awesomeface.png"))
  #t)

(define (update)
  #t)

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (sprite:draw *sprite* '(0 0 0) '(1 1 1))
  
  (window:swap))
