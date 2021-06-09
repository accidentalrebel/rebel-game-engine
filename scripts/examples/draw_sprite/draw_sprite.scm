(define (init)
  (set! *sprite*
	(sprite:create 1.0 1.0))
  ;; (sprite:add_texture *sprite* 
  ;; 		      (texture:load "assets/textures/" "awesomeface.png" "texture_diffuse"))
  #t)

(define (update)
  #t)

(define (render)
  (window:clear '(1.0 0.1 0.1))

  
  
  (window:swap))
