(define *sprite*)
(define *texture*)

(define (init)
  (set! *sprite*
	(sprite:create 1.0 1.0))
  (set! *texture*
	(texture:load "assets/textures/awesomeface.png" "texture_diffuse"))
  (sprite:load_texture *sprite* *texture*))

(define (update)
  #t)

(define (render)
  (window:clear '(1 1 1))

  (shader:use (shader:default))
  (renderer:draw *sprite* '(1 0 0) '(0 0 180) '(1 1 1 0.5))
  (renderer:draw *sprite* '(0 0 0) '(0 0 90) '(1 1 1 0.75))
  (renderer:draw *sprite* '(-1 0 0) '(0 0 0) '(1 1 1 1))

  (renderer:draw *sprite* '(1 -1 0) '(0 0 180) '(0 0 1 1))
  (renderer:draw *sprite* '(0 -1 0) '(0 0 90) '(0 1 0 1))
  (renderer:draw *sprite* '(-1 -1 0) '(0 0 0) '(1 0 0 1))
  
  (window:swap))

(define (destroy)
  (texture:unload *texture*))
