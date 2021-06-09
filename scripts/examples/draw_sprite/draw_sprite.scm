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
  
  (renderer:draw *sprite*
		 position:'(1 0 0)
		 rotation:'(0 0 180)
		 color:'(1 1 1 0.5))
  
  (renderer:draw *sprite*
		 rotation:'(0 0 90)
		 color:'(1 1 1 0.75))
  
  (renderer:draw *sprite*
		 position:'(-1 0 0))

  (renderer:draw *sprite*
		 position:'(1 -1 0)
		 rotation:'(0 0 180)
		 color:'(0 0 1 1))
  
  (renderer:draw *sprite*
		 position:'(0 -1 0)
		 rotation:'(0 0 90)
		 color:'(0 1 0 1))
  
  (renderer:draw *sprite*
		 position:'(-1 -1 0)
		 color:'(1 0 0 1))
  
  (window:swap))

(define (destroy)
  (texture:unload *texture*))
