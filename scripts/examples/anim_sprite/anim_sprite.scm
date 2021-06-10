(include-relative "../../extensions/debug")

(define *sprite*)
(define *texture*)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *texture* (texture:load "assets/textures/animated-sprite.png"
				       "texture_diffuse"))
  (set! *sprite* (sprite:create 128 128))
  (sprite:load_texture *sprite*
		       *texture*)
  )

(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(1 1 1))
  (shader:use (shader:default))

  (renderer:draw *sprite*
		 draw_rect:(list 0 0 128 128))
  
  (window:swap))

(define (destroy)
  (texture:unload *texture*))
