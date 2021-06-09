(define *sprite*)
(define *model-shader*)

(define (init)
  (set! *sprite*
	(sprite:create 1.0 1.0))
  (sprite:add_texture *sprite* 
		      (texture:load "assets/textures/" "awesomeface.png" "texture_diffuse"))
  (set! *model-shader*
	(shader:create "shaders/image-loading.vs" "shaders/image-loading.fs")))

(define (update)
  #t)

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (shader:use *model-shader*)
  (renderer:draw *sprite* '(0 0 0) '(1 1 1))
  
  (window:swap))
