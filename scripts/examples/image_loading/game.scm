(define *sprite*)
(define *model-shader*)

;; Setup the first person camera extension
(include-relative "../../extensions/fpcam")
(include-relative "../../extensions/debug")

(define (init)
  ;; sprite:create is just a convenience method for creating sprites
  ;; it stull uses the model/mesh system
  (set! *sprite* (sprite:create 1.0 1.0))

  ;; TODO; Not specifying the third parameter for material:load_texture should default to "texture_diffuse"
  ;; TODO; Consider changing the third parameter to an Enum
  (model:add_texture *sprite*
  		     (material:load_texture "assets/textures/" "awesomeface.png" "texture_diffuse"))

  (set! *model-shader*
	(shader:create "shaders/image-loading.vs" "shaders/image-loading.fs"))
  )

(define (update)
  ;; This is needed for the first person camera extension to work
  (fpcam:update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (shader:use *model-shader*)

  (renderer:draw *sprite* '(0.5 0 -0.5) '(1 1 1))
  (renderer:draw *sprite* '(0 0 0) '(1 1 1))
  
  (window:swap))
