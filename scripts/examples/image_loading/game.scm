(define *sprite*)
(define *model-shader*)

(display ">> AYO")

;; Setup the first person camera extension
(include-relative "../../extensions/fpcam")

(display ">> YO")

(define (init)
  (display ">> 1")
  (set! *sprite*
	(model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))

  (display ">> 2")
  ;; TODO; Not specifying the third parameter for material:load_texture should default to "texture_diffuse"
  ;; TODO; Consider changing the third parameter to an Enum
  (model:add_texture *sprite*
  		     (material:load_texture "assets/textures/" "awesomeface.png" "texture_diffuse"))

  (display ">> 3")
  (set! *model-shader*
	(shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  	;; (shader:create "shaders/model-loading.vs" "shaders/model-loading.fs"))
  )

(define (update)
  ;; This is needed for the first person camera extension to work
  (fpcam:update)
  #t)

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (shader:use *model-shader*)

  (model:draw *sprite* '(0 0 0) '(1 1 1))
  
  (window:swap))
