(include-relative "../../extensions/fpcam")

(define *cube*)
(define *box-shader*)
(define *light-shader*)

(define *point-lights* '())

(define (init)
  (set! *point-lights*
	(list 
	 (light:point:create2 '(-1.0 1.0 1.0) '(1.0 0.0 0.0))
	 (light:point:create2 '(0.0 1.0 1.0) '(0.0 1.0 0.0))
	 (light:point:create2 '(1.0 1.0 1.0) '(0.0 0.0 1.0))))
  
  ;; Generate a cube mesh and load it as a model
  ;; TODO; Fix the mesh normals
  (set! *cube* (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))
  (set! *light-cube* (model:load_from_mesh (mesh:generate_cube 0.5 0.5 0.5)))

  ;; Load the texture and assign is as a texture diffuse
  (model:texture_diffuse! *cube* (texture:load "assets/textures/texel-checker.png"))

  ;; TODO; Make own shader for this example only
  (set! *box-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! *light-shader* (shader:create "shaders/light-shader.vs" "shaders/light-shader.fs"))
  )

(define (update)
  (fpcam:update)
  #t
  )

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (shader:use *box-shader*)

  (model:draw *cube* '(0.0 0.0 0.0))

  (shader:use *light-shader*)

  (model:draw *light-cube*
	      (light:point:position
	       (first *point-lights*)))

  (window:swap)
  )
