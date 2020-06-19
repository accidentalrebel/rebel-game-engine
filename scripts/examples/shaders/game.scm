(include-relative "../../extensions/fpcam")

(define *cube*)
(define *box-shader*)
(define *light-shader*)
(define *angle* 6.282)

(define *point-lights* '())

(define (init)
  (set! *point-lights*
	(list 
	 (light:point:create2 '(-1.0 1.0 1.0) '(1.0 0.0 0.0))
	 (light:point:create2 '(0.0 1.0 -1.0) '(0.0 1.0 0.0))
	 (light:point:create2 '(1.0 1.0 1.0) '(0.0 0.0 1.0))))
  
  ;; Generate a cube mesh and load it as a model
  ;; TODO; Fix the mesh normals
  (set! *cube* (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))
  (set! *light-cube* (model:load_from_mesh (mesh:generate_cube 0.25 0.25 0.25)))

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

  (model:draw *cube* '(0.0 0.0 0.0) '(1.0 1.0 1.0))

  (shader:use *light-shader*)

  (set! *angle* (- *angle*
		   (* 2 (time:elapsed))))

  (let* ((point-light (first *point-lights*))
	 (pos (light:point:position point-light)))
    (light:point:position! point-light
			   (list
			    (* (cos *angle*) 4)
			    (second pos)
			    (* (sin *angle*) 4))))
  
  ;; Draw each point-light
  (for-each
   (lambda (point-light)
     (model:draw *light-cube*
		 (light:point:position point-light)
		 (light:point:diffuse point-light)))
   *point-lights*)

  (window:swap)
  )
