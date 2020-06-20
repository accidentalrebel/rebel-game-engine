(define *cube*)
(define *box-shader*)
(define *light-shader*)
(define *angle* 6.282)

(define *point-lights* '())

; Setup the first person camera extension
(include-relative "../../extensions/fpcam")

(define (init)
  ; Setup the point lights
  (set! *point-lights*
	(list
	 (light:point:create2 '(0.0 1.0 0.0) '(1.0 1.0 1.0))
	 (light:point:create2 '(-1.0 1.0 1.0) '(1.0 0.0 0.0))
	 (light:point:create2 '(0.0 1.0 -1.0) '(0.0 1.0 0.0))
	 (light:point:create2 '(1.0 1.0 1.0) '(0.0 0.0 1.0))))
  
  ;; Generate a cube mesh and load it as a model
  (set! *cube* (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))
  (set! *light-cube* (model:load_from_mesh (mesh:generate_cube 0.25 0.25 0.25)))

  ;; Load the texture and assign is as a texture diffuse
  (model:texture_diffuse! *cube* (texture:load "assets/textures/texel-checker.png"))

  ;; Setup the shaders
  (set! *box-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! *light-shader* (shader:create "shaders/light-shader.vs" "shaders/light-shader.fs")))

(define (update)
  (fpcam:update))

(define (render)
  (window:clear '(0.1 0.1 0.1))

  (shader:use *box-shader*)

  (model:draw *cube* '(0.0 0.0 0.0) '(1.0 1.0 1.0))
  (model:draw *cube* '(1.5 0.0 0.0) '(1.0 1.0 1.0))
  (model:draw *cube* '(-1.5 0.0 0.0) '(1.0 1.0 1.0))

  (shader:use *light-shader*)

  (set! *angle* (- *angle* (* 2 (time:elapsed))))

  (orbit_point_light (first *point-lights*) 1.0 2.0)
  (orbit_point_light (second *point-lights*) -0.6 2.0)
  (orbit_point_light (third *point-lights*) 0.2 2.0)
  (orbit_point_light (fourth *point-lights*) 0.35 2.0)
  
  ;; Draw each point-light
  (for-each
   (lambda (point-light)
     (model:draw *light-cube*
		 (light:point:position point-light)
		 (light:point:diffuse point-light)))
   *point-lights*)

  (window:swap))

(define (orbit_point_light point-light multiplier distance)
  (let* ((pos (light:point:position point-light)))
    (light:point:position! point-light
			   (list
			    (* (cos (* *angle* multiplier)) distance)
			    (second pos)
			    (* (sin (* *angle* multiplier)) distance)))))
