(include-relative "extensions/fpcam") ;; Setup first person camera movement

(define *cube*)
(define *sprite*)
(define *tile*)
(define *light*)

(define *cube-shader*)
(define *sprite-shader*)
(define *tile-shader*)
(define *light-shader*)

(define *basic-shader*)

(define *cube-positions*)

(define *TILE_MAP_COLS* 8)
(define *TILE_MAP_ROWS* 8)
(define *tile-map*
  '(0 0 0 0 0 0 0 0
      0 1 0 0 0 0 0 0
      0 0 0 0 0 2 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 1 0 0
      0 2 1 0 0 0 0 0
      0 0 0 0 0 0 0 0))

(define *point-lights* '())

(define (init)
  (light:directional:create
   '(1.0 -1.0 1.0)
   '(0.05 0.05 0.05)
   '(0.1 0.1 0.1)
   '(0.2 0.2 0.2))
  
  (set! *point-lights*
	(list
	 (light:point:create
	  '(2.0 -0.25 2.0)
	  '(0.1 0.05 0.05)
	  '(0.8 0.2 0.2)
	  '(1.0 0.2 0.2)
	  1.0 0.7 1.8)
	 
	 (light:point:create
	  '(5.0 -0.75 2.0)
	  '(0.05 0.05 0.1)
	  '(0.2 0.8 0.2)
	  '(0.2 0.2 1.0)
	  1.0 0.7 1.8)

	 (light:point:create
	  '(2.0 0.75 6.0)
	  '(0.05 0.05 0.1)
	  '(0.2 0.2 0.8)
	  '(0.2 0.2 1.0)
	  1.0 0.7 1.8)

	 (light:point:create
	  '(5.0 -0.25 6.0)
	  '(0.1 0.1 0.05)
	  '(0.8 0.8 0.2)
	  '(1.0 1.0 0.2)
	  1.0 0.7 1.8)))

  (set! *tile* (cube:create))
  (set! *tile-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *tile* (texture:load "assets/textures/texel-checker.png"))
  (material:texture_specular! *tile* (texture:load "assets/textures/container-specular.png"))
  (material:shininess! *tile* 12.0)

  (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0))
  (texture:load "assets/textures/texel-checker.png")

  ;; (set! *cube* (cube:create))
  ;; (set! *cube-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  ;; (material:texture_diffuse! *cube* (texture:load "assets/textures/container.png"))
  ;; (material:texture_specular! *cube* (texture:load "assets/textures/container-specular.png"))
  ;; (material:shininess! *cube* 64.0)

  (set! *light* (cube:create))
  (set! *light-shader* (shader:create "shaders/light-shader.vs" "shaders/light-shader.fs"))

  (set! *basic-shader* (shader:create "shaders/basic.vs" "shaders/basic.fs"))

  (set! *sprite* (sprite:create))
  (set! *sprite-shader*	(shader:create "shaders/simple.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *sprite* (texture:load "assets/textures/awesomeface.png"))

  ;; (set! *cube-positions*
  ;; 	(list (vec3:create 0.0 0.0 0.0)
  ;; 	      (vec3:create 2.25 0.0 0.0)
  ;; 	      (vec3:create -1.25 0.0 0.0)))
  (display (first (camera:position (camera:main))))
  )

(define (update)
  (fpcam:update)

  (when (key:up? keys/G)
    (display "GC triggered.\n")
    (display (gc #t))
    (newline)))

(define (render)
  (window:clear)

  (shader:use *tile-shader*)
  (rebel:draw)
  
  ;; (shader:use *tile-shader*)

  (let ((current-col 0)
  	(current-row 0))
    (for-each
     (lambda (tile-value)
       ;; TODO; The width and height of the object should probably be moved to the renderer?
       (renderer:draw *tile*
  		      (list current-col (sub1 tile-value) current-row)
  		      1.0 1.0)

       (set! current-col (add1 current-col))
       (when (>= current-col *TILE_MAP_COLS*)
  	 (set! current-col 0)
  	 (set! current-row (add1 current-row))))
     *tile-map*))

  (shader:use *sprite-shader*)
  (renderer:draw *sprite*
  		 '(1.0 0.0 1.51)
  		 1.0 1.0)

  (shader:use *light-shader*)

  (for-each
   (lambda (point-light)
     (material:color! *light* (light:point:diffuse point-light))
     (renderer:draw *light*
		    (light:point:position point-light)
		    0.5 0.5))
   *point-lights*)

  (shader:use (shader:default))

  (window:swap))
