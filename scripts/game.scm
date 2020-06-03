(include-relative "extensions/fpcam")

(define *cube*)
(define *cube-shader*)
(define *sprite*)
(define *sprite-shader*)
(define *cube-positions*)
(define *tile*)
(define *tile-shader*)

(define *light*)
(define *light-shader*)

(define *TILE_MAP_COLS* 8)
(define *TILE_MAP_ROWS* 8)
(define *tile-map*
  '(0 0 0 0 0 0 0 0
      0 1 0 0 0 0 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 1 0 0
      0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0))

(define gpoint-lights* '())

(define (init)
  (light:directional:create
   (vec3:create 1.2 -1.0 -1.0)
   (vec3:create 0.05 0.05 0.05)
   (vec3:create 0.4 0.4 0.4)
   (vec3:create 0.5 0.5 0.5))
  
  (set! *point-lights*
	(list
	 (light:point:create
	  (vec3:create 7.0 0.0 4.0)
	  (vec3:create 0.1 0.05 0.05)
	  (vec3:create 0.8 0.2 0.2)
	  (vec3:create 1.0 0.2 0.2)
	  1.0 0.09 0.032)

	 (light:point:create
	  (vec3:create -1.25 0.0 -1.0)
	  (vec3:create 0.05 0.05 0.1)
	  (vec3:create 0.2 0.2 0.8)
	  (vec3:create 0.2 0.2 1.0)
	  1.0 0.09 0.032)))

  (set! *tile* (cube:create))
  (set! *tile-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *tile* (texture:load "assets/textures/tile.png"))
  (material:texture_specular! *tile* (texture:load "assets/textures/container-specular.png"))
  (material:shininess! *tile* 12.0)

  (set! *cube* (cube:create))
  (set! *cube-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *cube* (texture:load "assets/textures/container.png"))
  (material:texture_specular! *cube* (texture:load "assets/textures/container-specular.png"))
  (material:shininess! *cube* 64.0)

  (set! *light* (cube:create))
  (set! *light-shader* (shader:create "shaders/light-shader.vs" "shaders/light-shader.fs"))
  (material:color! *light* (vec3:create 1.0 0.0 0.0))

  (set! *sprite* (sprite:create))
  (set! *sprite-shader*	(shader:create "shaders/simple.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *sprite* (texture:load "assets/textures/awesomeface.png"))

  (set! *cube-positions*
	(list (vec3:create 0 0 0)
	      (vec3:create 2.25 0 0)
	      (vec3:create -1.25 0 0))))

(define (update)
  (fpcam:update)

  (when (key:up? KEY_G)
    (display "GC triggered.\n")
    (display (gc #t))
    (newline)))

(define (render)
  (window:clear)
  (shader:use *cube-shader*)
  
  (for-each
   (lambda (position)
     (let ((%tint% (vec3:create% 1 0 1)))
       (renderer:draw *cube* position 1 1 %tint%)
       (free% %tint%)))
   *cube-positions*)

  (shader:use *tile-shader*)

  (let ((current-col 0)
	(current-row 0))
    (for-each
     (lambda (tile-value)
       (let ((%pos% (vec3:create% current-col (sub1 tile-value) current-row)))
	 (renderer:draw *tile* %pos% 1 1 #f)
	 (free% %pos%))

       (set! current-col (add1 current-col))
       (when (>= current-col *TILE_MAP_COLS*)
	 (set! current-col 0)
	 (set! current-row (add1 current-row))
	 )
       )
     *tile-map*))

  (shader:use *sprite-shader*)

  ;; TODO; Make sure to use float values
  (let ((%pos% (vec3:create% 0 1.25 0))
	(%tint% (vec3:create% 1 1 1)))
    (renderer:draw *sprite* %pos% 1 1 %tint%)
    (free% %pos%)
    (free% %tint%))

  (shader:use *light-shader*)

  ;; (let ((%pos% (vec3:create% 2.0 2.0 2.0)))
  ;;   (renderer:draw *light* %pos% 1 1 #f)
  ;;   (free% %pos%))

  (for-each
   (lambda (point-light)
     (renderer:draw *light* (light:point:position point-light) 1.0 1.0 #f)
     )
   *point-lights*)

  (shader:use (shader:default))

  (window:swap))
