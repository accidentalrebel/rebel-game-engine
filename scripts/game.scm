(include-relative "extensions/fpcam")

(define *cube*)
(define *cube-shader*)
(define *sprite*)
(define *sprite-shader*)
(define *cube-positions*)

(define (init)
  (let ((dir-light (light:directional:create
		    (vec3:create 0.0 -1.0 1.0)
		    (vec3:create 0.0 0.0 0.6)
		    (vec3:create 0.0 0.6 0.0)
		    (vec3:create 1.0 1.0 1.0))))
    (light:directional:ambient! dir-light (vec3:create 0.6 0.0 0.0)))
  
  (set! *cube*
	(cube:create "assets/textures" "tile.png"))
  (set! *cube-shader*
	(shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))

  (set! *sprite*
	(sprite:create "assets/textures" "awesomeface.png"))
  (set! *sprite-shader*
	(shader:create "shaders/simple.vs" "shaders/simple.fs"))

  (renderer:color! *sprite* (vec3:create 0.0 0.0 1.0))
  (material:ambient! *cube* (vec3:create 0.135 0.2225 0.1575))
  (material:diffuse! *cube* (vec3:create 0.54 0.89 0.63))
  (material:specular! *cube* (vec3:create 0.3162 0.3162 0.3162))
  (material:shininess! *cube* 32.0)

  (set! *cube-positions*
	(list (vec3:create 0 0 0)
	      (vec3:create 1.25 0 0)
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

  (shader:use *sprite-shader*)

  (let ((%pos% (vec3:create% 0 1.25 0))
	(%tint% (vec3:create% 1 1 1)))
    (renderer:draw *sprite* %pos% 1 1 %tint%)
    (free% %pos%)
    (free% %tint%))

  (shader:use (shader:default))

  (window:swap))
