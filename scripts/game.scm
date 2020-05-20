(define MOVEMENT_SPEED 0.001)

(include-relative "extensions/fpcam")

(define *cube*)
(define *cube-shader*)
(define *sprite*)
(define *sprite-shader*)
(define *cube-positions*)

(define (init)
  (light:directional:create (vec3:create 0 -1 1) (vec3:create 1 1 1))
  
  (set! *cube*
	(cube:create "assets/textures" "tile.png"))
  (set! *cube-shader*
	(shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))

  (set! *sprite*
	(sprite:create "assets/textures" "awesomeface.png"))
  (set! *sprite-shader*
	(shader:create "shaders/simple.vs" "shaders/simple.fs"))
  
  (set! *cube-positions*
	(list (vec3:create 0 0 0)
	      (vec3:create 1.25 0 0)
	      (vec3:create -1.25 0 0))))

(define (update)
  (window:clear)

  (fpcam:update)

  (when (key:up? KEY_G)
    (display "GC triggered.\n")
    (display (gc #t))
    (newline))
  
  (for-each
   (lambda (position)
     (let ((%tint% (vec3:create% 1 0 1)))
       (cube:draw *cube* position 1 1 %tint% *cube-shader*)
       (free% %tint%)))
   *cube-positions*)

  (let ((%pos% (vec3:create% 0 1.25 0))
	(%tint% (vec3:create% 1 1 1)))
    (sprite:draw *sprite* %pos% 1 1 %tint% *sprite-shader*)
    (free% %pos%)
    (free% %tint%))

  (window:swap))
