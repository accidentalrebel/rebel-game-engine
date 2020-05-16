(define MOVEMENT_SPEED 0.001)

(include-relative "extensions/fpcam")

(define *cube*)
(define *cube-shader*)
(define *cube-positions*)

(define (init)
  (set! *cube*
	(cube:create "assets/textures" "awesomeface.png"))
  (set! *cube-shader*
	(shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! *cube-positions*
	(list (vec3:create 0 0 0)
	      (vec3:create 1.25 0 0)
	      (vec3:create -1.25 0 0))))

(define (update)
  (window:clear)

  (fpcam:update)
  
  (for-each
   (lambda (position)
     (let ((%tint% (vec3:create% 1 0 1)))
       (cube:draw *cube* position 1 1 %tint% *cube-shader*)
       (free% %tint%)))
   *cube-positions*)

  (window:swap))
