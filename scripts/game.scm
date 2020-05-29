(include-relative "extensions/fpcam")

(define *cube*)
(define *cube-shader*)
(define *sprite*)
(define *sprite-shader*)
(define *cube-positions*)

(define (init)
  (light:directional:create
   (vec3:create 1.2 -1.0 -1.0)
   (vec3:create 0.05 0.05 0.05)
   (vec3:create 0.4 0.4 0.4)
   (vec3:create 0.5 0.5 0.5))

  (light:point:create
   (vec3:create 2.25 0.0 -1.0)
   (vec3:create 0.1 0.05 0.05)
   (vec3:create 0.8 0.2 0.2)
   (vec3:create 1.0 0.2 0.2)
   1.0 0.09 0.032)

  (light:point:create
   (vec3:create -1.25 0.0 -1.0)
   (vec3:create 0.05 0.05 0.1)
   (vec3:create 0.2 0.2 0.8)
   (vec3:create 0.2 0.2 1.0)
   1.0 0.09 0.032)

  (set! *cube* (cube:create))
  (set! *cube-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (material:texture_diffuse! *cube* (texture:load "assets/textures/container.png"))
  (material:texture_specular! *cube* (texture:load "assets/textures/container-specular.png"))
  (material:shininess! *cube* 64.0)

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

  (shader:use *sprite-shader*)

  (let ((%pos% (vec3:create% 0 1.25 0))
	(%tint% (vec3:create% 1 1 1)))
    (renderer:draw *sprite* %pos% 1 1 %tint%)
    (free% %pos%)
    (free% %tint%))

  (shader:use (shader:default))

  (window:swap))
