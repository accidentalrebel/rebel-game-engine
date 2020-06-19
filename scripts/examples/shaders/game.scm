(include-relative "../../extensions/fpcam")

(define *box*)
(define *box-shader*)

(define (init)
  (light:point:create
   '(-2.0 -0.25 2.0)
   '(1.0 1.0 1.0)
   '(0.8 0.8 0.8)
   '(1.0 1.0 1.0)
   1.0 0.7 1.8)
  
  (light:point:create
   '(2.0 -0.25 2.0)
   '(1.0 0.05 0.05)
   '(0.8 0.2 0.2)
   '(1.0 0.2 0.2)
   1.0 0.7 1.8) 
  
  ;; Generate a cube mesh and load it as a model
  (set! *box* (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))

  ;; Load the texture and assign is as a texture diffuse
  (model:texture_diffuse! *box* (texture:load "assets/textures/texel-checker.png"))

  ;; TODO; Make own shader for this example only
  (set! *box-shader* (shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  )

(define (update)
  (fpcam:update)
  #t
  )

(define (render)
  (window:clear)

  (shader:use *box-shader*)

  ;; TODO; Change to use renderer:draw
  (renderer:draw2 *box* '(0.0 0.0 0.0) 1.0 1.0)

  (window:swap)
  )
