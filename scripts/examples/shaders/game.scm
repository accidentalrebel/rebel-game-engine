(include-relative "../../extensions/fpcam")

(define *box*)
(define *box-shader*)

(define (init)
  (light:point:create
   '(-1.0 1.0 1.0)
   '(0.1 0.1 0.1)
   '(1.0 0.0 0.0)
   '(0.0 0.0 0.0)
   1.0 0.14 0.07)
  
  ;; Generate a cube mesh and load it as a model
  ;; TODO; Fix the mesh normals
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
  (window:clear '(1.0 1.0 1.0))

  (shader:use *box-shader*)

  ;; TODO; Change to use renderer:draw
  (renderer:draw2 *box* '(0.0 0.0 0.0) 1.0 1.0)

  (window:swap)
  )
