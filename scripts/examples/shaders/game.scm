(define *box*)

(define (init)
  ;; Generate a cube mesh and load it as a model
  (set! *box* (model:load_from_mesh (mesh:generate_cube 1.0 1.0 1.0)))

  ;; Load the texture and assign is as a texture diffuse
  (model:texture_diffuse! *box* (texture:load "assets/texture/texel-checker.png"))
  )

(define (update)
  ;; (renderer:draw2 *box* '(0.0 0.0 0.0) 1.0 1.0)
  )

(define (render)
  (window:clear)

  (window:swap)
  )
