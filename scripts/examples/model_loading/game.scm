(define *basic-shader*)
(define *backpack*)

;; Setup the first person camera extension
(include-relative "../../extensions/fpcam")

(define (init)
  ;; Load the model object
  ;; model:load also loads the textures automatically
  (set! *backpack*
	(model:load "assets/models/backpack/backpack.obj"))

  ;; Load a basic shader
  (set! *basic-shader*
	(shader:create "shaders/model-loading.vs" "shaders/model-loading.fs")))

(define (update)
  ;; This is needed for the first person camera extension to work
  (fpcam:update))

(define (render)
  (window:clear '(0.1 0.1 0.1))

  ;; Use the basic-shader
  (shader:use *basic-shader*)

  ;; Draw the backpack model
  (model:draw *backpack* '(0 0 0) '(1 1 1))
  
  (window:swap))
