(include-relative "../../extensions/debug")

(define *ANIM_DURATION* 0.1)

(define *sprite*)
(define *texture*)
(define *countdown* *ANIM_DURATION*)
(define *current-frame-index* 0)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *texture* (texture:load "assets/textures/animated-sprite.png"
				"texture_diffuse"))
  (set! *sprite* (sprite:create 128 128))
  (sprite:load_texture *sprite*
		       *texture*))

(define (update)
  (debug:update)
  (set! *countdown*
	(- *countdown* (time:elapsed)))
  (when (< *countdown* 0)
    (set! *current-frame-index* (+ *current-frame-index* 1))
    (when (>= *current-frame-index* 6)
      (set! *current-frame-index* 0))
    (set! *countdown* *ANIM_DURATION*)))

(define (render)
  (window:clear '(1 1 1))
  (shader:use (shader:default))

  (renderer:draw *sprite*
		 draw_rect:(list (* 128
				    *current-frame-index*)
				 0 128 128))
  
  (window:swap))

(define (destroy)
  (texture:unload *texture*))
