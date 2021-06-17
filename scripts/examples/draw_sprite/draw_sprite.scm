(include-relative "../../extensions/debug")

(define *sprite*)
(define *texture*)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *texture* (texture:load "assets/textures/awesomeface.png"
				"texture_diffuse"))
  (set! *sprite* (sprite:create 100
				100))

  (sprite:load_texture *sprite*
		       *texture*))

(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(1 1 1))
  (shader:use (shader:default))

  (renderer:draw *sprite*
		 position:'(0 0 0)
		 rotation:'(0 0 45))
  (renderer:draw *sprite*
		 position:'(100 0 0)
		 rotation:'(0 0 45))
  (renderer:draw *sprite*
		 position:'(200 0 0)
		 rotation:'(0 0 45))

  ;; (renderer:draw *sprite*
  ;; 		 position:'(100 100 0)
  ;; 		 rotation:'(0 0 180)
  ;; 		 scale: '(0.5 0.5 0.5))
  
  ;; (renderer:draw *sprite*
  ;; 		 position:'(100 0 0)
  ;; 		 rotation:'(0 0 180)
  ;; 		 color:'(1 1 1 0.5))
  
  ;; (renderer:draw *sprite*
  ;; 		 rotation:'(0 0 90)
  ;; 		 color:'(1 1 1 0.75))
  
  ;; (renderer:draw *sprite*
  ;; 		 position:'(-100 0 0))

  ;; (renderer:draw *sprite*
  ;; 		 position:'(100 -100 0)
  ;; 		 rotation:'(0 0 180)
  ;; 		 color:'(0 0 1 1))
  
  ;; (renderer:draw *sprite*
  ;; 		 position:'(0 -100 0)
  ;; 		 rotation:'(0 0 90)
  ;; 		 color:'(0 1 0 1))
  
  ;; (renderer:draw *sprite*
  ;; 		 position:'(-100 -100 0)
  ;; 		 color:'(1 0 0 1))
  
  (window:swap))

(define (destroy)
  (texture:unload *texture*))
