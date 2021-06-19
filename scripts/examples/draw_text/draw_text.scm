(include-relative "../../extensions/debug")

(define *rotation-index* 0)
(define *font*)
(define *text*)
(define *mono-font*)
(define *mono-text*)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *font* (font:load "assets/fonts/font"
			  "texture_diffuse"))
  (set! *text* (text:create "abcdefghijklmnopqrstuvwxyz"))

  (text:load_font *text*
		  *font*)

  (set! *mono-font* (font:load "assets/fonts/mono-font"
			  "texture_diffuse"))
  (set! *mono-text* (text:create "abcdefghijklmnopqrstuvwxyz"))

  (text:load_font *mono-text*
		  *mono-font*)
  #t)
  
(define (update)
  (debug:update)
  (set! *rotation-index* (if (> *rotation-index* 720)
		    0
		    (+ *rotation-index* 0.5)))
  #t)

(define (render)
  (window:clear '(0 0 0))
  (shader:use (shader:default))

  (renderer:draw_text *text*
		      position:'(0 260 0)
		      scale:'(1 1 1))
  (renderer:draw_text *mono-text*
		      position:'(0 200 0)
		      scale:'(0.75 0.75 0.75)
		      color:'(1 0 0 1))
  (renderer:draw_text *text*
		      position:'(0 140 0)
		      color:'(0 1 0 1))
  (renderer:draw_text *mono-text*
		      position:'(0 70 0)
		      color:'(0 0 1 1))
  (renderer:draw_text *text*
		      position:'(-100 -70 0)
		      color:'(1 1 1 0.5)
		      rotation:(list 0 0 *rotation-index*))
  (renderer:draw_text *mono-text*
		      position:'(100 -70 0)
		      color:'(1 1 1 0.5)
		      rotation:(list 0 0 (* *rotation-index* -1)))
   
  (window:swap))

(define (destroy)
  (text:destroy *text*)
  (text:destroy *mono-text*)
  (font:unload *font*)
  (font:unload *mono-font*)
  #t)

