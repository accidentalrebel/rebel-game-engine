(include-relative "../../extensions/debug")

(define *index* 0)
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
  (set! *index* (if (> *index* 720)
		    0
		    (+ *index* 0.5)))
  #t)

(define (render)
  (window:clear '(0 0 0))
  (shader:use (shader:default))

  (renderer:draw_text *text*
		      position:'(0 100 0)
		      scale:'(1 1 1)
 		      rotation:(list 0 0 *index*))
  (renderer:draw_text *mono-text*
		      position:'(0 -300 0)
		      scale:'(1 1 1)
 		      rotation:(list 0 0 *index*))
  
  (window:swap))

(define (destroy)
  (text:destroy *text*)
  (text:destroy *mono-text*)
  (font:unload *font*)
  (font:unload *mono-font*)
  #t)
