(include-relative "../../extensions/debug")

(define *font*)
(define *text*)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *font* (font:load "assets/fonts/font.png"
			  "texture_diffuse"))
  (set! *text* (text:create "AopBQD"))

  (text:load_font *text*
		  *font*)
  #t)
  
(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(0 0 0))
  (shader:use (shader:default))

  (renderer:draw_text *text*)
  
  (window:swap))

(define (destroy)
  #t)
