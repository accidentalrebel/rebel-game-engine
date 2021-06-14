(include-relative "../../extensions/debug")

(define *font*)
(define *text*)

(define (init)
  (set! *font* (font:load "assets/fonts/font.png"
			  "texture_diffuse"))
  (set! *text* (text:create "test"))
  
  (text:load_font *text*
		  *font*)
  #t)
  
(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(1 1 1))
  (shader:use (shader:default))

  (renderer:draw (text:canvas *text*))
  
  (window:swap))

(define (destroy)
  #t)
