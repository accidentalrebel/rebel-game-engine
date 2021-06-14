(include-relative "../../extensions/debug")

(define *font*)
(define *text*)

(define (init)
  (set! *font* (font:load "assets/fonts/font.png"
			  "texture_diffuse"))
  (set! *text* (text:create "test"))
  #t)
  
(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(1 1 1))

  (renderer:draw_text *text*)
  
  (window:swap))

(define (destroy)
  #t)
