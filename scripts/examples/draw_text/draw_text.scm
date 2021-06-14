(include-relative "../../extensions/debug")

(define *font*)

(define (init)
  (set! *font* (font:load "assets/fonts/font.png"
			  "texture_diffuse"))
  #t)
  
(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(1 1 1))
  
  (window:swap))

(define (destroy)
  #t)
