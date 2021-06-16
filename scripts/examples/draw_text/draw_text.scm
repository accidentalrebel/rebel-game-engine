(include-relative "../../extensions/debug")

(define *font*)
(define *text*)
(define *mono-font*)
(define *mono-text*)

(define (init)
  (camera:projection! (camera:main) camera-projection/ORTHOGRAPHIC)
  
  (set! *font* (font:load "assets/fonts/font"
			  "texture_diffuse"))
  (set! *text* (text:create "Aa.Bb Pp*Qq Gg>Hh"))

  (text:load_font *text*
		  *font*)

  (set! *mono-font* (font:load "assets/fonts/mono-font"
			  "texture_diffuse"))
  (set! *mono-text* (text:create "Aa.Bb Pp:Qq Gg>Hh"))

  (text:load_font *mono-text*
		  *mono-font*)
  #t)
  
(define (update)
  (debug:update)
  #t)

(define (render)
  (window:clear '(0 0 0))
  (shader:use (shader:default))

  (renderer:draw_text *text*
		      position:'(-400 32 0)
		      scale:'(1 1 1))
  (renderer:draw_text *mono-text*
		      position:'(-400 -32 0)
		      scale:'(1 1 1))
  
  (window:swap))

(define (destroy)
  (text:destroy *text*)
  (text:destroy *mono-text*)
  #t)
