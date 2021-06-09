(include-relative "../../engine/rebel")

(rebel:init 800 600 "Rebel Engine")

(include-relative "draw_sprite")

(define previous-time (time:current))
(define elapsed-time 0)

(define (loop-program)
  (if (window:close?)
      (exit-program)
      (begin

	(let* ((current-time (time:current)))
	  
	  (set! elapsed-time (- current-time previous-time))
	  
	  (input:process)
	  (update)
	  (camera:update_vectors (camera:main))

	  (render)
	  (set! previous-time current-time))
	(loop-program))
      ))

(define (exit-program)
  (rebel:destroy)
  (print "Program exited."))

(init)
(loop-program)
