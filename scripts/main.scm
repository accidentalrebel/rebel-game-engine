(include-relative "rebel")
(include-relative "game")

(rebel:init 800 600 "Rebel Engine")

(define (loop-program)
  (if (window:can_close)
      (exit-program)
      (begin
	(process_inputs)
	(update)
	(loop-program))
      ))

(define (exit-program)
  (rebel:destroy)
  (print "Program exited."))

(init)
(loop-program)
