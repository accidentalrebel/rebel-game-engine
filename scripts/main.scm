(include-relative "rebel")
(include-relative "game")

(rebel_init 800 600 "Rebel Engine")

(define (loop-program)
  (if (window_can_close)
      (exit-program)
      (begin
	(process_inputs)
	(update)
	(loop-program))
      ))

(define (exit-program)
  (rebel_destroy)
  (print "Program exited."))

(init)
(loop-program)
