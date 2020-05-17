(include-relative "rebel")

(rebel:init 800 600 "Rebel Engine")

(include-relative "game")

(define (loop-program)
  (if (= (window:can_close) 1)
      (exit-program)
      (begin
	(input:process)
	(update)
	(camera:update_vectors (camera:main))
	(loop-program))
      ))

(define (exit-program)
  (rebel:destroy)
  (print "Program exited."))

(init)
(loop-program)
