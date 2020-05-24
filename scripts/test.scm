(define vector-test)

(define (init)
  (set! *cube*
	(cube:create "assets/textures" "tile.png"))

  (vec3:create 1 2 3)
  (material:ambient! *cube* (vec3:create 4 5 6))

  (camera:position! (camera:main) (vec3:create 7 8 9))

  (let ((local-test (vec3:create 10 11 12)))
    #t
    )
  
  (set! vector-test (vec3:create 13 14 15))
  (vec3:copy (vec3:create 16 17 18))
  (vec3:copy vector-test)
  )

(define (update)
  (when (key:up? KEY_G)
    (display "GC triggered.\n")
    (display (gc #t))
    (newline))

  (when (key:up? KEY_C)
    (display "GC triggered.\n")
    (display (gc #f))
    (newline)))

(define (render)
  #t)
