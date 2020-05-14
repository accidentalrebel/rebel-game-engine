(define MOVEMENT_SPEED 0.001)

(define *cube*)
(define *cube-shader*)
(define *cube-positions*)

(define (init)
  (set! *cube*
	(cube:create "assets/textures" "awesomeface.png"))
  (set! *cube-shader*
	(shader:create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! *cube-positions*
	(list (vec3:create 0 0 0)
	      (vec3:create 1.25 0 0)
	      (vec3:create -1.25 0 0)))
  #t)

(define (update)
  (window:clear)
  
  (let* ((main-camera (camera:main))
	 (current-projection (camera:projection main-camera))
	 (camera-pos (camera:position main-camera)))
    
    (when (key:up? KEY_C)
      (if (= current-projection PERSPECTIVE)
	  (camera:projection! main-camera ORTHOGRAPHIC)
	  (camera:projection! main-camera PERSPECTIVE)))
    
    (when (key:down? KEY_A)
      (vec3_x! camera-pos
		  (+ (vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (key:down? KEY_E)
      (vec3_x! camera-pos
		  (- (vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (key:down? KEY_COMMA)
      (vec3_z! camera-pos
		  (+ (vec3_z camera-pos) MOVEMENT_SPEED)))
    (when (key:down? KEY_O)
      (vec3_z! camera-pos
		  (- (vec3_z camera-pos) MOVEMENT_SPEED))))
  
  (for-each
   (lambda (position)
     (let ((tint (vec3:create% 1 0 1)))
       (cube:draw *cube* position 1 1 tint *cube-shader*)
       (free% tint)))
   *cube-positions*)

  (window:swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
