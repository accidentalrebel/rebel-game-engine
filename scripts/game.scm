(import (chicken string))

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
	(list (make_vec3 0 0 0)
	      (make_vec3 1.25 0 0)
	      (make_vec3 -1.25 0 0)))
  #t)

(define (update)
  (window:clear)
  
  (let* ((main-camera (camera_main))
	 (current-projection (camera:projection main-camera))
	 (camera-pos (camera:position main-camera)))
    
    (when (key_up? KEY_C)
      (if (= current-projection PERSPECTIVE)
	  (camera:projection! main-camera ORTHOGRAPHIC)
	  (camera:projection! main-camera PERSPECTIVE)))
    
    (when (key_down? KEY_A)
      (vec3_x! camera-pos
		  (+ (vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (key_down? KEY_E)
      (vec3_x! camera-pos
		  (- (vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (key_down? KEY_COMMA)
      (vec3_z! camera-pos
		  (+ (vec3_z camera-pos) MOVEMENT_SPEED)))
    (when (key_down? KEY_O)
      (vec3_z! camera-pos
		  (- (vec3_z camera-pos) MOVEMENT_SPEED))))
  
  (for-each
   (lambda (position)
     (let ((tint (make_vec3% 1 0 1)))
       (cube:draw *cube* position 1 1 tint *cube-shader*)
       (free% tint)))
   *cube-positions*)

  (window:swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
