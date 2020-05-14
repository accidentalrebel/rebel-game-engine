(import (chicken string))

(define MOVEMENT_SPEED 0.001)

(define *cube*)
(define *cube-shader*)
(define *cube-positions*)

(define (init)
  (set! *cube*
	(cube_create "assets/textures" "awesomeface.png"))
  (set! *cube-shader*
	(shader_create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! *cube-positions*
	(list (make_vec3 0 0 0)
	      (make_vec3 1.25 0 0)
	      (make_vec3 -1.25 0 0)))
  #t)

(define (update)
  (window_clear)
  
  (let* ((main-camera (camera_main))
	 (current-projection (camera_projection main-camera))
	 (camera-pos (camera_position main-camera)))
    
    (when (key_up? KEY_C)
      (if (= current-projection PERSPECTIVE)
	  (camera_projection! main-camera ORTHOGRAPHIC)
	  (camera_projection! main-camera PERSPECTIVE)))
    
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
       (cube_draw *cube* position 1 1 tint *cube-shader*)
       (free% tint)))
   *cube-positions*)

  (window_swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
