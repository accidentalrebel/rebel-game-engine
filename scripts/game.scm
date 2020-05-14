(import (chicken string))

(define MOVEMENT_SPEED 0.001)

(define cube)
(define cube-shader)
(define cube-positions)

(define (init)
  (set! cube
	(cube_create "assets/textures" "awesomeface.png"))
  (set! cube-shader
	(shader_create "shaders/simple-3d.vs" "shaders/simple.fs"))
  (set! cube-positions
	(list (make_vec3 0 0 0)
	      (make_vec3 1.25 0 0)
	      (make_vec3 -1.25 0 0)))
  #t)

(define (update)
  (window_clear)
  
  (let* ((main-camera (camera_get_main))
	 (current-projection (camera_get_projection main-camera))
	 (camera-pos (get_camera_position main-camera)))
    
    (when (is_key_up KEY_C)
      (if (= current-projection PERSPECTIVE)
	  (camera_set_projection main-camera ORTHOGRAPHIC)
	  (camera_set_projection main-camera PERSPECTIVE)))
    
    (when (is_key_down KEY_A)
      (set_vec3_x camera-pos
		  (+ (get_vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (is_key_down KEY_E)
      (set_vec3_x camera-pos
		  (- (get_vec3_x camera-pos) MOVEMENT_SPEED)))
    (when (is_key_down KEY_COMMA)
      (set_vec3_z camera-pos
		  (+ (get_vec3_z camera-pos) MOVEMENT_SPEED)))
    (when (is_key_down KEY_O)
      (set_vec3_z camera-pos
		  (- (get_vec3_z camera-pos) MOVEMENT_SPEED))))
  
  (for-each
   (lambda (position)
     (let ((tint (make_vec3_ 1 0 1)))
       (cube_draw cube position 1 1 tint cube-shader)
       (free tint)))
   cube-positions)

  (window_swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
