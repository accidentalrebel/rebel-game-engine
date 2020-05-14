(import (chicken string))

(define smile-sprite)

(define cube-shader)
(define cube)

(define current-pos)
(define cube-positions)

(define MOVEMENT_SPEED 0.001)

(define (init)
  (set! smile-sprite (sprite_create "assets/textures" "awesomeface.png"))
  
  (set! cube (cube_create "assets/textures" "awesomeface.png"))
  (set! cube-shader (shader_create "shaders/simple-3d.vs" "shaders/simple.fs"))

  (set!
   cube-positions (list (make_vec3 0 0 0)
			(make_vec3 1.25 0 0)
			(make_vec3 -1.25 0 0)))
  
  (set! current-pos (make_vec3 0 0 0))
  #t)

(define (update)
  (window_clear)
  
  (when (is_key_down KEY_COMMA)
    (set_vec3_y current-pos (+ (get_vec3_y current-pos) MOVEMENT_SPEED)))
  (when (is_key_down KEY_O)
    (set_vec3_y current-pos (- (get_vec3_y current-pos) MOVEMENT_SPEED)))
  (when (is_key_down KEY_E)
    (set_vec3_x current-pos (+ (get_vec3_x current-pos) MOVEMENT_SPEED)))
  (when (is_key_down KEY_A)
    (set_vec3_x current-pos (- (get_vec3_x current-pos) MOVEMENT_SPEED)))

  (when (is_key_up KEY_PERIOD)
    (set_vec3_z current-pos 1))
  (when (is_key_up KEY_SEMICOLON)
    (set_vec3_z current-pos -1))
  
  (when (is_key_up KEY_C)
    (let* ((main-camera (camera_get_main))
	   (current-projection (camera_get_projection main-camera)))
      (if (= current-projection PERSPECTIVE)
	  (camera_set_projection main-camera ORTHOGRAPHIC)
	  (camera_set_projection main-camera PERSPECTIVE))))
  
  (for-each
   (lambda (position)
     (let ((tint (make_vec3_ 1 0 1)))
       (cube_draw cube
		  position
		  1 1 ;30 30
		  tint
		  cube-shader)
       (free tint)))
   cube-positions)

  (let ((tint (make_vec3_ 0 0 1)))
    (sprite_draw smile-sprite
		 current-pos
		 1 1
		 tint #f)
    (free tint))
  
  (window_swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
