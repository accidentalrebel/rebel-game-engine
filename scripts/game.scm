(import (chicken string))

(define smile-sprite)

(define cube-shader)
(define cube)

(define current-pos)

(define MOVEMENT_SPEED 0.05)

(define (init)
  (set! smile-sprite (sprite_create "assets/textures" "awesomeface.png"))
  
  (set! cube (cube_create "assets/textures" "awesomeface.png"))
  (set! cube-shader (shader_create "shaders/simple-3d.vs" "shaders/simple.fs"))
   
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

  (when (is_key_down KEY_PERIOD)
    (set_vec3_z current-pos 1))
  (when (is_key_down KEY_SEMICOLON)
    (set_vec3_z current-pos -1))

  (let ((tint (make_vec3_ 0 0 1)))
    (sprite_draw smile-sprite
		 current-pos
		 50 50
		 tint #f)
    (cube_draw cube
	       (make_vec3 0 0 0)
	       50 50
	       (make_vec3 1 0 0) cube-shader)
    (free tint))
  
  (window_swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
