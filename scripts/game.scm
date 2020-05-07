(import (chicken string))

(define box-sprite)
(define box-pos)

(define (init)
  (set! box-sprite (sprite_create "assets/textures" "tile.png"))
  (set! box-pos (make_vec3 400 300 0))
  #t)

(define (update)
  (window_clear)
  
  (when (is_key_down KEY_COMMA)
    (set_vec3_y box-pos (+ (get_vec3_y box-pos) 1)))
  (when (is_key_down KEY_O)
    (set_vec3_y box-pos (- (get_vec3_y box-pos) 1)))
  (when (is_key_down KEY_E)
    (set_vec3_x box-pos (+ (get_vec3_x box-pos) 1)))
  (when (is_key_down KEY_A)
    (set_vec3_x box-pos (- (get_vec3_x box-pos) 1)))

  (when (is_key_down KEY_PERIOD)
    (set_vec3_z box-pos 1))
  (when (is_key_down KEY_SEMICOLON)
    (set_vec3_z box-pos -1))

  (let ((tint (make_vec3_ 1 0 1)))
    (sprite_draw box-sprite box-pos 50 50 tint #f)
    (sprite_draw box-sprite
		 (make_vec3 400 300 0)
		 100 100
		 (make_vec3 1 0 0) #f)
    (free tint))
  
  (window_swap)
  ;; (display (conc "GC: " (->string (gc #f))))
  #t)
