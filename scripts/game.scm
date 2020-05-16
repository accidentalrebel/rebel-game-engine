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
	      (vec3:create -1.25 0 0))))

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
      (camera:move main-camera LEFT MOVEMENT_SPEED))
    (when (key:down? KEY_E)
      (camera:move main-camera RIGHT MOVEMENT_SPEED))
    (when (key:down? KEY_COMMA)
      (camera:move main-camera FORWARD MOVEMENT_SPEED))
    (when (key:down? KEY_O)
      (camera:move main-camera BACKWARD MOVEMENT_SPEED))
    
    (cond ((key:down? KEY_SEMICOLON)
	   (camera:yaw! main-camera (- (camera:yaw main-camera) 0.01)))
	  ((key:down? KEY_PERIOD)
	   (camera:yaw! main-camera (+ (camera:yaw main-camera) 0.01))))
    (cond ((key:down? KEY_APOSTROPHE)
	   (camera:pitch! main-camera (+ (camera:pitch main-camera) 0.01)))
	  ((key:down? KEY_J)
	   (camera:pitch! main-camera (- (camera:pitch main-camera) 0.01))))
    )
  
  (for-each
   (lambda (position)
     (let ((%tint% (vec3:create% 1 0 1)))
       (cube:draw *cube* position 1 1 %tint% *cube-shader*)
       (free% %tint%)))
   *cube-positions*)

  (window:swap))
