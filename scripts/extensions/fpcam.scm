(mouse:enable)

(define MOUSE_SENSITIVITY 0.2)

(define last-mouse-x)
(define last-mouse-y)
(define first-mouse 1)

(define (fpcam::handle_mouse)
  (when (= first-mouse 1)
    (set! last-mouse-x (mouse:x))
    (set! last-mouse-y (mouse:y))
    (set! first-mouse 0))

  (let* ((main-camera (camera:main))
	(x-offset (* (- (mouse:x) last-mouse-x) MOUSE_SENSITIVITY))
	(y-offset (* (- last-mouse-y (mouse:y)) MOUSE_SENSITIVITY))
	(computed-pitch (+ (camera:pitch main-camera) y-offset)))

    (when (and (not (= x-offset 0.0))
	       (not (= y-offset 0.0)))
      (camera:yaw! main-camera (+ (camera:yaw main-camera) x-offset))
      (camera:pitch! main-camera
		     (cond ((> computed-pitch 89.0)
			    89.0)
			   ((< computed-pitch -89.0)
			    -89.0)
			   (else
			    computed-pitch)))
      
      (camera:update_vectors main-camera)))
  
  (set! last-mouse-x (mouse:x))
  (set! last-mouse-y (mouse:y))
  )

(define (fpcam:update)
  (when (mouse:initialized?)
    (fpcam::handle_mouse))
  
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
	   (camera:pitch! main-camera (- (camera:pitch main-camera) 0.01))))))
