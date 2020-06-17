(mouse:enable)

(define MOVEMENT_SPEED 5)
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

    (camera:yaw! main-camera (+ (camera:yaw main-camera) x-offset))
    (camera:pitch! main-camera
		   (cond ((> computed-pitch 89.0)
			  89.0)
			 ((< computed-pitch -89.0)
			  -89.0)
			 (else
			  computed-pitch)))
    
    (camera:update_vectors main-camera))
  
  (set! last-mouse-x (mouse:x))
  (set! last-mouse-y (mouse:y)))

(define (fpcam:update)
  (when (mouse:initialized?)
    (fpcam::handle_mouse))
  
  (let* ((main-camera (camera:main))
	 (current-projection (camera:projection main-camera))
	 (move-speed (* MOVEMENT_SPEED (time:elapsed))))
    
    (when (key:up? keys/C)
      (if (= current-projection camera-projection/PERSPECTIVE)
	  (camera:projection! main-camera camera-projection/ORTHOGRAPHIC)
	  (camera:projection! main-camera camera-projection/PERSPECTIVE)))
    
    (when (key:down? keys/A)
      (camera:move main-camera LEFT move-speed))
    (when (key:down? keys/E)
      (camera:move main-camera RIGHT move-speed))
    (when (key:down? keys/COMMA)
      (camera:move main-camera FORWARD move-speed))
    (when (key:down? keys/O)
      (camera:move main-camera BACKWARD move-speed))))
