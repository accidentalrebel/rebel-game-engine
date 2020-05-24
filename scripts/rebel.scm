(import bind)
(import (chicken gc))
(import (chicken string))

(set-gc-report! #t)

;; Note; Some functions have two versions. One of them has an underscore (vec3:create%),
;; another does not (vec3:create).
;;
;; The one without underscores return pointer objects that are automatically tracked
;; by the garbage collector. The one with underscores are not and you'd have to manually
;; free them after using them using the (free) function below.
;;
;; For the most part you'll only have to use the gc-tracked one. The other one is for when
;; you need more control over your memory allocations.

(bind-options mutable-fields: #t) ;; Allows the generation of setter procedures on structured definitions
;; Note; The bind egg (extension) has a feature where it automatically generates getter and setter functions to Structs. Take a look at the getter and setter fuctions of Vec3 (vec3:x and vec3:x!).

(bind* "#include \"../src/stub.h\"") ;; stub.h is where c declarations used by the defines below are found

;; Used by functions that need a finalizer
;; Can also be called manually for freeing non-gc objects
(define free% (foreign-lambda void "free" c-pointer))
(define (free_vector_debug% v)
  (display "FREED: ")
  (display (vec3:x v))
  (display " ")
  (display (vec3:y v))
  (display " ")
  (display (vec3:z v))
  (newline)
  (free% v))

(define rebel:init (foreign-lambda void "RebelInit" unsigned-integer unsigned-integer c-string))
(define rebel:destroy (foreign-lambda void "RebelDestroy"))
(define time:current (foreign-lambda double "GetCurrentTime"))
(define (time:elapsed) elapsed-time)
(define input:process (foreign-lambda void "InputProcess"))

(define vec3:create% (foreign-lambda c-pointer "Vec3Create" float float float))
(define (vec3:create x y z) (set-finalizer! (vec3:create% x y z) free%))
(define vec3:copy% (foreign-lambda c-pointer "Vec3Copy" (c-pointer (struct "Vec3"))))
(define (vec3:copy x) (set-finalizer! (vec3:copy% x) free_vector%))

(define (vec3:x p) (Vec3-x p))
(define (vec3:x! p x) (set! (Vec3-x p) x))

(define (vec3:y p) (Vec3-y p))
(define (vec3:y! p y) (set! (Vec3-y p) y))

(define (vec3:z p) (Vec3-z p))
(define (vec3:z! p z) (set! (Vec3-z p) z))

(define camera:main (foreign-lambda c-pointer "CameraGetMain"))
(define camera:update_vectors (foreign-lambda void "CameraUpdateVectors" (c-pointer (struct "Camera"))))
(define camera:move (foreign-lambda void "CameraMove" (c-pointer (struct "Camera")) (enum "Direction") float))
(define (camera:projection! camera value) (set! (Camera-projection camera) value))
(define (camera:projection camera) (Camera-projection camera))
(define (camera:position camera) (Camera-position camera))
(define (camera:position! camera value) (set! (Camera-position camera) value))
(define (camera:front camera) (Camera-front camera))
(define (camera:front! camera value) (set! (Camera-front camera) value))
(define (camera:yaw camera) (Camera-yaw camera))
(define (camera:yaw! camera value) (set! (Camera-yaw camera) value))
(define (camera:pitch camera) (Camera-pitch camera))
(define (camera:pitch! camera value) (set! (Camera-pitch camera) value))

(define light:directional:create_ (foreign-lambda c-pointer "DirectionLightCreate"
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))))
(define (light:directional:create x y) (light:directional:create_ (vec3:copy% x) (vec3:copy% y)))

(define window:can_close (foreign-lambda unsigned-integer "WindowCanClose"))
(define (window:close?)
  (if (= (window:can_close) 1)
      #t #f))
(define window:clear (foreign-lambda void "WindowClear"))
(define window:swap (foreign-lambda void "WindowSwap"))
(define window:destroy (foreign-lambda void "WindowDestroy"))

(define cube:create% (foreign-lambda c-pointer "CubeCreate" c-string c-string))
(define (cube:create x y) (set-finalizer! (cube:create% x y) free%))

(define sprite:create% (foreign-lambda c-pointer "SpriteCreate" c-string c-string))
(define (sprite:create x y) (set-finalizer! (sprite:create% x y) free%))

(define renderer:draw (foreign-lambda void "RendererDraw"
				    (c-pointer (struct "Renderer"))
				    (c-pointer (struct "Vec3"))
				    float float
				    (c-pointer (struct "Vec3"))))

;; Note: Sets the color of the renderer. Overwrites the material.
;; If you more control over the material just use the material: functions below
(define renderer:color!_ (foreign-lambda void "RendererSetColor"
					(c-pointer (struct "Renderer"))
					(c-pointer (struct "Vec3"))))
(define (renderer:color! x y) (renderer:color!_ x (vec3:copy% y)))
(define (material:ambient renderer) (Material-ambient (Renderer-material renderer)))
(define (material:ambient! renderer a) (set! (Material-ambient (Renderer-material renderer)) (vec3:copy% a)))
(define (material:diffuse renderer) (Material-diffuse (Renderer-material renderer)))
(define (material:diffuse! renderer a) (set! (Material-diffuse (Renderer-material renderer)) (vec3:copy% a)))
(define (material:specular renderer) (Material-specular (Renderer-material renderer)))
(define (material:specular! renderer a) (set! (Material-specular (Renderer-material renderer)) (vec3:copy% a)))
(define (material:shininess renderer) (Material-shininess (Renderer-material renderer)))
(define (material:shininess! renderer a) (set! (Material-shininess (Renderer-material renderer)) a))

(define shader:use (foreign-lambda void "ShaderUse" (c-pointer (struct "Shader"))))
(define shader:default (foreign-lambda c-pointer "ShaderDefault"))
(define shader:create% (foreign-lambda c-pointer "ShaderCreate" c-string c-string))
(define (shader:create x y) (set-finalizer! (shader:create% x y) free%))

(define key:is_down (foreign-lambda unsigned-integer "KeyIsDown" (enum "Keys")))
(define key:is_up (foreign-lambda unsigned-integer "KeyIsUp" (enum "Keys")))
(define (key:down? key)
  (if (= (key:is_down key) 1)
      #t #f))
(define (key:up? key)
  (if (= (key:is_up key) 1)
      #t #f))

(define mouse:enable (foreign-lambda void "MouseEnable"))
(define mouse:instance (foreign-lambda c-pointer "MouseGetInstance"))
(define (mouse:x) (Mouse-xPos (mouse:instance)))
(define (mouse:y) (Mouse-yPos (mouse:instance)))
(define (mouse:initialized?)
  (if (= (Mouse-initialized (mouse:instance)) 1)
    #t #f))
