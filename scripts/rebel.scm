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

;; REBEL
;; =====
(define rebel:init (foreign-lambda void "RebelInit" unsigned-integer unsigned-integer c-string))
(define rebel:destroy (foreign-lambda void "RebelDestroy"))
(define time:current (foreign-lambda double "GetCurrentTime"))
(define (time:elapsed) elapsed-time)
(define input:process (foreign-lambda void "InputProcess"))

;; VECTORS
;; =======
(define vec3:create% (foreign-lambda c-pointer "Vec3Create" float float float))
(define (vec3:create x y z) (set-finalizer! (vec3:create% x y z) free%))
(define vec3:copy% (foreign-lambda c-pointer "Vec3Copy" (c-pointer (struct "Vec3"))))
(define (vec3:copy x) (set-finalizer! (vec3:copy% x) free_vector%))

;; This functios checks if a vector is not a boolean before copying it.
;; This is useful because we can pass #f if we don't want to pass a vector value
;; For example, "(light:directional:create #f #f)" if you don't want to specify a direction or color
(define (vec3:check_copy% x) (if (not (boolean? x)) (vec3:copy% x) x))

(define (vec3:x vec) (Vec3-x vec))
(define (vec3:x! vec x) (set! (Vec3-x vec) x))

(define (vec3:y vec) (Vec3-y vec))
(define (vec3:y! vec y) (set! (Vec3-y vec) y))

(define (vec3:z vec) (Vec3-z vec))
(define (vec3:z! vec z) (set! (Vec3-z vec) z))

;; CAMERA
;; ======
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

;; LIGHT
;; =====
(define (light:ambient! light vec)
  (set! (Light-ambient light) vec))

;; DIRECTIONAL_LIGHT
;; =================
(define light:directional:create_ (foreign-lambda c-pointer "DirectionLightCreate"
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))))
(define (light:directional:create direction ambient diffuse specular)
  (light:directional:create_
   (vec3:check_copy% direction)
   (vec3:check_copy% ambient)
   (vec3:check_copy% diffuse)
   (vec3:check_copy% specular)))

(define light:point:create_ (foreign-lambda c-pointer "PointLightCreate"
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 float float float))
(define (light:point:create direction ambient diffuse specular constant linear quadratic)
  (light:point:create_
   (vec3:check_copy% direction)
   (vec3:check_copy% ambient)
   (vec3:check_copy% diffuse)
   (vec3:check_copy% specular)
   constant linear quadratic))
(define (light:directional:light light) (DirectionLight-light light))
(define (light:directional:ambient! light vec)
  (light:ambient! (light:directional:light light) (vec3:check_copy% vec)))

;; WINDOW
;; ======
(define window:can_close (foreign-lambda unsigned-integer "WindowCanClose"))
(define (window:close?)
  (if (= (window:can_close) 1)
      #t #f))
(define window:clear (foreign-lambda void "WindowClear"))
(define window:swap (foreign-lambda void "WindowSwap"))
(define window:destroy (foreign-lambda void "WindowDestroy"))

;; RENDERER
;; ========
(define cube:create% (foreign-lambda c-pointer "CubeCreate"))
(define (cube:create) (set-finalizer! (cube:create%) free%))

(define sprite:create% (foreign-lambda c-pointer "SpriteCreate"))
(define (sprite:create) (set-finalizer! (sprite:create%) free%))

(define renderer:draw (foreign-lambda void "RendererDraw"
				    (c-pointer (struct "Renderer"))
				    (c-pointer (struct "Vec3"))
				    float float
				    (c-pointer (struct "Vec3"))))
;; MATERIAL
;; ========
(define (material:texture_diffuse! renderer texture)
  (set! (Material-textureDiffuse1
	 (Renderer-material renderer))
	texture))
(define (material:texture_specular! renderer texture)
  (set! (Material-textureSpecular1
	 (Renderer-material renderer))
	texture))
(define (material:color renderer) (Material-color (Renderer-material renderer)))
(define (material:color! renderer color)
  (set! (Material-color
	 (Renderer-material renderer))
	(vec3:check_copy% color)))

(define (material:shininess renderer) (Material-shininess (Renderer-material renderer)))
(define (material:shininess! renderer shine)
  (set! (Material-shininess
	 (Renderer-material renderer))
	shine))

;; SHADER
;; ======
(define shader:use (foreign-lambda void "ShaderUse" (c-pointer (struct "Shader"))))
(define shader:default (foreign-lambda c-pointer "ShaderDefault"))
(define shader:create% (foreign-lambda c-pointer "ShaderCreate" c-string c-string))
(define (shader:create vertexPath fragmentPath) (set-finalizer! (shader:create% vertexPath fragmentPath) free%))

;; TEXTURE
;; =======
(define texture:load (foreign-lambda unsigned-integer "TextureLoad" c-string))
(define texture:unload (foreign-lambda void "TextureUnload" unsigned-integer))

;; KEYBOARD
;; ========
(define key:is_down (foreign-lambda unsigned-integer "KeyIsDown" (enum "Keys")))
(define key:is_up (foreign-lambda unsigned-integer "KeyIsUp" (enum "Keys")))
(define (key:down? key)
  (if (= (key:is_down key) 1)
      #t #f))
(define (key:up? key)
  (if (= (key:is_up key) 1)
      #t #f))

;; MOUSE
;; =====
(define mouse:enable (foreign-lambda void "MouseEnable"))
(define mouse:instance (foreign-lambda c-pointer "MouseGetInstance"))
(define (mouse:x) (Mouse-xPos (mouse:instance)))
(define (mouse:y) (Mouse-yPos (mouse:instance)))
(define (mouse:initialized?)
  (if (= (Mouse-initialized (mouse:instance)) 1)
    #t #f))
