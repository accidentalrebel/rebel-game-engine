(import (srfi-1))
(import (chicken gc))
(import (chicken string))
(import (chicken foreign))
(import foreigners)

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

(foreign-declare "#include \"external/cglm/cglm.h\"")
(foreign-declare "#include \"rebel.h\"")
(foreign-declare "#include \"data/vec3.h\"")
(foreign-declare "#include \"core/window.h\"")
(foreign-declare "#include \"graphics/shader.h\"")
(foreign-declare "#include \"graphics/camera.h\"")
(foreign-declare "#include \"graphics/renderer.h\"")
(foreign-declare "#include \"graphics/material.h\"")
(foreign-declare "#include \"graphics/lighting/light.h\"")
(foreign-declare "#include \"input/keyboard.h\"")
(foreign-declare "#include \"input/mouse.h\"")

;; MACROS
;; ======
(define-syntax make_vec3_setter
  (syntax-rules ()
    ((make_vec3_setter function-name struct-name variable-name body ...)
     (define function-name
      (foreign-lambda*
       void
       (((c-pointer (struct struct-name)) a0)
	(float a1)
	(float a2)
	(float a3))
       "glm_vec3_copy((vec3){ a1, a2, a3 }, a0->"variable-name");")))))

(define-syntax make_vec3_getter
  (syntax-rules ()
    ((make_vec3_setter function-name struct-name variable-name body ...)
     (define function-name
       (foreign-lambda*
	(c-pointer (struct "Vec3"))
	(((c-pointer (struct struct-name)) a0))
	"Vec3* v = Vec3Create(a0->"variable-name"[0], a0->"variable-name"[1], a0->"variable-name"[2]);
C_return(v);")))))

;; UTILS
;; =====
;; Used by functions that need a finalizer
;; Can also be called manually for freeing non-gc objects
(define free% (foreign-lambda void "free" c-pointer))
(define (free_vec3_debug% v)
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
(define rebel:draw (foreign-lambda void "RebelDraw"))
(define time:current (foreign-lambda double "GetCurrentTime"))
(define (time:elapsed) elapsed-time)
(define input:process (foreign-lambda void "InputProcess"))

;; VECTORS
;; =======
(define-foreign-record-type (vec3 Vec3)
  (float x Vec3-x Vec3-x!)
  (float y Vec3-y Vec3-y!)
  (float z Vec3-z Vec3-z!))

(define vec3:create% (foreign-lambda c-pointer "Vec3Create" float float float))

(define (vec3:x vec) (Vec3-x vec))
(define (vec3:x! vec x) (set! (Vec3-x vec) x))

(define (vec3:y vec) (Vec3-y vec))
(define (vec3:y! vec y) (set! (Vec3-y vec) y))

(define (vec3:z vec) (Vec3-z vec))
(define (vec3:z! vec z) (set! (Vec3-z vec) z))

;; VEC3 CONVERTERS
;; ===============
(define (list_to_vec3 l)
  (if (not (boolean? l))
      (vec3:create% (first l) (second l) (third l))
      l))

;; Converts vec3 to list and then frees the vector afterwards
(define (vec3_to_list v)
  (if (not (boolean? v))
      (let ((l (list (vec3:x v) (vec3:y v) (vec3:z v))))
	(free% v)
	l)
      v))

;; CAMERA
;; ======
(define-foreign-enum-type (direction unsigned-integer)
  (direction->unsigned-integer unsigned-integer->direction)
  ((forward direction/FORWARD) FORWARD)
  ((backward direction/BACKWARD) BACKWARD)
  ((right direction/RIGHT) RIGHT)
  ((left direction/LEFT) LEFT)
  ((up direction/UP) UP)
  ((down direction/DOWN) DOWN))

(define-foreign-enum-type (camera-projection unsigned-integer)
  (camera-projection->unsigned-integer unsigned-integer->camera-projection)
  ((perspective camera-projection/PERSPECTIVE) PERSPECTIVE)
  ((orthographic camera-projection/ORTHOGRAPHIC) ORTHOGRAPHIC))

(define-foreign-record-type (camera Camera)
  (unsigned-integer projection Camera-projection Camera-projection!)
  (float pitch Camera-pitch Camera-pitch!)
  (float yaw Camera-yaw Camera-yaw!))

(make_vec3_getter Camera-position "Camera" "position")

(define camera:main (foreign-lambda c-pointer "CameraGetMain"))
(define camera:update_vectors (foreign-lambda void "CameraUpdateVectors" (c-pointer (struct "Camera"))))
(define camera:move (foreign-lambda void "CameraMove" (c-pointer (struct "Camera")) (enum "Direction") float))
(define (camera:projection! camera value) (Camera-projection! camera value))
(define (camera:projection camera) (Camera-projection camera))

(define (camera:position camera)
  (vec3_to_list (Camera-position camera)))

;(define (camera:position! camera value) (set! (Camera-position camera) value))
(define (camera:front camera) (Camera-front camera))
(define (camera:front! camera value) (set! (Camera-front camera) value))
(define (camera:yaw camera) (Camera-yaw camera))
(define (camera:yaw! camera value) (Camera-yaw! camera value))
(define (camera:pitch camera) (Camera-pitch camera))
(define (camera:pitch! camera value) (Camera-pitch! camera value))

;; LIGHT
;; =====
;; (define-foreign-record-type (light Light)
;;   ((c-pointer (struct "vec3")) diffuse Light-diffuse Light-diffuse!))

(make_vec3_setter Light-diffuse! "Light" "diffuse")
(make_vec3_getter Light-diffuse "Light" "diffuse")

(define (light:ambient! light vec)
  (set! (Light-ambient light) (list_to_vec3 vec)))
(define (light:diffuse light)
  (vec3_to_list (Light-diffuse light)))

;; DIRECTIONAL_LIGHT
;; =================
(define-foreign-record-type (point-light PointLight)
  ((c-pointer (struct "Light")) light PointLight-light PointLight-light!))

(make_vec3_setter PointLight-position! "PointLight" "position")
(make_vec3_getter PointLight-position "PointLight" "position")

(define light:directional:create_ (foreign-lambda c-pointer "DirectionLightCreate"
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))))
(define (light:directional:create direction ambient diffuse specular)
  (light:directional:create_
   (list_to_vec3 direction)
   (list_to_vec3 ambient)
   (list_to_vec3 diffuse)
   (list_to_vec3 specular)))

(define light:point:create_ (foreign-lambda c-pointer "PointLightCreate"
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 (c-pointer (struct "Vec3"))
						 float float float))
(define (light:point:create direction ambient diffuse specular constant linear quadratic)
  (light:point:create_
   (list_to_vec3 direction)
   (list_to_vec3 ambient)
   (list_to_vec3 diffuse)
   (list_to_vec3 specular)
   constant linear quadratic))

(define (light:point:light point-light) (PointLight-light point-light))
(define (light:point:position point-light)
  (vec3_to_list (PointLight-position point-light)))
(define (light:point:position! point-light pos)
  (PointLight-position! point-light (first pos) (second pos) (third pos)))
(define (light:point:diffuse point-light)
  (light:diffuse (light:point:light point-light)))
(define (light:directional:light light) (DirectionLight-light light))
(define (light:directional:ambient! light vec)
  (light:ambient! (light:directional:light light) vec))

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

(define renderer:draw_
  (foreign-lambda*
   void
   (((c-pointer (struct "Renderer")) a0)
    (float a1)
    (float a2)
    (float a3)
    (float a4)
    (float a5))
   "RendererDraw(a0, (vec3){ a1, a2, a3 }, a4, a5);"))
(define (renderer:draw a b c d)
  (renderer:draw_ a (first b) (second b) (third b) c d))

(define-foreign-record-type (renderer Renderer)
  ((c-pointer (struct "Material")) material Renderer-material Renderer-material!))

;; MATERIAL
;; ========
(define-foreign-record-type (material Material)
  (unsigned-integer textureDiffuse1 Material-textureDiffuse1 Material-textureDiffuse1!)
  (unsigned-integer textureSpecular1 Material-textureSpecular1 Material-textureSpecular1!)
  (float shininess Material-shininess Material-shininess!))

(make_vec3_setter Material-color! "Material" "color")

(define (material:texture_diffuse! renderer texture)
  (Material-textureDiffuse1! (Renderer-material renderer) texture))

(define (material:texture_specular! renderer texture)
  (Material-textureSpecular1! (Renderer-material renderer) texture))

(define (material:color renderer) (Material-color (Renderer-material renderer)))

;; TODO; Make a macro for "(first color) (second color) (third color)"
(define (material:color! renderer color)
  (Material-color! (Renderer-material renderer) (first color) (second color) (third color)))

(define (material:shininess renderer) (Material-shininess (Renderer-material renderer)))

(define (material:shininess! renderer shine)
  (Material-shininess! (Renderer-material renderer) shine))

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
(define-foreign-enum-type (keys unsigned-integer)
  (keys->unsigned-integer unsigned-integer->keys)
  ((a keys/A) KEY_A)
  ((c keys/C) KEY_C)
  ((e keys/E) KEY_E)
  ((g keys/G) KEY_G)
  ((o keys/O) KEY_O)
  ((comma keys/COMMA) KEY_COMMA))

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
(define-foreign-record-type (mouse Mouse)
  (unsigned-integer initialized Mouse-initialized Mouse-initialized!)
  (double xPos Mouse-xPos Mouse-xPos!)
  (double yPos Mouse-yPos Mouse-yPos!))

(define mouse:enable (foreign-lambda void "MouseEnable"))
(define mouse:instance (foreign-lambda c-pointer "MouseGetInstance"))
(define (mouse:x) (Mouse-xPos (mouse:instance)))
(define (mouse:y) (Mouse-yPos (mouse:instance)))
(define (mouse:initialized?)
  (if (= (Mouse-initialized (mouse:instance)) 1)
    #t #f))
