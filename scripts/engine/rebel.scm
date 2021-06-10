(import (srfi-1))
(import (chicken gc))
(import (chicken string))
(import (chicken foreign))
(import foreigners)

(set-gc-report! #t)

;; DECLARATIONS
;; ============
(foreign-declare "
typedef struct Vec3
{
  float x;
  float y;
  float z;
} Vec3;
Vec3* Vec3Create(float x, float y, float z)
{
  Vec3* v = (Vec3*)malloc(sizeof(Vec3));
  v->x = x;
  v->y = y;
  v->z = z;
  return v;
}
");

(foreign-declare "#include \"external/cglm/cglm.h\"")
(foreign-declare "#include \"rebel.h\"")
(foreign-declare "#include \"core/window.h\"")
(foreign-declare "#include \"graphics/shader.h\"")
(foreign-declare "#include \"graphics/sprite.h\"")
(foreign-declare "#include \"graphics/camera.h\"")
(foreign-declare "#include \"graphics/renderer.h\"")
(foreign-declare "#include \"graphics/model.h\"")
(foreign-declare "#include \"graphics/mesh.h\"")
(foreign-declare "#include \"graphics/material.h\"")
(foreign-declare "#include \"graphics/lighting/light.h\"")
(foreign-declare "#include \"input/keyboard.h\"")
(foreign-declare "#include \"input/mouse.h\"")

;; MACROS
;; ======

;; This is an explanation for make_vec3_getter and make_vec3_setter:
;;
;; Say we have a C struct below:
;;; typedef struct Duck {
;;;   vec3 direction;
;;; }
;;
;; We then define a lisp function to get the value of direction as shown below:
;;; (make_vec3_getter Duck-direction "Duck" "direction")
;;; (define (duck:direction duck)
;;;   (vec3_to_list (Duck-direction duck)))
(define-syntax make_vec3_getter
  (syntax-rules ()
    ((make_vec3_setter function-name struct-name variable-name body ...)
     (define function-name
       (foreign-lambda*
	(c-pointer (struct "Vec3"))
	(((c-pointer (struct struct-name)) a0))
	"Vec3* v = Vec3Create(a0->"variable-name"[0], a0->"variable-name"[1], a0->"variable-name"[2]);
C_return(v);")))))

;; To set the direction, we do the following:
;;; (make_vec3_getter Duck-direction "Duck" "direction")
;;; (define (duck:direction! duck value)
;;;   (set! (Duck-direction duck) (list_to_vec3 value)))
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
  (float x vec3:x vec3:x!)
  (float y vec3:y vec3:y!)
  (float z vec3:z vec3:z!))

(define vec3:create% (foreign-lambda c-pointer "Vec3Create" float float float))

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
  (unsigned-integer projection camera:projection camera:projection!)
  (float pitch camera:pitch camera:pitch!)
  (float yaw camera:yaw camera:yaw!)
  (unsigned-int size camera:size camera:size!))

(define camera:main (foreign-lambda c-pointer "CameraGetMain"))
(define camera:update_vectors (foreign-lambda void "CameraUpdateVectors" (c-pointer (struct "Camera"))))
(define camera:move (foreign-lambda void "CameraMove" (c-pointer (struct "Camera")) (enum "Direction") float))

(make_vec3_getter Camera-position "Camera" "position")
(define (camera:position camera)
  (vec3_to_list (Camera-position camera)))

(define (camera:position! camera value)
  (set! (Camera-position camera)
	(list_to_vec3 value)))

(define (camera:front camera)
  (vec3_to_list (Camera-front camera)))

(define (camera:front! camera value)
  (set! (Camera-front camera)
	(list_to_vec3 value)))

;; LIGHT
;; =====
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

(define light:directional:create_
  (foreign-lambda*
   (c-pointer (struct "DirectionLight"))
   ((float a0) (float a1) (float a2)
    (float a3) (float a4) (float a5)
    (float a6) (float a7) (float a8)
    (float a9) (float a10) (float a11))
   "C_return(DirectionLightCreate((vec3){ a0, a1, a2}, (vec3){ a3, a4, a5}, (vec3){ a6, a7, a8}, (vec3){a9, a10, a11}));"))
(define (light:directional:create direction ambient diffuse specular)
  (light:directional:create_
   (first direction) (second direction) (third direction)
   (first ambient) (second ambient) (third ambient)
   (first diffuse) (second diffuse) (third diffuse)
   (first specular) (second specular) (third specular)))

(define light:point:create_
  (foreign-lambda*
   (c-pointer (struct "PointLight"))
   ((float a0) (float a1) (float a2)
    (float a3) (float a4) (float a5)
    (float a6) (float a7) (float a8)
    (float a9) (float a10) (float a11)
    (float a12)
    (float a13)
    (float a14))
   "C_return(PointLightCreate((vec3){ a0, a1, a2}, (vec3){ a3, a4, a5}, (vec3){ a6, a7, a8}, (vec3){a9, a10, a11}, a12, a13, a14));"))
(define (light:point:create position ambient diffuse specular constant linear quadratic)
  (light:point:create_
   (first position) (second position) (third position)
   (first ambient) (second ambient) (third ambient)
   (first diffuse) (second diffuse) (third diffuse)
   (first specular) (second specular) (third specular)
   constant linear quadratic))

(define light:point:create2_
  (foreign-lambda*
   (c-pointer (struct "PointLight"))
   ((float a0) (float a1) (float a2)
    (float a3) (float a4) (float a5))
   "C_return(PointLightCreate2((vec3){ a0, a1, a2}, (vec3){ a3, a4, a5}));"))
(define (light:point:create2 position color)
  (light:point:create2_
   (first position) (second position) (third position)
   (first color) (second color) (third color)))

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

(define window:clear_
  (foreign-lambda*
   void
   ((float a0) (float a1) (float a2))
   "WindowClear((vec3){a0, a1, a2});"))
(define (window:clear color)
  (window:clear_ (first color) (second color) (third color)))
  
(define window:swap (foreign-lambda void "WindowSwap"))
(define window:destroy (foreign-lambda void "WindowDestroy"))

;; RENDERER
;; ========
(define renderer:draw_
  (foreign-lambda*
   void
   (((c-pointer (struct "Model")) a0)
    (float a1) (float a2) (float a3)
    (float a4) (float a5) (float a6)
    (float a7) (float a8) (float a9)
    (float a10) (float a11) (float a12) (float a13))
   "RendererDraw(a0, (vec3){ a1, a2, a3 }, (vec3){ a4, a5, a6 }, (vec3){ a7, a8, a9}, (vec4){ a10, a11, a12, a13});"))

(define (renderer:draw model #!key (position '(0 0 0)) (scale '(1 1 1)) (rotation '(0 0 0)) (color '(1 1 1 1)))
  (renderer:draw_ model
		  (first position) (second position) (third position)
		  (first scale) (second scale) (third scale)
		  (first rotation) (second rotation) (third rotation)
		  (first color) (second color) (third color) (fourth color)))

;; SPRITE
;; ======
(define sprite:create (foreign-lambda (c-pointer "Model") "SpriteCreate" float float))
(define sprite:load_texture (foreign-lambda void "SpriteLoadTexture" (c-pointer (struct "Model")) (c-pointer (struct "Texture"))))

;; MESH
;; ====
(define mesh:generate_cube (foreign-lambda (c-pointer (struct "Mesh")) "MeshGenerateCube" float float float))
(define mesh:generate_plane (foreign-lambda (c-pointer (struct "Mesh")) "MeshGeneratePlane" float float))

;; MODEL
;; =====
(define-foreign-record-type (model Model)
  ((c-pointer (struct "Material")) material Model-material Model-material!))

(define model:load_from_mesh (foreign-lambda (c-pointer (struct "Model")) "ModelLoadFromMesh" (c-pointer (struct "Mesh"))))
(define (model:texture_diffuse! model texture)
  (Material-textureDiffuse1! (Model-material model) texture))

(define model:load (foreign-lambda (c-pointer (struct "Model")) "ModelLoad" c-string))
(define model:load_texture (foreign-lambda void "ModelLoadTexture" (c-pointer (struct "Model")) (c-pointer (struct "Texture"))))

;; MATERIAL
;; ========
(define-foreign-record-type (material Material)
  (unsigned-integer textureDiffuse1 Material-textureDiffuse1 Material-textureDiffuse1!)
  (unsigned-integer textureSpecular1 Material-textureSpecular1 Material-textureSpecular1!)
  (float shininess Material-shininess Material-shininess!))

(make_vec3_setter Material-color! "Material" "color")

;; TEXTURE
;; =======
(define-foreign-record-type (texture Texture)
  (float width texture:width)
  (float height texture:height))

;; (define (texture:width texture) (Texture-width texture))
;; (make_vec3_getter Texture-width "Texture" "width")
;; (make_vec3_getter Texture-height "Texture" "height")

;; (define (texture:width texture)
;;   (Texture-width texture))
;; (define (texture:height texture)
;;   (Texture-height texture))

;; TODO; Change the third parameter into an enum
(define texture:load_ (foreign-lambda (c-pointer (struct "Texture")) "TextureLoad" c-string c-string c-string))
(define (texture:load path type-name)
  (let ((splitted-path (string-split path "/")))
    (texture:load_ (string-append (car (flatten (string-intersperse (take splitted-path
									  (- (length splitted-path)
									     1))
								    "/")))
				  "/")
		   (last splitted-path)
		   type-name)))

(define texture:unload (foreign-lambda void "TextureUnload" (c-pointer (struct "Texture"))))

;; SHADER
;; ======
(define shader:use (foreign-lambda void "ShaderUse" (c-pointer (struct "Shader"))))
(define shader:default (foreign-lambda c-pointer "ShaderDefault"))
(define shader:create% (foreign-lambda c-pointer "ShaderCreate" c-string c-string))
(define (shader:create vertexPath fragmentPath) (set-finalizer! (shader:create% vertexPath fragmentPath) free%))

;; KEYBOARD
;; ========
(define-foreign-enum-type (keys unsigned-integer)
  (keys->unsigned-integer unsigned-integer->keys)
  ((a keys/A) KEY_A)
  ((c keys/C) KEY_C)
  ((e keys/E) KEY_E)
  ((g keys/G) KEY_G)
  ((m keys/M) KEY_M)
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
  (unsigned-integer cursorEnabled Mouse-cursor_enabled Mouse-cursor_enabled!)
  (double xPos Mouse-xPos Mouse-xPos!)
  (double yPos Mouse-yPos Mouse-yPos!))

(define mouse:enable (foreign-lambda void "MouseEnable"))
(define mouse:instance (foreign-lambda c-pointer "MouseGetInstance"))
(define mouse:cursor_enable (foreign-lambda void "CursorEnable"))
(define mouse:cursor_disable (foreign-lambda void "CursorDisable"))
(define (mouse:cursor_enabled?)
  (if (= (Mouse-cursor_enabled (mouse:instance)) 1)
      #t #f))

(define (mouse:x) (Mouse-xPos (mouse:instance)))
(define (mouse:y) (Mouse-yPos (mouse:instance)))
(define (mouse:initialized?)
  (if (= (Mouse-initialized (mouse:instance)) 1)
    #t #f))
