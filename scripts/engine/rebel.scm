(import (srfi-1))
(import (chicken gc))
(import (chicken string))
(import (chicken foreign))
(import foreigners)

(set-gc-report! #t)

;; DECLARATIONS
;; ============
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
(foreign-declare "#include \"graphics/text.h\"")
(foreign-declare "#include \"graphics/lighting/light.h\"")
(foreign-declare "#include \"input/keyboard.h\"")
(foreign-declare "#include \"input/mouse.h\"")

;; UTILS
;; =====
;; Used by functions that need a finalizer
;; Can also be called manually for freeing non-gc objects
(define free% (foreign-lambda void "free" c-pointer))

;; REBEL
;; =====
(define rebel:init (foreign-lambda void "RebelInit" unsigned-integer unsigned-integer c-string))
(define rebel:destroy (foreign-lambda void "RebelDestroy"))
(define rebel:draw (foreign-lambda void "RebelDraw"))
(define time:current (foreign-lambda double "GetCurrentTime"))
(define (time:elapsed) elapsed-time)
(define input:process (foreign-lambda void "InputProcess"))

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

(define (camera:position! camera value)
  (set! (Camera-position camera)
	(list_to_vec3 value)))

(define (camera:front camera)
  (vec3_to_list (Camera-front camera)))

(define (camera:front! camera value)
  (set! (Camera-front camera)
	(list_to_vec3 value)))


;; DIRECTIONAL_LIGHT
;; =================
(define-foreign-record-type (point-light PointLight)
  ((c-pointer (struct "Light")) light PointLight-light PointLight-light!))

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
(define renderer:wireframe? (foreign-lambda bool "RendererIsWireFrameEnabled"))
(define renderer:wireframe! (foreign-lambda void "RendererSetWireFrameMode" bool))

(define renderer:draw_
  (foreign-lambda*
   void
   (((c-pointer (struct "Model")) a0)
    (float a01) (float a02) (float a03) (float a04)
    (float a1) (float a2) (float a3)
    (float a4) (float a5) (float a6)
    (float a7) (float a8) (float a9)
    (float a10) (float a11) (float a12) (float a13))
   "RendererDraw(a0, (vec4){a01, a02, a03, a04}, (vec3){ a1, a2, a3 }, (vec3){ a4, a5, a6 }, (vec3){ a7, a8, a9}, (vec4){ a10, a11, a12, a13});"))

(define (renderer:draw model #!key (draw_rect '(0 0 0 0)) (position '(0 0 0)) (scale '(1 1 1)) (rotation '(0 0 0)) (color '(1 1 1 1)))
  (renderer:draw_ model
		  (first draw_rect) (second draw_rect) (third draw_rect) (fourth draw_rect)
		  (first position) (second position) (third position)
		  (first scale) (second scale) (third scale)
		  (first rotation) (second rotation) (third rotation)
		  (first color) (second color) (third color) (fourth color)))

(define renderer:draw_text_
  (foreign-lambda*
   void
   (((c-pointer (struct "Text")) a0)
    (float a1) (float a2) (float a3)
    (float a4) (float a5) (float a6)
    (float a7) (float a8) (float a9)
    (float a10) (float a11) (float a12) (float a13))
   "RendererDrawText(a0, (vec3){ a1, a2, a3 }, (vec3){ a4, a5, a6 }, (vec3){ a7, a8, a9}, (vec4){ a10, a11, a12, a13});"))

(define (renderer:draw_text text #!key (position '(0 0 0)) (scale '(1 1 1)) (rotation '(0 0 0)) (color '(1 1 1 1)))
  (renderer:draw_text_ text
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

;; TEXTURE
;; =======
(define-foreign-record-type (texture Texture)
  (float width texture:width)
  (float height texture:height))

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

;; TEXT
;; ====
(define font:load_ (foreign-lambda (c-pointer (struct "Font")) "FontLoad" c-string c-string c-string))
(define (font:load path type-name)
  (let ((splitted-path (string-split path "/")))
    (font:load_ (string-append (car (flatten (string-intersperse (take splitted-path
									  (- (length splitted-path)
									     1))
								    "/")))
			       "/")
		(last splitted-path)
		type-name)))

(define text:create (foreign-lambda (c-pointer (struct "Text")) "TextCreate" c-string))
(define text:load_font (foreign-lambda void "TextLoadFont" (c-pointer (struct "Text")) (c-pointer (struct "Font"))))
(define-foreign-record-type (text Text)
  ((c-pointer (struct "Model")) canvas text:canvas text:canvas!))

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
  ((w keys/W) KEY_W)
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
