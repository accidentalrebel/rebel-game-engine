(import bind)
(import (chicken gc))

(set-gc-report! #t)

;; Note; Some functions have two versions. One of them has an underscore (make_vec3_),
;; another does not (make_vec3).
;;
;; The one without underscores return pointer objects that are automatically tracked
;; by the garbage collector. The one with underscores are not and you'd have to manually
;; free them after using them using the (free) function below.
;;
;; For the most part you'll only have to use the gc-tracked one. The other one is for when
;; you need more control over your memory allocations.

(bind-options mutable-fields: #t) ;; Allows the generation of setter procedures on structured definitions
(bind* "#include \"../src/stub.h\"") ;; stub.h is where c declarations used by the defines below are found

;; Used by functions that need a finalizer
;; Can also be called manually for freeing non-gc objects
(define free (foreign-lambda void "free" c-pointer)) 

(define rebel_init (foreign-lambda void "RebelInit" unsigned-integer unsigned-integer c-string))
(define rebel_destroy (foreign-lambda void "RebelDestroy"))
(define process_inputs (foreign-lambda void "ProcessInputs"))
(define make_vec3_ (foreign-lambda c-pointer "MakeVec3" float float float))
(define (make_vec3 x y z) (set-finalizer! (make_vec3_ x y z) free))
(define (get_vec3_x p) (Vec3-x p))
(define (get_vec3_y p) (Vec3-y p))
(define (set_vec3_x p x) (set! (Vec3-x p) x))
(define (set_vec3_y p y) (set! (Vec3-y p) y))
(define (get_vec3_z p) (Vec3-z p))
(define (set_vec3_z p z) (set! (Vec3-z p) z))

(define window_can_close (foreign-lambda bool "WindowCanClose"))
(define window_clear (foreign-lambda void "WindowClear"))
(define window_swap (foreign-lambda void "WindowSwap"))
(define window_destroy (foreign-lambda void "WindowDestroy"))

(define cube_create_ (foreign-lambda c-pointer "CubeCreate" c-string c-string))
(define (cube_create x y) (set-finalizer! (cube_create_ x y) free))

(define sprite_create_ (foreign-lambda c-pointer "SpriteCreate" c-string c-string))
(define (sprite_create x y) (set-finalizer! (sprite_create_ x y) free))
(define sprite_draw (foreign-lambda void "SpriteDraw"
				    (c-pointer (struct "Sprite"))
				    (c-pointer (struct "Vec3"))
				    float float
				    (c-pointer (struct "Vec3"))
				    (c-pointer (struct "Shader"))))

(define shader_create_ (foreign-lambda c-pointer "ShaderCreate" c-string c-string))
(define (shader_create x y) (set-finalizer! (shader_create_ x y) free))

(define is_key_down (foreign-lambda bool "IsKeyDown" (enum "Keys")))
(define is_key_up (foreign-lambda bool "IsKeyUp" (enum "Keys")))


