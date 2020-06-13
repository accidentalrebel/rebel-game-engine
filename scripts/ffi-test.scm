; TODO; Have a Metafile https://github.com/yashrk/raylib-scm/blob/c0aad6866057a479cc58d1b9c2932d2c6d571494/raylib-scm.meta

(import (chicken foreign))
(import foreigners)

(foreign-declare "#include \"../src/ffi-test.h\"")

(define vector3
  (foreign-lambda*
   (c-pointer (struct "Vector3"))
   ((float a0)
    (float a1)
    (float a2))
   "C_return(MakeVector3(a0, a1, a2));"))
(define string_accept
  (foreign-lambda*
   void
   ((c-string a0))
   "StringAccept(a0);"))
(define vector3_accept
  (foreign-lambda*
   void
   (((c-pointer (struct "Vector3")) a0))
   "Vector3Accept(*a0);"))

;; Makes accessors
(define-foreign-record-type (vector3 Vector3)
  (float x vector3_x vector3_x!)
  (float y vector3_y vector3_y!)
  (float z vector3_z vector3_z!))

(display (vector3_x (vector3 8.0 9.0 10.0)))

(string_accept "Test")
(vector3_accept (vector3 1.0 2.0 3.0))


