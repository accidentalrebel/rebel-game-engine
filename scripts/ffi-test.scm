; TODO; Have a Metafile https://github.com/yashrk/raylib-scm/blob/c0aad6866057a479cc58d1b9c2932d2c6d571494/raylib-scm.meta

(import (chicken foreign))
(import foreigners)

(foreign-declare "#include \"../src/ffi-test.h\"")

(define-foreign-record-type (vector-3 Vector3))

(define vector-3
  (foreign-lambda*
   void
   ()
   "FFITest();"))

(vector-3)
