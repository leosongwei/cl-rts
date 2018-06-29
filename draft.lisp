(make-instance 'astro-obj)
(astro-obj-orbit-angle (make-instance 'astro-obj))
(describe (make-instance 'astro-obj))

(defclass test-class-0 ()
  ((a :initarg :a
      :initform 1.0
      :accessor t-a)
   (b :initarg :b
      :initform 2.0
      :accessor t-b)))

(defclass test-class-1 (test-class-0)
  ((a :initarg :a
      :initform 3.0
      :accessor t1-a)
   (c :initarg :c
      :initform 4.0
      :accessor t1-c)))

(let ((o (make-instance 'test-class-1)))
  (print (sb-mop:class-precedence-list (find-class 'test-class-1)))
  (list (slot-value o 'a)
        (t-a o)
        (t1-a o)))

(time
 (dotimes (i 10000)
   (export-obj (make-instance 'planet))))

'(FACILITIES NIL
  POPULATION NIL
  POS-Y 0.0
  POS-X 0.0
  ORBIT-RADIUS 1.0
  ORBIT-ANGLE 0
  ORBITING NIL
  RADIUS 1.0
  MASS 1.0
  TYPE :ROCKY
  RESOURCES NIL
  ECOSYS 0.0
  ENV 0.0)



(init-window)
(clear)
(update)

(print *sdl2-surface*)
(print #'cffi:null-pointer)

(defparameter *earth* (img-load "rsc/earth.png"))

(let ((img-surface (img-load "rsc/earth.png")))
  (sdl2:blit-surface img-surface nil *sdl2-surface* nil)
  (update)
  (sdl2:free-surface img-surface))

(defun show-earth-at (x y)
  (sdl2:blit-surface *earth* nil
                     *sdl2-surface*
                     (sdl2:make-rect x y 128 128)))

(progn
  (clear)
  (show-earth-at 10 10)
  (show-earth-at 80 80)
  (update))

(close-window)
