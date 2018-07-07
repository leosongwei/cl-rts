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


(init-sdl2)
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

;; (sdl2:get-renderer *sdl2-window*)

;; (sdl2:set-render-draw-blend-mode *sdl2-renderer* :blend)
;; (progn
;;   (clear)
;;   (show-earth-at -20 -20)
;;   (show-earth-at 50 50)
;;   (sdl2:set-render-draw-color *sdl2-renderer* 150 0 0 128)
;;   (sdl2:render-fill-rect *sdl2-renderer*
;;                          (sdl2:make-rect 30 30 128 128))
;;   (sdl2:blit-surface *text-surface* nil *sdl2-surface* nil)
;;   (update))

(progn
  (clear-window *win1*)
  (sdl2:blit-surface *text-surface* nil
                     (window-sdl2-surface *win1*) nil)
  (update-window *win1*))

(close-window *win1*)

(progn
  (sdl2-ttf-init)
  (defparameter *font-default*
    (open-ttf "/usr/share/fonts/TTF/DejaVuSansMono.ttf" 16))
  (set-font-style *font-default* '(:normal))
  (defparameter *text-surface*
    (render-text-as-surface "The quick brown fox jumps over the lazy dog, 你好世界"
                            *font-default*
                            0 0 255))
  (values (sdl2:surface-width *text-surface*)
          (sdl2:surface-height *text-surface*)))


  (defparameter *text-texture*
    (sdl2:create-texture-from-surface
     (window-sdl2-renderer *win1*)
     *text-surface*))

(progn
  (clear-window *win1*)
  (sdl2:render-copy (window-sdl2-renderer *win1*) *text-texture*
                    :source-rect nil
                    :dest-rect (sdl2:make-rect 0 0 490 19))
  (update-window *win1*))


(init-sdl2)
(defparameter *win1* (make-window))
(window-resize-handler *win1*)
(clear-window *win1*)
(update-window *win1*)
(close-window *win1*)
