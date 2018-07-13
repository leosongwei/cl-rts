(ql:quickload 'sdl2)
(ql:quickload :cffi-libffi)

(load "utils.lisp")

(progn ;; video parameters
  (defparameter *w* 640)
  (defparameter *h* 480)
  (defparameter *sdl2-window* nil)
  (defparameter *sdl2-renderer* nil)
  (defparameter *sdl2-surface* nil)
  (defparameter *sdl2-pixel-buffer* nil))

(load "cffi.lisp")
;;(load "text-render.lisp")
(load "ui.lisp")

(defparameter *class-slots* (make-hash-table))

(defun parse-defclass-f (name slots)
  (let ((slots-list '()))
    (dolist (slot slots)
      (let* ((slot-name (car slot))
             (slot-attrib (cdr slot))
             (slot-alloc (getf slot-attrib :allocation)))
        (when (or (null slot-alloc)
                  (eq slot-alloc :instance))
          (push slot-name slots-list))))
    (setf (gethash name *class-slots*) (reverse slots-list))
    (reverse slots-list)))

(defmacro class-def (name superclass-list slots &rest options)
  (parse-defclass-f name slots)
  `(defclass ,name ,superclass-list ,slots ,@options))
;;(gethash 'astro-obj *class-slots*)

(defun class-precedence-list (class)
  (sb-mop:class-precedence-list class))

(defun export-obj (obj) ;; (slow)
  (let* ((class (class-of obj))
         (precedence (class-precedence-list class))
         (result nil)
         (avail-slots nil))
    (dolist (class precedence)
      (let* ((class-name (class-name class))
             (slots (gethash class-name *class-slots*))) ;; a symbol
        (dolist (slot slots)
          (when (not (getf result slot))
            (setf (getf result slot) t)
            (push slot avail-slots)))))
    (dolist (slot avail-slots)
      (setf (getf result slot) (slot-value obj slot)))
    result))

(load "objdef.lisp")

