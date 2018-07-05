(defparameter *window-list* nil)

(defclass window ()
  ((size-w :initform 0 :type 'integer
           :accessor window-size-w)
   (size-h :initform 0 :type 'integer
           :accessor window-size-h)
   (focus-x :initform 0.0 :type 'single-float
            :accessor window-focus-x)
   (focus-y :initform 0.0 :type 'single-float
            :accessor window-focus-y)
   (zoom :initform 1.0 :type 'single-float
         :accessor window-zoom)
   (max-fps :initform 60 :type integer
            :accessor window-max-fps)
   (sdl2-window :initform nil
                :accessor window-sdl2-window)
   (sdl2-renderer :initform nil
                  :accessor window-sdl2-renderer)
   (frames :initform nil
           :accessor window-frames)
   (thread :initform nil
           :accessor window-thread)))

(defun window-resize-handler (window)
  (sdl2:get-window-surface (window-sdl2-window window))
  (sdl2:get-renderer-output-size (window-sdl2-renderer window)))

(defun close-window (window)
  (if (typep (window-thread window)
             'sb-thread:thread)
      (sb-thread:terminate-thread (window-thread window)))
  (sdl2:destroy-renderer (window-sdl2-renderer window))
  (sdl2:destroy-window (window-sdl2-window window))
  (setf (window-sdl2-renderer window) nil)
  (setf (window-sdl2-window window) nil))

(defun update-window (window)
  (sdl2:render-present (window-sdl2-renderer window)))

(defun make-window (&optional (title "CL-RTS") (w 640) (h 480))
  (let* ((window (make-instance 'window))
         (sdl2-window (sdl2:create-window :title title
                                          :w w
                                          :h h
                                          :flags '(:shown :resizable)))
         (sdl2-renderer (sdl2:create-renderer sdl2-window -1
                                              '(:accelerated))))
    (sdl2:get-window-surface sdl2-window)
    (sdl2:set-render-draw-blend-mode sdl2-renderer :blend)
    (setf (window-size-w window) w)
    (setf (window-size-h window) h)
    (setf (window-sdl2-window window) sdl2-window)
    (setf (window-sdl2-renderer window) sdl2-renderer)
    window))

