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
   (sdl2-window-id :initform -1
                   :accessor window-sdl2-window-id)
   (sdl2-renderer :initform nil
                  :accessor window-sdl2-renderer)
   (sdl2-surface :initform nil
                 :accessor window-sdl2-surface)
   (frames :initform nil
           :accessor window-frames)
   (thread :initform nil
           :accessor window-thread)))

(defun window-resize-handler (window w h)
  (sdl2:set-window-size (window-sdl2-window window) w h)
  (setf (window-sdl2-surface window)
        (sdl2:get-window-surface (window-sdl2-window window)))
  (setf (window-size-w window) w)
  (setf (window-size-h window) h))

(defun close-window (window)
  (if (typep (window-thread window)
             'sb-thread:thread)
      (sb-thread:terminate-thread (window-thread window)))
  (sdl2:destroy-renderer (window-sdl2-renderer window))
  (sdl2:destroy-window (window-sdl2-window window))
  (setf (window-sdl2-renderer window) nil)
  (setf (window-sdl2-window window) nil))

(defun clear-window (window)
  (sdl2:render-clear (window-sdl2-renderer window)))

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
    (setf (window-sdl2-window window) sdl2-window)
    (setf (window-sdl2-renderer window) sdl2-renderer)
    (setf (window-sdl2-window-id window)
          (sdl2:get-window-id sdl2-window))
    (window-resize-handler window)
    (sdl2:set-render-draw-blend-mode sdl2-renderer :blend)
    window))

