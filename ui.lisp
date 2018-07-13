(defparameter *window-list* nil)
(sdl2-ttf-init)
(defparameter *font-default*
  (open-ttf "/usr/share/fonts/TTF/DejaVuSansMono.ttf" 16))

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
    (window-resize-handler window w h)
    (sdl2:set-render-draw-blend-mode sdl2-renderer :blend)
    window))

;; ------------------------------------------------------------
;; UI CLASS

(defun add-accessor (class-name slot-def)
  (when (null (getf (cdr slot-def) :accessor))
    (let* ((slot-name (car slot-def))
           (accessor-name (intern (concatenate 'string
                                               (symbol-name class-name)
                                               "-"
                                               (symbol-name slot-name)))))
      (concatenate 'list slot-def `(:accessor ,accessor-name)))))

(defun add-initarg (slot-def)
  (when (null (getf (cdr slot-def) :initarg))
    (let* ((slot-name (car slot-def))
           (initarg-name (intern (symbol-name slot-name) "KEYWORD")))
      (concatenate 'list slot-def `(:initarg ,initarg-name)))))

(defmacro ui-class-def (name superclass-list slots &rest options)
  (let* ((slots-with-accessor (mapcar (lambda (slot)
                                        (add-accessor name slot))
                                      slots))
         (slots-with-accessor-and-initarg (mapcar #'add-initarg slots-with-accessor)))
    `(defclass ,name ,superclass-list ,slots-with-accessor-and-initarg ,@options)))

;; Everything is a "frame"
(ui-class-def
 frame ()
 ((size-h :initform 0 :type 'integer)
  (size-w :initform 0 :type 'integer)
  (pos-x :initform 0 :type 'integer)
  (pos-y :initform 0 :type 'integer)

  ;; children
  (child-list :initform nil :type 'list)
  (child-size-w :initform -1 :type 'integer)
  (child-size-h :initform -1 :type 'integer)

  ;; SDL
  (sdl-surface :initform nil)
  (surface-update-func :initform nil)

  ;; mouse
  (event-mouse-down :initform nil)
  (event-mouse-up :initform nil)
  (event-mouse-motion :initform nil)
  (event-mouse-wheel :initform nil)
  ;; key
  (event-key-down :initform nil)
  (event-key-up :initform nil)
  (event-text-input :initform nil)))

;; An UI Window contains ONE or None(NIL) ui widget
(ui-class-def
 ui-window (frame)
 ((color :initform '(255 0 0 128))
  (sdl-renderer :initform nil)
  (sdl-texture :initform nil)
  (surface-update-func :initform nil)))

(defun ui-window-surface-update (ui-window)
  (let* ((sdl-surface (frame-sdl-surface ui-window))
         (pixel-format (sdl2:surface-format sdl-surface))
         (color (ui-window-color ui-window))
         (sdl-color (sdl2-map-rgba pixel-format
                                   (nth 0 color) (nth 1 color)
                                   (nth 2 color) (nth 3 color))))
    (sdl2:fill-rect sdl-surface nil sdl-color)
    ;; TODO: blit child widget pixel map here!!!
    (when (ui-window-sdl-texture ui-window)
      (sdl2:destroy-texture (ui-window-sdl-texture ui-window)))
    (setf (ui-window-sdl-texture ui-window)
          (sdl2:create-texture-from-surface (ui-window-sdl-renderer ui-window) sdl-surface))))

(defun show-ui-window (ui-window)
  (sdl2:render-copy (ui-window-sdl-renderer ui-window) (ui-window-sdl-texture ui-window)
                    :dest-rect (sdl2:make-rect (frame-pos-x ui-window)
                                               (frame-pos-y ui-window)
                                               (frame-size-w ui-window)
                                               (frame-size-h ui-window))))

(defun make-ui-window (window w h &optional (x 0) (y 0))
  (let ((ui-window (make-instance 'ui-window
                                  :size-w w :size-h h
                                  :pos-x x :pos-y y
                                  :child-size-w (- w 8)
                                  :child-size-h (- h 8)
                                  :sdl-surface (make-surface w h)
                                  :sdl-renderer (window-sdl2-renderer window)
                                  :surface-update-func #'ui-window-surface-update)))
    ui-window))

;; Button
(ui-class-def
 ui-button (frame)
 ((text :initform "" :type 'string)
  (text-surface :initform nil)
  ;; event
  (event-mouse-motion :initform nil)
  (event-mouse-up :initform nil)
  (event-mouse-down :initform nil)
  (press-func :initform nil)
  ;; looking
  (color :initform '(255 0 0 128))
  (pointed-at-p :initform nil)
  (pressed-down-p :initform nil)
  (activated-p :initform t)))

(defun ui-button-surface-update (ui-button)
  (let ((looking :normal))
    (when (ui-button-pointed-at-p ui-button)
      (setf looking :pointed))
    (when (ui-button-pressed-down-p ui-button)
      (setf looking :pressed))
    (when (not (ui-button-activated-p ui-button))
      (setf looking :deactivated))
    (case looking
      ;; todo
      (otherwise (let* ((color (ui-button-color ui-button))
                        (surface (frame-sdl-surface ui-button))
                        (pixel-format (sdl2:surface-format surface))
                        (sdl-color (sdl2-map-rgba pixel-format
                                                  (nth 0 color) (nth 1 color)
                                                  (nth 2 color) (nth 3 color)))
                        (text-surface (ui-button-text-surface ui-button))
                        (text-w (sdl2:surface-width text-surface))
                        (text-h (sdl2:surface-height text-surface))
                        (center-x (/ (frame-size-w ui-button) 2))
                        (center-y (/ (frame-size-h ui-button) 2))
                        (pos-x (floor (- center-x (/ text-w 2))))
                        (pos-y (floor (- center-y (/ text-h 2)))))
                   (sdl2:fill-rect surface nil sdl-color)
                   (sdl2:blit-surface text-surface nil
                                      surface
                                      (sdl2:make-rect pos-x pos-y text-w text-h)))))))

(defun make-ui-button (frame text f)
  (let* ((size-w (frame-child-size-w frame))
         (size-h (frame-child-size-h frame))
         (text-surface (render-text-as-surface text *font-default* 255 255 255 255))
         (ui-button (make-instance 'ui-button
                                   :size-h size-h :size-w size-w
                                   :text text :text-surface text-surface
                                   :sdl-surface (make-surface size-w size-h))))
    (ui-button-surface-update ui-button)
    ui-button))

