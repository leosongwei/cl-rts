(defun init-sdl2 ()
  (sdl2:init :everything))

(defun clear ()
  (let ((format (sdl2:surface-format *sdl2-surface*)))
    (sdl2:fill-rect *sdl2-surface*
                    nil
                    (sdl2:map-rgb format 0 0 0))))

(defun update()
  (sdl2:update-window *sdl2-window*))

(defun init-window (&key (title "CL-RTS"))
  (setf *sdl2-window* (sdl2:create-window :title title
                                          :w *w* :h *h*
                                          :flags '(:shown)))
  (setf *sdl2-surface* (sdl2:get-window-surface *sdl2-window*))
  (clear)
  (update))

(defun close-window ()
  (sdl2:free-surface *sdl2-surface*)
  (sdl2:destroy-window *sdl2-window*)
  (setf *sdl2-surface* nil)
  (setf *sdl2-window* nil))

(defun make-surface (ptr)
  (sdl2-ffi::make-sdl-surface :ptr ptr))

;; SDL2_image
(cffi:load-foreign-library "libSDL2_image.so")
(cffi:defcfun ("IMG_Init" sdl2-img-init) :int
  (init-flag :int))
(defun init-sdl2-img ()
  "IMG_InitFlags:
    IMG_INIT_JPG = 0x00000001,
    IMG_INIT_PNG = 0x00000002,
    IMG_INIT_TIF = 0x00000004,
    IMG_INIT_WEBP = 0x00000008"
  (sdl2-img-init 2))
(cffi:defcfun ("IMG_Quit" quit-sdl2-img) :void)
(init-sdl2-img)

(cffi:defcfun ("IMG_Load" sdl2-img-load) :pointer
  (filepath :pointer))

(defun img-load (filepath)
  (cffi:with-foreign-string (filepath-c filepath)
    (let ((surface-ptr (sdl2-img-load filepath-c)))
      (if (cffi:null-pointer-p surface-ptr)
          (error (format nil "Image \"~A\" Load Failed" filepath))
          (make-surface surface-ptr)))))
