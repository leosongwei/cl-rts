(defun init-sdl2 ()
  (sdl2:init :everything))
;;(init-sdl2)

(defun make-surface (w h)
  (sdl2:create-rgb-surface w h 32
                           :r-mask #xff000000
                           :g-mask #x00ff0000
                           :b-mask #x0000ff00
                           :a-mask #x000000ff))

(defun make-surface-from-ptr (ptr)
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
          (make-surface-from-ptr surface-ptr)))))

;; SDL2_ttf
(cffi:load-foreign-library "libSDL2_ttf.so")
(cffi:defcfun ("TTF_Init" sdl2-ttf-init) :int) ;; Returns: 0 on success, -1 on any error
(cffi:defcfun ("TTF_Quit" sdl2-ttf-quit) :void)

(cffi:defcfun ("TTF_OpenFont" sdl2-ttf-openfont) :pointer
  (filepath :pointer) (ptsize :int))
(defun open-ttf (path ptsize)
  (cffi:with-foreign-string (filepath path)
    (sdl2-ttf-openfont filepath ptsize)))
(cffi:defcfun ("TTF_CloseFont" close-ttf) :void
  (font :pointer))

(cffi:defcfun ("TTF_RenderUTF8_Blended" sdl2-render-utf8-blended) :pointer
  (font :pointer) (string :pointer) (fg-color :uint32))

(declaim (inline map-color))
(defun map-color (r g b &optional (a 255))
  (declare (type (unsigned-byte 8) r g b a))
  (let ((result (+ (ash r 24) (ash g 16) (ash b 8) a)))
    (declare (type (unsigned-byte 32) result))
    result))
(defun render-text-as-surface (text font &optional (r 128) (g 128) (b 128) (a 255))
  (let ((color (map-color r g b a)))
    (cffi:with-foreign-string (string text)
      (sdl2-render-utf8-blended font string color))))

;; #define TTF_STYLE_NORMAL        0x00
;; #define TTF_STYLE_BOLD          0x01
;; #define TTF_STYLE_ITALIC        0x02
;; #define TTF_STYLE_UNDERLINE     0x04
;; #define TTF_STYLE_STRIKETHROUGH 0x08
(cffi:defcfun ("TTF_SetFontStyle" sdl2-ttf-setfontstyle) :void
  (font :pointer) (style :int))
(defun set-font-style (font style-list)
  (let ((style 0))
    (dolist (s style-list)
      (setf style
            (logior style
                    (case s
                      (:normal 0)
                      (:bold 1)
                      (:italic 2)
                      (:underline 4)
                      (:strikethroug 8)
                      (otherwise 0)))))
    (sdl2-ttf-setfontstyle font style)))
