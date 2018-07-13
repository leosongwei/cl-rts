(defun init-sdl2 ()
  (sdl2:init :everything))
(init-sdl2)

(cffi:load-foreign-library "libSDL2_image.so")
(cffi:load-foreign-library "libSDL2_ttf.so")
(cffi:load-foreign-library "./cutils/libcint.so")

;; C interface

(defun make-surface (w h)
  (sdl2:create-rgb-surface w h 32
                           :r-mask #xff000000
                           :g-mask #x00ff0000
                           :b-mask #x0000ff00
                           :a-mask #x000000ff))

(defun make-surface-from-ptr (ptr)
  (sdl2-ffi::make-sdl-surface :ptr ptr))

(cffi:defcfun ("SDL_MapRGBA" sdl2-map-rgba) :uint32
  (format-ptr :pointer) (r :uint8) (g :uint8) (b :uint8) (a :uint8))
;; (format t "~X~%" (sdl2-map-rgba (sdl2:surface-format (make-surface 10 10)) 255 0 0 255))
;; > FF0000FF

;;---------------------------------------
;; SDL2_image
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

;;-----------------------------------------
;; SDL2_ttf
(cffi:defcfun ("TTF_Init" sdl2-ttf-init) :int) ;; Returns: 0 on success, -1 on any error
(cffi:defcfun ("TTF_Quit" sdl2-ttf-quit) :void)

(cffi:defcfun ("TTF_OpenFont" sdl2-ttf-openfont) :pointer
  (filepath :pointer) (ptsize :int))
(defun open-ttf (path ptsize)
  (cffi:with-foreign-string (filepath path)
    (sdl2-ttf-openfont filepath ptsize)))
(cffi:defcfun ("TTF_CloseFont" close-ttf) :void
  (font :pointer))

(cffi:defcfun ("wrapper_TTF_RenderUTF8_Blended" sdl2-render-utf8-blended) :pointer
  (font :pointer) (string :pointer) (r :uint8) (g :uint8) (b :uint8) (a :uint8))

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

(cffi:defcfun ("TTF_SizeUTF8" sdl2-ttf-size-text) :int
  (font :pointer) (text :pointer) (width-ptr :pointer) (height-ptr :pointer))

;; -----------------------------
;; sdl2-event

;; typedef enum
;; {
;;     SDL_ADDEVENT,  // 0
;;     SDL_PEEKEVENT, // 1
;;     SDL_GETEVENT   // 2
;; } SDL_eventaction;

(cffi:defcfun ("SDL_PeepEvents" sdl2-peep-events) :int
  (events :pointer)
  (num-events :int)
  (action :int)
  (min-type :uint32)
  (max-type :uint32))

(cffi:defcfun ("SDL_FlushEvents" sdl2-flushevents) :void
  (min-type :uint32)
  (max-type :uint32))

(cffi:defcfun ("SDL_WaitEvent" sdl2-waitevent) :int
  (event-ptr :pointer))

(defun flush-events ()
  (sdl2-flushevents #.(sdl2::enum-value 'sdl2-ffi:sdl-event-type :firstevent)
                    #.(sdl2::enum-value 'sdl2-ffi:sdl-event-type :lastevent)))

(defun get-next-event ()
  "SDL2 shares a global event queue, all events from all windows
   are in this queue.
   The event get from this function must be free, after use."
  (let* ((event (sdl2:new-event))
         (event-ptr (sdl2-ffi::sdl-event-ptr event)))
    (sdl2-peep-events event-ptr
                      1
                      #.(sdl2::enum-value 'sdl2-ffi::sdl-eventaction :getevent)
                      #.(sdl2::enum-value 'sdl2-ffi:sdl-event-type :firstevent)
                      #.(sdl2::enum-value 'sdl2-ffi:sdl-event-type :lastevent))
    event))

(defun wait-sdl-event ()
  (let* ((event (sdl2:new-event))
         (event-ptr (sdl2-ffi::sdl-event-ptr event)))
    (sdl2-waitevent event-ptr)
    event))

