(defun text-size (text font-ptr)
  (let ((width 0)
        (height 0))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (cffi:with-foreign-string (text-ptr text)
        (sdl2-ttf-size-text font-ptr text-ptr w h))
      (setf width (cffi:mem-aref w :int))
      (setf height (cffi:mem-aref h :int)))
    (values width height)))
;; (text-size "The quick brown fox jumps over the lazy dog, 你好世界" *font-default*)
;; => 490 19

(defun render-text-as-surface (text font &optional (r 128) (g 128) (b 128) (a 255))
  (let* ((surface (make-surface-from-ptr
                   (cffi:with-foreign-string (string text)
                     (sdl2-render-utf8-blended font string r g b a))))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (values surface width height)))
;; (render-text-as-surface "The quick brown fox jumps over the lazy dog, 你好世界"
;;                         *font-default*
;;                         0 255 0)
;; => #(pointer) 490 19

(defun split-paragraphs (text)
  (let ((paragraphs nil)
        (current-para (make-dynamic-string)))
    (dotimes (i (length text))
      (if (eq #\newline (aref text i))
          (progn (push current-para paragraphs)
                 (setf current-para (make-dynamic-string)))
          (vector-push-extend (aref text i) current-para)))
    (push current-para paragraphs)
    (reverse paragraphs)))

(defun substring (string start end)
  (when (> end (length string))
    (setf end (length string)))
  (subseq string start end))

(defun sum-list (list)
  (let ((sum 0))
    (dolist (n list)
      (incf sum n))
    sum))

(defun render-multiline-paragrah (para font width r g b a)
  (let ((cursor 0)
        (line-length 1)
        (surfaces nil))
    (block :printing-lines
      (loop ;; printing lines
         (if (>= cursor (length para))
             (return-from :printing-lines))
         ;; increasing loop
         (block :increasing
           (loop (let* ((line-text (substring para cursor (+ cursor line-length)))
                        (w (text-size line-text font)))
                   (if (< w width)
                       ;; good
                       (progn (incf line-length)
                              (if (>= line-length (length para))
                                  (return-from :increasing)))
                       ;; bad
                       (return-from :increasing)))))
         ;; decreasing loop
         (block :decreasing
           (loop
              (when (= 1 line-length)
                (return-from :decreasing))
              (let* ((line-text (substring para cursor (+ cursor line-length)))
                     (w (text-size line-text font)))
                (if (< w width)
                    (return-from :decreasing)
                    (decf line-length)))))
         ;; print as surface
         (let ((text (substring para cursor (+ cursor line-length))))
           (incf cursor (length text))
           (push (render-text-as-surface text font r g b a) surfaces))))
    (reverse surfaces)))

(defun render-multiline-text-fixed-width (string font width
                                          &optional (r 255) (g 255) (b 255) (a 255))
  (let* ((paragraphs (split-paragraphs string))
         (surfaces nil))
    (dolist (para paragraphs)
      (if (= 0 (length para))
          (push (render-text-as-surface " " font r g b a) surfaces)
          (dolist (s (render-multiline-paragrah para font width r g b a))
            (push s surfaces))))
    (setf surfaces (reverse surfaces))
    (let* ((h-cursor 0)
           (height (sum-list (mapcar #'sdl2:surface-height surfaces)))
           (surface (make-surface width height)))
      (dolist (s surfaces)
        (sdl2:blit-surface s nil surface
                           (sdl2:make-rect 0 h-cursor
                                           width (sdl2:surface-height s)))
        (incf h-cursor (sdl2:surface-height s)))
      (dolist (s surfaces)
        (sdl2:free-surface s))
      (values surface width height))))

