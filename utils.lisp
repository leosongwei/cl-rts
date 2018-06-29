(defun make-dynamic-string ()
  (make-array 0 :adjustable t
              :fill-pointer 0
              :element-type 'character))
;; (let ((s (make-dynamic-string)))
;;   (vector-push-extend #\a s)
;;   (vector-push-extend #\b s)
;;   s)

(defmacro mvb-let* (bindings &body body)
  (let* ((exp (car bindings))
         (vars (butlast exp))
         (multi-val-exp (car (last exp)))
         (rest-bindings (cdr bindings)))
    (if rest-bindings
        `(multiple-value-bind ,vars ,multi-val-exp
           (mvb-let* ,rest-bindings ,@body))
        `(multiple-value-bind ,vars ,multi-val-exp
           ,@body))))
;; (defun test-binding-0 ()
;;   (values 1 2))
;; (defun test-binding-1 ()
;;   (values 1 2 3))
;; (mvb-let* ((a b (test-binding-0))
;;            (c d e (test-binding-1)))
;;   (list a b c d e))
