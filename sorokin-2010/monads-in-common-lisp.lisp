#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
;; monads-in-common-lisp.lisp
;; Mac Radigan
;; based on Monad Macros in Common Lisp [Sorokin 2010]
;; see https://www.common-lisp.net/project/cl-monad-macros/monad-macros.htm

 (defmacro with-monad ((unit-func funcall-func) &body body)
  `(macrolet
       ((unit (a) (list ',unit-func a))
        (funcall! (k m) (list ',funcall-func k m))
        (progn! (&body ms) (append '(generic-progn!) '(,funcall-func) ms))
        (let! (decls m) (list 'generic-let! ',funcall-func decls m)))
     ,@body))

 (defmacro generic-let! (funcall-func decls m)
   (reduce #'(lambda (decl m)
               (destructuring-bind (x e) decl
                 `(,funcall-func #'(lambda (,x) ,m) ,e)))
           decls
           :from-end t
           :initial-value m))

 (defmacro generic-progn! (funcall-func &body ms)
   (reduce #'(lambda (m1 m2)
               (let ((x (gensym)))
                 `(,funcall-func
                   #'(lambda (, x)
                       (declare (ignore ,x))
                       ,m2)
                   ,m1)))
           ms
           :from-end t))

  ;(with-monad (unitf funcallf)
  ;  (let! ((x1 e1)
  ;         (x2 e2))
  ;        (progn! m1 m2
  ;                (unit (list x1 x2)))))

  (quit)

;; *EOF*
