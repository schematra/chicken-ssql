(module foops

(make-object derive-object)

(import chicken scheme)
(use matchable)
(import-for-syntax chicken)
(begin-for-syntax
 (use srfi-1))

(define-for-syntax args-without-self 
  '(if (and (pair? args) (procedure? (car args)))
       (cdr args)
       args))

(define-syntax derive-object
  (ir-macro-transformer
   (lambda (x i c)
     (let-optionals (second x)
         ((ancestor #f)
          (self #f)
          (super #f))
       
       `(let ((ancestor ,ancestor))
          (letrec ((self (lambda args
                           (if (null? args)
                               ancestor
                               (let* ((self* (if (and (pair? args) (procedure? (car args)))
                                                 (car args)
                                                 self))
                                      ,@(if self `((,self self*)) '())
                                      ,@(if super
                                            `((,super
                                               (lambda args
                                                 (apply ancestor self* args))))
                                            '())
                                      (args ,args-without-self))
                               
                                 (match args
                                   ,@(map (lambda (m)
                                            `(((quote ,(caar m)) . ,(cdar m))
                                              . ,(cdr m)))
                                          (cddr x))
                                   (_ (apply ancestor self* args))))))))
            self))))))

(define-syntax make-object
  (ir-macro-transformer
   (lambda (x i r)
     `(derive-object 
       ((lambda args 
          (error "message not understood" ,args-without-self))
        . ,(cadr x))
       . ,(cddr x)))))

)