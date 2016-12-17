(load "pattern-matcher.scm")
(print-gensym #f)

;--------------------------finding
(define quotedList?
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))         
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     #f)))))


(define help-real
  (lambda (first next)(if (or (quotedList? first) (not (pair? first)) (not (pair? next)) (null? next)) #f
        (if (or (equal? first next) (equal? first (car next))) first
            (or
             (help-real first (cdr next))
             (if (pair? (car next)) (help-real first (car next)) #f)                              
             )))
    ))

(define real-help-find-recuuring
  (lambda (first next)
    (if (or (quotedList? first) (not (pair? first)) (not (pair? next)) (null? next)) #f
        (if (or (equal? first next) (equal? first (car next))) first
            (or
             (help-real first (cdr next))
             (if (pair? (car next)) (real-help-find-recuuring first (car next)) #f)
             (ormap (lambda (deep-first) (real-help-find-recuuring deep-first next)) first)                                 
             )))
    ))

(define help-find-recuuring
  (lambda (first next)
    (if (null? next) '()
        (if (real-help-find-recuuring first (car next))
            `(,(real-help-find-recuuring first (car next)) 
                                                      ,@(help-find-recuuring first (cdr next))) 
                                                      (help-find-recuuring first (cdr next))))
    ))

(define find-recurring
  (lambda (e)
    (if (or (not (pair? e)) (null? e)) '()
         (append (help-find-recuuring (car e) (cdr e))
                     (find-recurring (car e)) (find-recurring (cdr e))))                     
    ))


;-------------------making let
(define make-list-for-let
	(lambda (syms recurring)
		(if (null? (cdr syms)) `([,(car syms) ,(car recurring)])
                    `([,(car syms) ,(car recurring)] ,@(make-list-for-let (cdr syms) (cdr recurring))))
		))

(define real-change-body
  (lambda (sym recurring e)
    (cond
      ((not (pair? e)) e)
      ((equal? e recurring) sym)
      ((null? (cdr e)) `(,(real-change-body sym recurring (car e))))
      (else `(,(real-change-body sym recurring (car e)) ,@(real-change-body sym recurring (cdr e)))))
    ))
                                                            
                         

(define change-body
	(lambda (syms recurring e)
		(if (null? (cdr syms)) (real-change-body (car syms) (car recurring) e)
                    (change-body (cdr syms) (cdr recurring) (real-change-body (car syms) (car recurring) e)))
          ))

;------------------------------prepare the recurring
	
(define remove-dupe
  (lambda (l)
    (if (null? l) '()
        (cons (car l) (remove-dupe (filter (lambda (x) (not (equal? x (car l)))) 
                                    (cdr l)))))
    ))


(define deeplength
  (lambda (x)
    (cond ((null? x) 0)
          ((pair? (car x)) (+ (deeplength (car x)) (deeplength (cdr x))))
          (else (+ 1 (deeplength (cdr x)))))
    ))

(define rec-make-list-syms-curring
  (lambda (syms curring lst)
    (if (null? syms) lst
        (rec-make-list-syms-curring (cdr syms) (cdr curring) `(,@lst ,`(,(car syms) ,(car curring)))))
    ))

(define make-list-syms-curring
  (lambda (syms curring)
        (rec-make-list-syms-curring (cdr syms) (cdr curring) `(,`(,(car syms) ,(car curring))))
    ))

(define deep-member
  (lambda (x lst)
    (if (null? lst) #f
        (if (equal? x (car lst)) x
            (begin
              (if (list? (car lst))
                  (let ((r (deep-member x (car lst))))
                    (if r
                        r
                        (deep-member x (cdr lst))))
                  (deep-member x (cdr lst))))))
        ))

(define find-replace
  (lambda (sym first e)
    (cond
      ((not (pair? e)) e)
      ((equal? first e) sym)
      ((deep-member first e) `(,(find-replace sym first (car e)) ,@(find-replace sym first (cdr e))))
      ((null? (cdr e)) (list (find-replace sym first (car e))))
      (else e))
    ))
           

(define real-change-in-curring
  (lambda (sym first next-with-sym)
    (if (null? next-with-sym) (list)
               `(,`(,(caar next-with-sym) ,(find-replace sym first (cadar next-with-sym)))
                                         ,@(real-change-in-curring sym first (cdr next-with-sym))))
    ))


(define cange-in-curring
  (lambda (syms curring-list-with-sym)
    (if (null? (cdr syms)) curring-list-with-sym
        `(,(car curring-list-with-sym) ,@(cange-in-curring (cdr syms)
                                                 (real-change-in-curring (car syms) 
                                                                         (cadar curring-list-with-sym) 
                                                                         (cdr curring-list-with-sym))))
        )))
		
;--------------------------------------main
(define cse
  (lambda (e)
    (begin (define recurring (remove-dupe (find-recurring e)))
			(if (null? recurring) e	
			 (begin (set! recurring-right-order (sort (lambda (x y) (< (deeplength x) (deeplength y))) recurring))					
					(set! syms (map (lambda (x) (gensym)) recurring-right-order))
                                        (set! recurring-with-syms 
                                                                   (make-list-syms-curring syms recurring-right-order))
					(set! recurring-with-syms-changed (cange-in-curring syms recurring-with-syms))
                                        `(,(if (= 1 (length syms)) 'let 'let*) 
                                          ,(make-list-for-let
                                            (map (lambda (x) (if (pair? x) (car x) x)) recurring-with-syms-changed) 
                                            (map (lambda (x) (if (pair? x) (cadr x) x)) recurring-with-syms-changed))
                                          ,(change-body
                                            (map (lambda (x) (if (pair? x) (car x) x)) recurring-with-syms-changed) 
                                            (map (lambda (x) (if (pair? x) (cadr x) x)) recurring-with-syms-changed)
                                            e))
                                        )
                         ))
    ))
