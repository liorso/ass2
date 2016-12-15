(load "pattern-matcher.scm")


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

(define real-help-find-recuuring
  (lambda (first next)
    (if (or (quotedList? first) (not (pair? first)) (not (pair? next)) (null? next)) #f
        (if (equal? first (car next)) first
            (or
             (real-help-find-recuuring first (cdr next))
             (if (pair? (car next)) (real-help-find-recuuring first (car next)) #f)        
             (ormap (lambda (deep-first) (real-help-find-recuuring deep-first next)) first)            
             )))
    ))

(define help-find-recuuring
  (lambda (first next)
    (if (real-help-find-recuuring first next) (real-help-find-recuuring first next) '())))
;(define help-find-recuuring
;  (lambda (first next)
;    (if (or (quotedList? first) (not (pair? first)) (not (pair? next)) (null? next)) '()
;        (if (and (equal? first (car next))) first 
;            (or
;             (if (pair? (car next)) (help-find-recuuring first (car next)) #f)
;             (if (null? (help-find-recuuring first (cdr next))) #f (help-find-recuuring first (cdr next)))
;             (map (lambda (deep-first) (help-find-recuuring deep-first next)) first)             
;             )))
;    ))

;(define find-recurring
;  (lambda (e)
;    (begin (define recurring (help-find-recuuring (car e) (cdr e)))
;           (if recurring recurring
;               (if (null? (cddr e)) #f
;                   (find-recurring (cdr e)))))  
;    ))
(define delet-null
  (lambda (e) (display e)
    (if (null? e) e
        (if (null? (car e)) (delet-null (cdr e))
            (append (car e) (delet-null (cdr e)))))
    ))

(define find-recurring
  (lambda (e)
    (if (null? e) '()
         (append (if (null? (help-find-recuuring (car e) (cdr e))) '()
                            `(,(help-find-recuuring (car e) (cdr e))))
                     (find-recurring (cdr e))))                     
    ))


;-------------------Handaling founded
(define make-list-for-let
	(lambda (recurring syms)
		(if (null? syms) (list)
		`([,(car syms) ,(car recurring)] ,(make-list-for-let (cdr recurring) (cdr syms))))
		))
		
(define change-body
	(lambda (e recurring syms)
		(if (null? e) '())))
        

(define compose-let
  (lambda (e recurring syms)
           `(let* ,(make-list-for-let recurring syms) ,(change-body e recurring syms)))
    )


;------------------------------prepare the recurring
(define change-rec
  (lambda (e recurring new-sym)
    (if (or (not (pair? e)) (null? e)) (list)
        (if (equal? recurring (car e)) `(,new-sym ,@(change-rec (cdr e) recurring new-sym))
            (if (pair? (car e)) `(,(change-rec (car e) recurring new-sym) ,@(change-rec (cdr e) recurring new-sym))
                `(,(car e) ,@(change-rec (cdr e) recurring new-sym)))))
    ))
	
(define remove-dupe
  (lambda (l)
    (if (null? l) '()
        (cons (car l) (remove-dupe (filter (lambda (x) (not (equal? x (car l)))) 
                                    (cdr l)))))
    ))

(define reverseList
  (lambda (l)
    (if (null? l) '()
     (append (reverseList (cdr l)) (list (car l))))
    ))
	
;(define depend ;TODO: test
;	(lambda (first next)
;		(if (or (not (pair? next)) (null? next)) #t
;			(if (equal? fisrt (car next)) #f
;				(and (depend first (cdr next))
;					(depend first (car next)))))
;					))
			

;(define order ;TODO: test
;	(lambda (li n)
;		(if (null? li) (list)
;			(if (and (= n 0)) (exeption) ; TODO
;				(if (depend (car li) (cdr li)) (order (list (cdr li) (car li)) (- n 1))
;					(list (car li) (order (cdr li) (length (cdr li)))))))))


;(define help-cange-in-curring ;TODO: test;
;	(lambda (syms fisrt next)
;		(if (or (not (pair? next)) (null? li)) (list)
;			(if (equal? first (car next)) (list )))))
			
			
(define cange-in-curring
	(lambda (syms first next)
		(if (null? next) (list)
			(list first (change-in-curring (cdr syms) 
							(car (cahge-recurring next first (car syms))) 
                                                        (cdr (cahge-recurring next first (car syms))))))
		))

(define deeplength
  (lambda (x)
    (cond ((null? x) 0)
          ((pair? (car x)) (+ (deeplength (car x)) (deeplength (cdr x))))
          (else (+ 1 (deeplength (cdr x)))))
    ))
		
;--------------------------------------main
(define cse
  (lambda (e)
    (begin (define recurring (remove-dupe (find-recurring e)))
			(if (null? recurring) e	
			 (begin (set! recurring-right-order (sort (lambda (x y) (< (deeplength x) (deeplength y))) recurring))					
					(set! syms (map (lambda (x) (gensym)) (car recurring-right-order)))
					(set! recurring-with-syms (cange-in-curring syms (car recurring-right-order)
                                                                                    (car recurring-right-order)))
recurring-with-syms
                                        ;(compose-let e recurring-with-syms syms))
               ))
    )))