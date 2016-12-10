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
(define change-rec
  (lambda (e recurring new-sym)
    (if (or (not (pair? e)) (null? e)) (list)
        (if (equal? recurring (car e)) `(,new-sym ,@(change-rec (cdr e) recurring new-sym))
            (if (pair? (car e)) `(,(change-rec (car e) recurring new-sym) ,@(change-rec (cdr e) recurring new-sym))
                `(,(car e) ,@(change-rec (cdr e) recurring new-sym)))))
    ))
        

(define compose-let
  (lambda (e recurring)
    (begin (define new-sym (gensym)) ; Maybe problematic to use "new-sym"
           `(let* ([,new-sym ,recurring]) ,(change-rec e recurring new-sym)))
    ))


;------------------------------main
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

(define cse
  (lambda (e)
    (begin (define recurring (reverseList (remove-dupe (find-recurring e))))
           (display recurring)
           ;(if (null? recurring) e
           ;    (cse (compose-let e recurring));todo
           ;    ))
    )))