(load "pattern-matcher.scm")

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

(define help-find-recuuring
  (lambda (first next)
    (if (or (quotedList? first) (not (pair? first)) (not (pair? next)) (null? next)) #f
        (if (and (equal? first (car next))) first
            (or
             (ormap (lambda (deep-first) (help-find-recuuring deep-first next)) first)
             (if (pair? (car next)) (help-find-recuuring first (car next)) #f)
             (help-find-recuuring first (cdr next)))))
    ))

(define find-recurring
  (lambda (e)
    (begin (define recurring (help-find-recuuring (car e) (cdr e)))
           (if recurring recurring
               (if (null? (cddr e)) #f
                   (find-recurring (cdr e)))))
        
    ))

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

(define cse
  (lambda (e)
    (begin (define recurring (find-recurring e))
           (if recurring (cse (compose-let e recurring));todo
               e))
    ))


