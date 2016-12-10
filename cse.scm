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

;TODO ignore quoted.
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
    (if (help-find-recuuring (car e) (cdr e)) (help-find-recuuring (car e) (cdr e))
        (if (null? (cddr e)) #f
            (find-recurring (cdr e))))
        
    ))

(define cse
  (lambda (e)
    (find-recurring e)
    ))


