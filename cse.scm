(load "pattern-matcher.scm")

(define help-find-recuuring
  (lambda (first next)
    (if (or (not (pair? first)) (not (pair? next)) (null? next)) #f
        (if (equal? first (car next)) first
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


(define parse
  (let ((run
	 (compose-patterns

          (pattern-rule
           `(,(? 'proc ) . ,(? 'args))
           (lambda (proc args)
             `(applic ,(parse proc) ,(map parse args))))

          
          
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     (error 'parse
		    (format 'yet e)))))))