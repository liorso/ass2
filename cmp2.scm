;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016

(load "pattern-matcher.scm")

;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



;------------------------------LIOR------------------------------------------------------------------------
(define void-object
	(if #f #f))

;-------var----
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

(define reserved-word?
  (lambda (v) (ormap (lambda (x) (equal? v x)) *reserved-words*)
    ))

(define var?
  (lambda (v)
    (and (symbol? v) (not (reserved-word? v)))
    ))

(define optional-lambda?
  (lambda (l) a));TODO!!!
    




;--------------------------------From guide 2015 --------------------------------------------------------------------







(define parse
  (let ((run
	 (compose-patterns
          ;--------------------applications-----------implimented
          (pattern-rule
           `(,(? 'proc (lambda (x) (not (reserved-word? x)))) . ,(? 'args)) ;maybe should change to reserved-symbol??
           (lambda (proc args)
             `(applic ,(parse proc) ,(map parse args))))
          ;---------------------const---------------implimented 
          ;Nil---------------implimented
          (pattern-rule
	   (? 'c null?)
	   (lambda (c) `(const '())))

         ;void---------------implimented
          (pattern-rule
	   (? 'c (lambda (x) (equal? x void-object)))
	   (lambda (c) `(const ,c)))
          ;vector---------------implimented
          (pattern-rule
	   (? 'c vector?)
	   (lambda (c) `(const ,c)))

          ;quote---------------implimented
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))

          ;Boolean--------------implimented
          (pattern-rule
	   (? 'c boolean?)
	   (lambda (c) `(const ,c)))

          ;---------------------char--------------implimented
          (pattern-rule
	   (? 'c char?)
	   (lambda (c) `(const ,c)))

          ;---------------------number--------------implimented
          (pattern-rule
	   (? 'c number?)
	   (lambda (c) `(const ,c)))

          ;---------------------string--------------implimented
          (pattern-rule
	   (? 'c string?)
	   (lambda (c) `(const ,c)))

          ;---------------------var-----------------implimented
	  (pattern-rule
	   (? 'v var?)
	   (lambda (v) `(var ,v)))

          ;---------------------if-------------------implimented
          ;if2
          (pattern-rule
	   `(if ,(? 'test) ,(? 'dit))
	   (lambda (test dit)
	     `(if3 ,(parse test) ,(parse dit) (const ,void-object))))
          ;if3
          (pattern-rule
	   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
	   (lambda (test dit dif)
	     `(if3 ,(parse test) ,(parse dit) ,(parse dif))))

          ;--------------------Disjunctions----------------implimented
          (pattern-rule
	   `(or . ,(? 'exprs))
	   (lambda (exprs)
	     `(or ,(map parse exprs))))

          ;--------------------Lambda----------------not implimented----daniel
          ;regular lambda
          (pattern-rule
	   `(lambda ,(? 'vs) . ,(? 'exprs))
	   (lambda (vs exprs)
	     (append `(lambda-simple ,vs) (map parse exprs))))

          ;lambda optional----------------------------TODO!!!!
          ;(pattern-rule
	  ; `(lambda ,(? 'vs optional-lambda?) . ,(? 'exprs))
	  ; (lambda (vs exprs)
	  ;   (append `(lambda-simle ,vs) (map parse exprs))))

          ;lambda variadic
          ;(pattern-rule
	  ; `(lambda ,(? 'args (lambda (x) (not (list? x)))) . ,(? 'exprs))
	  ; (lambda (args exprs)
	  ;   (append `(lambda-var ,vs) (map parse exprs))))


          ;--------------------Define----------------implimented
          ;regular define
          (pattern-rule
	   `(define ,(? 'v (lambda (x) (not (pair? x)))) ,(? 'e))
	   (lambda (v e)
	     `(define ,`(var ,v) ,(parse e))))

          ;MIT-style define
          (pattern-rule
	   `(define ,(? 'v pair?) . ,(? 'e))
	   (lambda (v e)
	     `(def ,`(var ,(car v)) ,(parse (append `(lambda ,(cdr v)) e))))) ;Didn't test waiting for lambda


          ;--------------------Assignments----------------implimented
          (pattern-rule
	   `(set! ,(? 'v) ,(? 'e))
	   (lambda (v e)
	     `(set ,`(var ,v) ,(parse e))))




          ;--------------------Sequences-----------implimented
          (pattern-rule
	   `(begin . ,(? 'seqs))
	   (lambda (seqs)
	     `(seq ,(map parse seqs))))

;----------------------------------------------------------------------------------------------
          ;---------------------let----------------implimented
          (pattern-rule
	   `(let ,(? 'def) . ,(? 'body))
	   (lambda (def body)
	     (parse `((lambda ,(map car def) ,@body) ,@(map cadr def)) )))
          

          ;---------------------let*-----------------not impl - taken from Mayer 151
	  ;; let*
	  (pattern-rule
	   `(let* ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (expr exprs)
	     (parse (beginify (cons expr exprs)))))
	  (pattern-rule
	   `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
	   (lambda (var val rest exprs)
	     (parse `(let ((,var ,val))
		       (let* ,rest . ,exprs)))))

          
          ;---------------------and----------------not implimented ---TODO: last and
          (pattern-rule
	   `(and)
	   (lambda ()
	     (parse #t)))
          (pattern-rule
	   `(and ,(? 'con))
	   (lambda (con)
	     (parse con)))
          (pattern-rule
	   `(and ,(? 'con1) ,(? 'con2))
	   (lambda (con1 con2)
	     (parse `(if ,con1 ,con2 #f))))
          (pattern-rule
	   `(and . ,(? 'conses))
	   (lambda (conses)
	     `(if3 ,(parse (car conses)) ,(parse (and (cdr conses))) #f))) ;last and
          
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     (error 'parse
		    (format 'yet e)))))))
