(define (author) (println "AUTHOR: Thomas Willingham twillingham@crimson.ua.edu"))


(define (nonlocals func)
    (define nonLocal (list))
  	(define denv (get '__context func))
  	(define body (cdr (get 'code func)))
  	(define local (get 'parameters func))
  	(define (function? items)
    	(and (eq? (car items) 'define)
         	 (list? (car (cdr items)))))
  	(define (define? items)
    	(eq? (car items) 'define))
  	(define (lambda? items)
    	(eq? (car items) 'lambda))
  	(define (let? items)
    	(eq? (car items) 'let)) 
	(define (iter items) 
		(cond
            ((null? items) 'DONE)
            ((atom? (car items))
                (if (not (member? (car items) local))
                    (set! nonLocal (append nonLocal (list (car items))))
                    (iter (cdr items))
                    ))
            ((list? (car items))
                (if (define? (car items))
                    (begin
                        (if (atom? (car (cdr (car items))))
                            (set! local (append local (car (cdr (car items)))))
                            (set! local (append local (car (car (cdr (car items))))))
                            )
                        )
                    (if (not (or (lambda? (car items)) (let? (car items))))
                        (iter (car items))
                        (iter (cdr items))
                        )
                    )
                )
            )
        )
    (iter body)
    (inspect local)
    )



			


(define (run1)
    (define (square x)
        (* x x)
        )
    (define (foo a b c d e) 
        (+ (* a b c d) e)
        )
    (nonlocals square)
    (nonlocals foo)
    )

(run1)




(define (replace sym val) 
    (cond
        ((eq? (car code) sim)
            (set-car! code val))
        )
    )

(define (big-gulp)
    (if !equal)
    )

