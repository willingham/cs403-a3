(include "streams.scm")
(define (author) (println "AUTHOR: Thomas Willingham twillingham@crimson.ua.edu"))

(define (nonlocals func)
    (define nonLocal (list))
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
    	(eq? (car items) 'let)) (define (iter items)
        ;(inspect items)
		(cond
            ((null? items) 'DONE)
            ((atom? (car items))
                (if (not (member? (car items) local))
                    (begin 
                        (set! nonLocal (append nonLocal (list (car items))))
                        (iter (cdr items)))
                    (iter (cdr items))
                    ))
            ((list? (car items))
                (if (define? (car items))
                    (begin
                        ;(set! nonLocal (append nonLocal (list (car (car items)))))
                        (if (atom? (car (cdr (car items))))
                            (set! local (append local (car (cdr (car items)))))
                            (set! local (append local (car (car (cdr (car items))))))
                            )
                        ;(iter (cdr items))
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
    nonLocal
    )

(define (run1)
    (define (square x)
        (* x x)
        )
    (define (foo a) 
        (+ a b)
        )
    (define (bar)
        (define a 2)
        (define b 3)
        (+ a b c)
        )
    (inspect (nonlocals square))
    (inspect (nonlocals foo))
    (inspect (nonlocals bar))
    )

(run1)




(define (replace sym val) 
    (cond
        ((eq? (car code) sim)
            (set-car! code val))
        )
    )
; 6


(define (big-gulp)
    (define (merge s1 s2)
        (cond 
            ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
                (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                    (cond 
                        ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                        ((> s1car s2car)
                            (cons-stream s2car (merge s1 (stream-cdr s2))))
                        (else
                            (cons-stream s1car
                                (merge (stream-cdr s1)
                                (stream-cdr s2)))))))))
    (define s (scons 1 (merge 
                            (sscale s 7) (sscale s 11))))
    (define s (scdr s)) ;get rid of 1
    s
    )

(define bgs (big-gulp))

(define (run6)
    (inspect (sdisplay bgs 4)) 
    )

(run6)


; 8

(define (denoms)
    (define (helper x c) (scons x (helper (* 1.0 x (* c 1) (+ c 2)) (+ c 2))))
    (scons 1 (helper 2 2))
    )

(define (numers x)
    (sop * x alt-ones (x-powers x 0))
    )

(define (mystery x)
    (sop / (numers x (denoms)))
    )

(define (run8)
    (inspect (mystery 3))
    )


; 9

(define (ramanujan)
    (define (pairs s t)
        (scons
            (list (scar s) (scar t))
            (sshuffle (smap (lambda (x) (list (scar s) x)) (scdr t)) (pairs (scdr s) (scdr t)))
            )
        )
    (define (sshuffle s t)
        (define a (+ (^ (car (scar s)) 3) (^ (cadr (scar s)) 3)))
        (define b (+ (^ (car (scar t)) 3) (^ (cadr (scar t)) 3)))
        (if (< a b)
            (scons (scar s) (sshuffle (scdr s) t))
            (scons (scar t) (sshuffle s (scdr t)))
            )
        )
    (define (duplicates s lastDup)
        (if (and (= (scar s) (scar (scdr s))) (not (= (scar s) lastDup)))
            (scons (scar s) (duplicates (scdr s) (scar s)))
            (duplicates (scdr s) lastDup)
            )
        )
    (define sortedPairs (pairs (ints-from 0) (ints-from 0)))
    (define sortedVals (smap (lambda (x) (+ (^ (car x) 3) (^ (cadr x) 3))) sortedPairs))
    (duplicates sortedVals 0)
    )

(define (run9)
    (sdisplay (ramanujan) 6)
    )

(println "assignment 3 loaded!")
