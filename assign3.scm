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
                        (if (not (member? 'define nonLocal))
                            (set! nonLocal (append nonLocal (list (car (car items))))))
                        ;(set-car! (car items) (car (car items)))
                        (if (atom? (car (cdr (car items))))
                            (set! local (append local (list (car (cdr (car items))))))
                            (set! local (append local (list (car (car (cdr (car items)))))))
                            )
                        ;(iter (cdr items))
                        )
                    (if (not (or (lambda? (car items)) (let? (car items))))
                        (iter (car items))
                        )
                    )
                    (iter (cdr items))
                )
            )
        )
    (iter body)
    (append (list 'begin) nonLocal)
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

;(run1)

; 3




; 4

(define (for-each-except exception procedure list)
    (define (loop items)
          (cond ((null? items) 'done)
                          ((eq? (car items) exception) (loop (cdr items)))
                                    (else (procedure (car items))
                                                          (loop (cdr items)))))
      (loop list))

(define (inform-about-value constraint)
    (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (member? new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if (not (equal? informant #f)) #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation - CONNECTOR"
                         request))))
    me))



(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (divider m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (/ (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value m1) (get-value product))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (* (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (squareCon m1 product)
  (define (process-new-value)
    (cond 
        ((and (has-value? m1) (= (get-value m1) 0))
            (set-value! product 0 me))
        ((has-value? m1)
            (set-value! product (* (get-value m1) (get-value m1)) me))
        ((has-value? product)
            (set-value! m1 (sqrt (get-value product)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect product me)
  me)

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
    ((connector 'connect) new-constraint))


(define (gravity f m1 m2 r)
    (let ((n (make-connector))
          (d (make-connector))
          (frac (make-connector))
          (g (make-connector))
          (rhs (make-connector)))
      (multiplier m1 m2 n)
	  (squareCon r d)
      (divider n d frac)
      (multiplier g frac f)
      (constant 0.00667300 g)
      'ok))

(define (run4)
    (define f (make-connector))
    (define m1 (make-connector))
    (define m2 (make-connector))
    (define r (make-connector))
    (gravity f m1 m2 r)
    (set-value! m1 1 this) 
    (set-value! m2 4 this)
    (set-value! f 0.00667300 this)
    (inspect (get-value r))
	)
;(run4)

; 5

(define (barrier)
    (define y 0)
    (define (set x)
        (set! y x)
        )
    (define (install)
        (lock)
        (set! y (- y 1))
        (unlock)
        )
    (define (remove)
        (if (= y 0)
            #t
            (begin
                (sleep 1)
                (remove)
                )
            )
        )
    this
    )

(define (run5)
    (define b (barrier))
    (define (helper)
        ((b'install))
        ((b'remove))
        )
    ((b'set) 3)
    (println "Starting thread 1")
    (define x (thread (begin (helper))))
    (println "Starting thread 2")
    (define y (thread (begin (helper))))
    (println "Starting thread 3")
    (define z (thread (begin (helper))))
    (tjoin x)
    (tjoin y)
    (tjoin z)
    (println "All threads finished")
    )
;(run5)

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

;(run6)


; 8

(define (x-powers x e) (scons (^ x e) (x-powers x (+ e 2))))

(define alt-ones (cons-stream 1 (cons-stream -1 alt-ones)))

(define (sop operator s1 s2)
	(scons (operator (scar s1) (scar s2)) (sop operator (scdr s1) (scdr s2)))
	)

(define (denoms)
	(define (helper x c)
		(scons x (helper (* 1.0 x (+ c 1) (+ c 2)) (+ c 2))))
	(scons 1 (helper 2 2))
)

(define (numers x)
    (sop * alt-ones (x-powers x 0))
    )

(define (mystery x)
    (sop / (numers x) (denoms))
    )

(define (ps-mystery x)
    (partial-sums (mystery x))
    )

(define (partial-sums s)
	(cons-stream (car s) 
				 (add-streams (partial-sums s)
							  (stream-cdr s)))) 

(define (square x)
	(* x x)
	)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
		(define x (+ s0 (* -2 s1) s2))
		(if (= x 0)
    		(cons-stream s1 (euler-transform (stream-cdr s)))
    		(cons-stream (- s2 (/ (square (- s2 s1)) x))
                 		 (euler-transform (stream-cdr s)))
			)
		)
	)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (acc-mystery x)
	(euler-transform (ps-mystery x))
	)

(define (super-mystery x)
	(accelerated-sequence euler-transform (ps-mystery x))
	)

(define (run8)
	(sdisplay (mystery 2) 6)
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
    (inspect (sdisplay (ramanujan) 4))
    )

(println "assignment 3 loaded!")
