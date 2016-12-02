
    
(define (ints-from n)
    (scons n (ints-from (+ n 1))) 
    ;recursive function that has no base case
    )
    
(define (stream-ref s n)
    (cond
        ((= n 0) (stream-car s))
        (else (stream-ref (stream-cdr s) (- n 1)))
        )
    )
    
(define (sscale s x)
    (scons (* x (scar s)) (sscale (scdr s) x))
    )
    
(define scar stream-car)
(define scdr stream-cdr)
(define scons cons-stream)

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (scons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (mul-streams a b) (stream-map2 * a b))
    
(define (sdisplay s n)
    (cond
        ((= n 0) (print "...\n"))
        (else
            (print (stream-car s) ",")
            (sdisplay (stream-cdr s) (- n 1))
            )
        )
    )

(define (add-streams s t)
    ;all these things starts with con-stream
    (cons-stream
        (+ (stream-car s) (stream-car t))
        (add-streams (stream-cdr s) (stream-cdr t))
        )
    ;(stream-map + s t)
    )
    
;helper function that allows us to divide two streams
    (define (div-streams s t)
        (scons
            (/ (scar s) (* 1.0 (scar t)))
            (div-streams (scdr s) (scdr t))
            )
        )
;function to find the partial sum
(define (psum s)
    (scons
        (scar s)
        (psum (scons (+ (scar s) (scar (scdr s))) (scdr (scdr s))))
        )
    )
    
(define (sremove p? s) ;sending it a predicate? and a stream
    (if (p? (scar s))
        (sremove p? (scdr s))
        (scons (scar s) (sremove p? (scdr s)))
        )
    )

(define (divides? x y) (= (% x y) 0))

(define (smap f s) (scons (f (scar s)) (smap f (scdr s))))

;----ADDED FROM BOOK------
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
          
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
  
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (sref s n)
            (if (= n 0)
                (scar s)
                (sref (scdr s) (- n 1))
                )
            )
