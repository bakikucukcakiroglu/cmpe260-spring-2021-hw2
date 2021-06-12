 #lang racket
 (provide (all-defined-out))





(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))





(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))                                         ;RELU


(define mse (lambda (x y) (mul (sub x y) (sub x y))))                                                       ;MSE



(define value-taker0 (lambda( args ) (list-ref args 0 )))

(define value-taker1 (lambda( args ) (list-ref args 1 )))                                                   ;GET-VALUE

(define get-value ( lambda( args )  (if(list? args) (map value-taker1  args ) (num-value args)  )))

(get-value '((num 3 1) (num 4 2) (num 0.0001 0.0)))
(get-value (num 10 2))





(define value-taker2 (lambda( args )  (list-ref args 2)))                                                   ;GET-GRAD

(define get-grad ( lambda( args)   (if(list? args) (map value-taker2  args ) (num-grad args)  )))

(get-grad '((num 3 1) (num 4 2) (num 0.0001 0.0)))
(get-grad (num 10 2))





(define (sum1 lst) (if (empty? lst) 0 (+ (num-value (first lst)) (sum1 (rest lst)))))                       ;ADD

(define (sum2 lst) (if (empty? lst) 0 (+ (num-grad (first lst)) (sum2 (rest lst)))))

(define  (adder args)  (num  (sum1 args)  (sum2 args)) )

(define  add  (lambda args   (adder args)))

(add (num 5 1) (num 2 0.2))
(add (num 5 1) (num 4 3) (num 6 2))




    
(define (multiplier lst) (if (empty? lst) 1 (* (num-value (first lst)) (multiplier (rest lst)))))           ;MUL

(define (grad lst args) (if (empty? lst) 0 (+ (* (/ (multiplier args) (num-value (first lst))) (num-grad (first lst))) (grad (rest lst) args))))
;(define (multiplier args) (num (mul1 args) (grad args args )))
(define mul (lambda args  (num (multiplier args) (grad args args ))))

(mul (num 5 1) (num 2 1))
(mul (num 5 1) (num 4 3) (num 6 2))





(define sub (lambda args (num (- (num-value(first args)) (num-value(second args))) (- (num-grad(first args)) (num-grad(second args))) )))       ;SUB

(sub (num 5 1) (num 2 1))
(sub (num 5 0) (num 2 1))


;(make-hash '([1    one uno]  [2    two dos]))
(define h (make-hash '([7    on un]  [8    tw do])))
h
fprintf(hash-set! (make-hash '([7    on un]  [8    tw do]))  9 '(abi babo) )
h

;(hash-union! (make-hash '([1    one uno]  [2    two dos])) (make-hash '([7    on un]  [8    tw do])))


;(define v 1)
;(define a 3)
;(define h #hash((a . v)))
;(hash h)  
(first (value-taker0 '((a b c d) (1 3 3 7) a)))

; (define (hash-append . hashes)
;     (make-immutable-hasheq
;        (apply append
;           (map hash->list hashes))))

(hash-set (hash 1 3) 2 10)
;(hash-append((a . (num 1 0.0)) (b . (num 3 1.0))))

(eq? (first (value-taker0 '((a b c d) (1 3 3 7) a))) (value-taker2 '((a b c d) (1 3 3 7) a)))

; (define (hash-maker lst args) 
    
;     (if (eq? (first (value-taker0 lst)) (value-taker2 args))

;         (hash-set! (hash (first (value-taker0 lst))  (num (first (value-taker1 lst))  1.0))  (first (value-taker0 lst))  '(num (first (value-taker1 lst))  1.0))
;         0))


(define (hasher a b ) (cons a (num b 1.0)))

(map hasher  (value-taker0 '((-1 2 5 -6) (1 3 3 7))) (value-taker1 '((-1 2 5 -6) (1 3 3 7)))   )

(make-hash (map hasher  (value-taker0 '((-1 2 5 -6) (1 3 3 7))) (value-taker1 '((-1 2 5 -6) (1 3 3 7)))))


 
(define create-hash(lambda args (make-hash (map  (lambda (a b) (cons a (num b  (if (eq? a (value-taker2 args)) 1.0  0.0))) )  (value-taker0 args) (value-taker1 args)) )))

(create-hash '(a b c d) '(1 3 3 7) 'b)
(create-hash '(a b c d) '(1 3 3 7) 'c)





;(lambda (a b) (cons a (num b  1.0)) ) 

;(if (eq? a (value-taker2 args)) 1.0  0.0)




















