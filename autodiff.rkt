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
(define value-taker3 (lambda( args ) (list-ref args 3 )))
(define value-taker4 (lambda( args ) (list-ref args 4 )))



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

(define (grad1 lst args) (if (empty? lst) 0 (+ (* (/ (multiplier args) (num-value (first lst))) (num-grad (first lst))) (grad1 (rest lst) args))))
;(define (multiplier args) (num (mul1 args) (grad args args )))
(define mul (lambda args  (num (multiplier args) (grad1 args args ))))

(mul (num 5 1) (num 2 1))
(mul (num 5 1) (num 4 3) (num 6 2))





(define sub (lambda args (num (- (num-value(first args)) (num-value(second args))) (- (num-grad(first args)) (num-grad(second args))) )))       ;SUB

(sub (num 5 1) (num 2 1))
(sub (num 5 0) (num 2 1))




 
(define create-hash(lambda args (make-hash (map  (lambda (a b) (cons a (num b  (if (eq? a (value-taker2 args)) 1.0  0.0))) )  (value-taker0 args) (value-taker1 args)) )))  ;CREATE-HASH

(create-hash '(a b c d) '(1 3 3 7) 'b)
(create-hash '(a b c d) '(1 3 3 7) 'c)





(define (ex-hand lst args)                                                                                  ;PARSE
    
    (if (eq? lst '())
        '()
        (if (list? lst)
            (cons (ex-hand (car lst) args) (ex-hand (cdr lst) args))
            (if ( or (eq? lst '+) (eq? lst '*) (eq? lst '-)(eq? lst 'mse)(eq? lst 'relu))
                (match lst
                    ['+ 'add]
                    ['* 'mul]
                    ['- 'sub]
                    ['mse 'mse]
                    ['relu 'relu]      ;!!default atmadım
                    )

                (if (number? lst)
                    (num lst 0.0)
                    (hash-ref (value-taker0 args) lst)
                    )))))

(define parse( lambda args  (ex-hand (value-taker1 args) args)  ))

(parse '#hash((x . (num 10  1.0)) (y . (num 20 0.0))) '(+ x y))
(parse (create-hash '(x y) '(10 20) 'x) '(+ x y))
;;(eval (parse '#hash((x . (num 10  1.0)) (y . (num 20 0.0))) '(+ x y))) bu testi terminale yapıştırıp yap, yoksa eval patlatıyor
(parse (create-hash '(x y) '(10 20) 'x) '(+ (* (+ x y) x) (+ y x 5)))





(define grad( lambda args ( num-grad (eval (parse (create-hash (value-taker0 args) (value-taker1 args) (value-taker2 args)) (value-taker3 args) )))))       ;GRAD
;inputları terminalden dene patlatıyor





(define partial-grad(lambda args (map (lambda (a) (if(member a (value-taker2 args)) (grad (value-taker0 args) (value-taker1 args) a (value-taker3 args)) 0.0)) (value-taker0 args))))      ;PARTIAL-GRAD
;inputları terminalden dene 





(define gradient-descent(lambda args (map - (value-taker1 args) (map (lambda (b) (* (value-taker3 args) b)) (partial-grad (value-taker0 args) (value-taker1 args) (value-taker2 args) (value-taker4 args))))))  ;GRADIENT-DESCENT
;inputları terminalden dene patlatıyor





(define optimize(lambda args                        ))







































