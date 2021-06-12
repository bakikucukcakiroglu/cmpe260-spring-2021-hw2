; hasan baki kucukcakiroglu
; 2018400141
; compiling: yes
; complete: yes


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
(define value-taker5 (lambda( args ) (list-ref args 5 )))




(define value-taker1 (lambda( args ) (list-ref args 1 )))                                                   ;GET-VALUE

(define get-value ( lambda( args )  (if(list? args) (map value-taker1  args ) (num-value args)  )))






(define value-taker2 (lambda( args )  (list-ref args 2)))                                                   ;GET-GRAD

(define get-grad ( lambda( args)   (if(list? args) (map value-taker2  args ) (num-grad args)  )))






(define (sum1 lst) (if (empty? lst) 0 (+ (num-value (first lst)) (sum1 (rest lst)))))                       ;ADD

(define (sum2 lst) (if (empty? lst) 0 (+ (num-grad (first lst)) (sum2 (rest lst)))))

(define  (adder args)  (num  (sum1 args)  (sum2 args)) )

(define  add  (lambda args   (adder args)))





    
(define (multiplier lst) (if (empty? lst) 1 (* (num-value (first lst)) (multiplier (rest lst)))))           ;MUL

(define (grad1 lst args) (if (empty? lst) 0 (+ (* (/ (multiplier args) (num-value (first lst))) (num-grad (first lst))) (grad1 (rest lst) args))))

(define mul (lambda args  (num (multiplier args) (grad1 args args ))))






(define sub (lambda args (num (- (num-value(first args)) (num-value(second args))) (- (num-grad(first args)) (num-grad(second args))) )))       ;SUB





 
(define create-hash(lambda args (make-hash (map  (lambda (a b) (cons a (num b  (if (eq? a (value-taker2 args)) 1.0  0.0))) )  (value-taker0 args) (value-taker1 args)) )))  ;CREATE-HASH






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
                    ['relu 'relu]      ;!!default atmadim
                    )

                (if (number? lst)
                    (num lst 0.0)
                    (hash-ref (value-taker0 args) lst)
                    )))))

(define parse( lambda args  (ex-hand (value-taker1 args) args)  ))






(define grad( lambda args ( num-grad (eval (parse (create-hash (value-taker0 args) (value-taker1 args) (value-taker2 args)) (value-taker3 args) )))))       ;GRAD





(define partial-grad(lambda args (map (lambda (a) (if(member a (value-taker2 args)) (grad (value-taker0 args) (value-taker1 args) a (value-taker3 args)) 0.0)) (value-taker0 args))))      ;PARTIAL-GRAD





(define gradient-descent(lambda args (map - (value-taker1 args) (map (lambda (b) (* (value-taker3 args) b)) (partial-grad (value-taker0 args) (value-taker1 args) (value-taker2 args) (value-taker4 args))))))  ;GRADIENT-DESCENT





(define (opt args k) (if (not(= 0 k)) (gradient-descent (value-taker0 args) (opt args (- k 1)) (value-taker2 args) (value-taker3 args) (value-taker5 args)) (value-taker1 args)))           ;OPTIMIZE

(define optimize(lambda args (opt args (value-taker4 args))))








