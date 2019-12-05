#lang racket

(require racket/match)

(define input (file->string "input"))
(define (codes input) 
    (map (lambda (elem) (string->number elem)) (string-split input ",")))

(define (parse-compound-opcode matches)
    (let* ([opcode (string->number (list-ref matches 2))]
        [modes-int (string->number (list-ref matches 1))]
        [modes (list (modulo modes-int 10) (modulo (quotient modes-int 10) 10) (modulo (quotient modes-int 100) 100))])
        (list opcode modes)))

(define (get-intructions index codes)
    (let* ([opcode (list-ref codes index)]
        [matches (regexp-match #px"(\\d+)(\\d{2})" (number->string opcode))])
        (if matches (parse-compound-opcode matches) (list opcode '(0 0 0)))))
    

(define (get-value codes position mode)
    (if (zero? mode) (list-ref codes (list-ref codes position)) (list-ref codes position)))

; operations
(define (do-the-math op codes index modes)
    (let* ([arg1 (get-value codes (+ 1 index) (list-ref modes 0))]
        [arg2 (get-value codes (+ 2 index) (list-ref modes 1))]
        [output-pos (get-value codes (+ 3 index) 1)]
        [result (op arg1 arg2)])
        (list-set codes output-pos result)))

(define (accept-input codes index modes)
    (let ([position (get-value codes (+ 1 index) 1)])
        (list-set codes position user-input)))

(define (append-output codes index modes outputs)
    (define value (get-value codes (+ 1 index) (list-ref modes 0)))
    (append outputs (list value)))

(define (jump-if bool codes index modes)
    (let ([value (get-value codes (+ 1 index) (list-ref modes 0))])
        (if (equal? bool (not (= 0 value))) (get-value codes (+ 2 index) (list-ref modes 1)) (+ index 3))))

(define (compare op codes index modes)
    (let* ([arg1 (get-value codes (+ 1 index) (list-ref modes 0))]
        [arg2 (get-value codes (+ 2 index) (list-ref modes 1))]
        [target (get-value codes (+ 3 index) 1)])
        (if (op arg1 arg2) (list-set codes target 1) (list-set codes target 0))))

(define (run-instruction opcode modes index codes outputs)
    (match opcode
        [1 (list (do-the-math + codes index modes) outputs 4)]
        [2 (list (do-the-math * codes index modes) outputs 4)]
        [3 (list (accept-input codes index modes) outputs 2)]
        [4 (list codes (append-output codes index modes outputs) 2)]
        [5 (list codes outputs (- (jump-if true codes index modes) index))]
        [6 (list codes outputs (- (jump-if false codes index modes) index))]
        [7 (list (compare < codes index modes) outputs 4)]
        [8 (list (compare = codes index modes) outputs 4)]))

(define (run-prog index codes outputs)
    (define instruction (get-intructions index codes))
    (define opcode (first instruction))
    (if (not (= opcode 99))
        (let* ([result (run-instruction opcode (second instruction) index codes outputs)]
            [new-codes (list-ref result 0)]
            [new-outputs (list-ref result 1)]
            [offset (list-ref result 2)])
            (run-prog (+ index offset) new-codes new-outputs)) 
        (printf "~s\n" outputs)))

(define user-input 1)
(run-prog 0 (codes input) '())

(set! user-input 5)
(run-prog 0 (codes input) '())