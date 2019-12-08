#lang racket

(require racket/match)

;(define program (file->string "input"))
(define program "3,8,1001,8,10,8,105,1,0,0,21,38,59,84,97,110,191,272,353,434,99999,3,9,1002,9,2,9,101,4,9,9,1002,9,2,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,102,5,9,9,101,5,9,9,1002,9,3,9,101,2,9,9,1002,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99")
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

(define (accept-input codes index modes inputs)
    (let ([position (get-value codes (+ 1 index) 1)])
        (list-set codes position (first inputs))))

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

(define (run-instruction opcode modes index codes outputs inputs)
    (match opcode
        [1 (list (do-the-math + codes index modes) outputs 4 inputs)]
        [2 (list (do-the-math * codes index modes) outputs 4 inputs)]
        [3 (list (accept-input codes index modes inputs) outputs 2 (drop inputs 1))]
        [4 (list codes (append-output codes index modes outputs) 2 inputs)]
        [5 (list codes outputs (- (jump-if true codes index modes) index) inputs)]
        [6 (list codes outputs (- (jump-if false codes index modes) index) inputs)]
        [7 (list (compare < codes index modes) outputs 4 inputs)]
        [8 (list (compare = codes index modes) outputs 4 inputs)]))

(define (run-prog index codes outputs inputs)
    (define instruction (get-intructions index codes))
    (define opcode (first instruction))
    (if (not (= opcode 99))
        (let* ([result (run-instruction opcode (second instruction) index codes outputs inputs)]
            [new-codes (list-ref result 0)]
            [new-outputs (list-ref result 1)]
            [offset (list-ref result 2)]
            [new-inputs (list-ref result 3)])
            (run-prog (+ index offset) new-codes new-outputs new-inputs)) 
        outputs))

(define outputs (map
    (lambda (variant)
        (foldl (lambda (input phase-setting)
            (last (run-prog 0 (codes program) '() (list input phase-setting))))
            0 variant))
    (permutations '(0 1 2 3 4))))

(define (largest lst) (foldl (lambda (one two) (if (> one two) one two)) 0 lst))

(println (largest outputs))