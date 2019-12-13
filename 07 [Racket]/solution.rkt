#lang racket

(require racket/match)
(require racket/undefined)
(require racket/block)

(define program (file->string "input"))
;(define program "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(define (codes input) 
  (map (lambda (elem) (string->number elem)) (string-split input ",")))

(define amplifier% 
  (class object%
    (init codes)

    (define tape codes)
    (define inputs '())
    (define position 0)
    (define output '())
    (define paused false)

    (super-new)

    (define/public (add-input elem)
      (set! inputs (append inputs (list elem))))

    (define/public (get-output) output)

    (define/public (run-to-pause)
      (set! paused false)
      ;(printf "[~s] ~s " position inputs)
      (let* ([instruction (get-instruction)]
        [opcode (first instruction)]
        [modes (second instruction)])
        (if (not (= opcode 99))
          (block 
            (match opcode
              [1 (do-the-math + modes)]
              [2 (do-the-math * modes)]
              [3 (consume-input modes)]
              [4 (append-output modes)]
              [5 (jump-if true modes)]
              [6 (jump-if false modes)]
              [7 (compare < modes)]
              [8 (compare = modes)])
            (if paused opcode (run-to-pause)))
          opcode)))
    
    (define/public (run-to-completion)
      (let ([exit-code (run-to-pause)])
        (if (not (= 99 exit-code))
          (run-to-pause)
          exit-code)))

    ; returns '(opcode modes)
    (define/private (get-instruction)
      (let* ([opcode (list-ref tape position)]
        [matches (regexp-match #px"(\\d+)(\\d{2})" (number->string opcode))])
        (if (not matches) 
          (list opcode '(0 0 0)) ; simple
          (let* ([opcode (string->number (list-ref matches 2))]
            [modes-int (string->number (list-ref matches 1))]
            [modes (list (modulo modes-int 10) (modulo (quotient modes-int 10) 10) (modulo (quotient modes-int 100) 100))])
            (list opcode modes)))))

    (define/private (get-value pos mode)
      (if (zero? mode)
        (list-ref tape (list-ref tape pos))
        (list-ref tape pos)))
    
    ; operations
    (define/private (do-the-math operator modes)
      (let* ([arg1 (get-value (add1 position) (list-ref modes 0))]
        [arg2 (get-value (+ 2 position) (list-ref modes 1))]
        [output-pos (get-value (+ 3 position) 1)]
        [result (operator arg1 arg2)])
        (set! tape (list-set tape output-pos result))
        (set! position (+ 4 position))))

    (define/private (consume-input modes)
      (let ([pos (get-value (add1 position) 1)])
        (set! tape (list-set tape pos (first inputs)))
        (set! inputs (drop inputs 1))
        (set! position (+ 2 position))))
    
    (define/private (append-output modes)
      (let ([value (get-value (add1 position) (list-ref modes 0))])
        (set! output (append output (list value)))
        (set! position (+ 2 position))
        (set! paused true)))

    (define/private (jump-if bool modes)
      (let ([value (get-value (add1 position) (list-ref modes 0))])
        (if (equal? bool (not (= 0 value)))
          (set! position (get-value (+ 2 position) (list-ref modes 1)))
          (set! position (+ 3 position)))))

    (define/private (compare operator modes)
      (let* ([arg1 (get-value (+ 1 position) (list-ref modes 0))]
        [arg2 (get-value (+ 2 position) (list-ref modes 1))]
        [target (get-value (+ 3 position) 1)])
        (if (operator arg1 arg2)
          (set! tape (list-set tape target 1))
          (set! tape (list-set tape target 0)))
        (set! position (+ 4 position))))
    ))

;(define amp (new amplifier% [codes (codes program)]))

(define outputs (map
  (lambda (variant)
    (foldl (lambda (input phase-setting)
      (define amp (new amplifier% [codes (codes program)]))
      (send amp add-input input)
      (send amp add-input phase-setting)
      (send amp run-to-completion)
      (last (send amp get-output))) 0 variant))
  (permutations '(0 1 2 3 4))))

(define (largest lst) (foldl (lambda (one two) (if (> one two) one two)) 0 lst))

(println (largest outputs))

; part 2
(define (amp-list settings) 
  (for/list ([x settings])
    (let ([amp (new amplifier% [codes (codes program)])])
      (send amp add-input x)
      amp)))

(define (left-shift lst) (append (cdr lst) (cons (car lst) '())))

(define (run-settings phase-settings)
  (let* ([amplifiers (amp-list phase-settings)])
    (run-in-loop amplifiers 0 0)))

(define (run-in-loop amplifiers exit-code input)
  (if (not (= exit-code 99))
    (let* ([amplifier (first amplifiers)])
      (send amplifier add-input input)
      (let* ([new-code (send amplifier run-to-pause)]
        [new-input (last (send amplifier get-output))]
        [new-amplifiers (left-shift amplifiers)])
        (run-in-loop new-amplifiers new-code new-input)))
    (last (send (list-ref amplifiers 3) get-output))))

(define outputs2 (map
  (lambda (variant)
    (run-settings variant))
  (permutations '(5 6 7 8 9))))

(println (largest outputs2))