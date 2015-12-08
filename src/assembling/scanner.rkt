#lang racket

(define-struct token (kind lexeme line char) #:transparent)
(define-struct transition (state charset next) #:transparent)

(define finalize-token/c (symbol? any/c exact-nonnegative-integer? . -> . token?))

(provide (contract-out
  [scan-generator (list? list? finalize-token/c . -> .  (-> string? (listof token?)))]
  [scan (list? list? finalize-token/c string? . -> . (listof token?))]
  [struct token ((kind symbol?) (lexeme any/c) (line exact-nonnegative-integer?) (char exact-nonnegative-integer?))]
  [struct transition ((state symbol?) (charset (-> char? boolean?)) (next symbol?))]))

(define (scan-generator transition-list final-states finalize-token)
  (curry scan transition-list final-states finalize-token))

(define (scan transition-list final-states finalize-token str)
  (scan-acc transition-list final-states finalize-token (string->list str) 'start empty empty))

;; scan-acc uses accumulative recursion while running the FSM on the incoming characters
;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)
(define (scan-acc trans final finalize-token initial-cl state acc tacc)
  (define (scan-acc-rec cl state acc tacc)
    (cond
      [(empty? cl)
         (if (member state final)
            (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
              (reverse tacc)
              (reverse (cons (finalize-token state (reverse acc) (- (length initial-cl) (length cl))) tacc)))
            (error 'ERROR "unexpected end of string\n"))]
      [else
        (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
          (cond
            [(and (boolean? trl) (member state final))
              (if (symbol=? state 'whitespace)
                (scan-acc-rec cl 'start empty tacc)
                (scan-acc-rec cl 'start empty (cons (finalize-token state (reverse acc) (- (length initial-cl) (length cl))) tacc)))]
            [(boolean? trl)
              (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
            [(symbol=? state 'comment)
              (reverse tacc)]
            [else
               (scan-acc-rec (rest cl)  (transition-next (first trl)) (cons (first cl) acc) tacc)]))]))
  (scan-acc-rec initial-cl state acc tacc))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))
