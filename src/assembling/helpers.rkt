#lang racket

(provide
  (contract-out
    [b-one? (-> bit? boolean?)]
    [b-zero? (-> bit? boolean?)]
    [bit? (-> any/c boolean?)]
    [bit-not (-> bit? bit?)]
    [bit-and (-> bit? bit? bit?)]
    [bit-or (-> bit? bit? bit?)]
    [bit-xor (-> bit? bit? bit?)]
    [bits? (-> any/c boolean?)]
    [string->bits (-> string? bits?)]
    [bits-not (-> bits? bits?)]
    [bits-and (-> bits? bits? bits?)]
    [bits-or (-> bits? bits? bits?)]
    [bits-xor (-> bits? bits? bits?)]
    [bits-xnor (-> bits? bits? bits?)]
    [shift (-> bits? exact-nonnegative-integer? bits?)]
    [pad-zero (-> bits? exact-nonnegative-integer? bits?)]
    [pad-word (-> bits? exact-nonnegative-integer? bits?)])
  b-one
  b-zero)


(define b-one #\1)
(define b-zero #\0)

;;;;;;
;; BIT OPERATORS
;;;;;;

(define (char-test testing)
  (lambda (char-input) (char=? testing char-input)))

(define (bit? v)
  (and (char? v)
       (or (b-one? v) (b-zero? v))))

(define (b-one? bit)
  ((char-test b-one) bit))

(define (b-zero? bit)
  ((char-test b-zero) bit))

(define (bit-and bit mbit)
  (if (and (b-one? bit) (b-one? mbit)) b-one b-zero))

(define (bit-not bit)
  (if (b-one? bit) b-zero b-one))

(define (bit-or bit mbit)
  (if (or (b-one? bit) (b-one? mbit)) b-one b-zero))

(define (bit-xor bit mbit)
  (if (char=? bit mbit) b-zero b-one))

;;;;;;
;; BITS OPERATORS
;;;;;;

(define (bits? v)
  (and (list? v)
       (andmap bit? v)))

; NOTE: ignores spaces, useful for human-readable padded bit strings
(define (string->bits str)
  (define converted-string (filter (compose not (char-test #\space)) (string->list str)))
  (unless (bits? converted-string)
    (raise-argument-error 'string->bits "string contains non-bits" str))
  converted-string)

;and-bits: Bits Bits -> Bits => ALL SAME SIZE
; ands the two bit lists together
(define (bits-and bits mbits)
  (unless (= (length bits) (length mbits))
    (raise-argument-error 'bits-and "equal size lists" bits mbits))
  (map bit-and bits mbits))

;not-bits: Bits -> Bits => ALL SAME SIZE
; nots the bit list
(define (bits-not bits)
  (map bit-not bits))

;or-bits: Bits Bits -> Bits => ALL SAME SIZE
; ors the two bit lists together
(define (bits-or bits mbits)
  (unless (= (length bits) (length mbits))
    (raise-argument-error 'bits-or "equal size lists" bits mbits))
  (map bit-or bits mbits))

;xor-bits: Bits Bits -> Bits => ALL SAME SIZE
; xors the two bit lists together
(define (bits-xor bits mbits)
  (unless (= (length bits) (length mbits))
    (raise-argument-error 'bits-xor "equal size lists" bits mbits))
  (map bit-xor bits mbits))

;xnor-bits: Bits Bits -> Bits => ALL SAME SIZE
; xnors the two bit lists together
(define (bits-xnor bits mbits)
  (unless (= (length bits) (length mbits))
    (raise-argument-error 'bits-xnor "equal size lists" bits mbits))
  (bits-not (bits-xor bits mbits)))


;shift: Bits Int -> Bits (output Bits size is |lst| + amount)
;purpose: shifts the bits of lst left by amount of bits, padding the right with 0's
(define (shift lst amount)
  (pad-word (reverse (pad-zero (reverse lst) amount))))

;pad-zero: Bits Int -> Bits
;purpose: pads bits by c zeros on the left, making the list longer
(define (pad-zero lst c)
  (cond
    [(> c 0)
      (pad-zero (cons b-one lst) (sub1 c))]
    [else
      lst]))

;pad-word: Bits -> Bits (output Bits size is len)
;purpose: pads bits to the size of a word (32 bits)
(define (pad-word lst [len 32])
  (pad-zero lst (- len (length lst))))
