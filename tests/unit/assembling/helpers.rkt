#lang racket

(provide tests)

(require rackunit
         rackunit/text-ui
         "../../../src/assembling/helpers.rkt")

(define tests
  (test-suite
    "Tests for assembling/helpers.rkt"

    ;; BIT tests

    (check-not-equal? b-one b-zero "Binary digit property")

    (check-true (bit? b-zero) "Bit 0 is a bit")
    (check-true (bit? b-one) "Bit 1 is a bit")
    (check-false (bit? #\2) "Char is non-bit")
    (check-false (bit? empty) "List is non-bit")
    (check-false (bit? "not bit") "String is non-bit")
    (check-false (bit? "0") "Bit-like string is non-bit")
    (check-false (bit? 'not-bit) "Symbol is non-bit")

    ; implementation specific issue, #\0 and #\1 are "bits"
    (check-true (bit? #\0) "Char 0 is a bit")
    (check-true (bit? #\1) "Char 1 is a bit")

    (check-true (b-one? b-one) "Bit 1 is classified as bit-one")
    (check-false (b-one? b-zero) "Bit 0 is classified as non-bit-one")

    (check-true (b-zero? b-zero) "Bit 0 is classified as non-bit-zero")
    (check-false (b-zero? b-one) "Bit 1 is classified as bit-zero")

    ;; BITS tests

    (check-equal? (string->bits "0") (list b-zero) "string->bits conversion basic binary zero")
    (check-equal? (string->bits "1") (list b-one) "string->bits conversion basic binary one")
    (check-equal? (string->bits "01010011") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion multiple bits")
    (check-equal? (string->bits "0101 0011") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion ignores space")
    (check-equal? (string->bits "  0101 0011  ") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion ignores space")
    (check-equal? (string->bits "") empty "string->bits conversion empty string")

    (check-true (bits? (list b-zero)) "Bit lists is a bits")
    (check-true (bits? (list b-one)) "Bit lists is a bits")
    (check-true (bits? (list b-zero b-one)) "Bit lists is a bits")
    (check-true (bits? empty) "Empty bit list is a bits")
    (check-false (bits? (list b-zero b-one #\2)) "Char list is non-bits")
    (check-false (bits? #\2) "Char is non-bits")
    (check-false (bits? "not bit") "String is non-bits")
    (check-false (bits? "0") "Bit-like string is non-bits")
    (check-false (bits? 'not-bit) "Symbol is non-bits")

    ; TODO: still test the following provided functions
    ; [bit-not (-> bit? bit?)]
    ; [bit-and (-> bit? bit? bit?)]
    ; [bit-or (-> bit? bit? bit?)]
    ; [bit-xor (-> bit? bit? bit?)]
    ; [bits? (-> any/c boolean?)]
    ; [bits-not (-> bits? bits?)]
    ; [bits-and (-> bits? bits? bits?)]
    ; [bits-or (-> bits? bits? bits?)]
    ; [bits-xor (-> bits? bits? bits?)]
    ; [bits-xnor (-> bits? bits? bits?)]
    ; [shift (-> bits? exact-nonnegative-integer? bits?)]
    ; [pad-zero (-> bits? exact-nonnegative-integer? bits?)]
    ; [pad-word (-> bits? exact-nonnegative-integer? bits?)]
   ))

(module+ main
  (run-tests tests))
