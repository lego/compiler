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

    ; [bit? (any/c . -> . boolean?)]
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

    ; [b-one? (bit? . -> . boolean?)]
    (check-true (b-one? b-one) "Bit 1 is classified as bit-one")
    (check-false (b-one? b-zero) "Bit 0 is classified as non-bit-one")

    ; [b-zero? (bit? . -> . boolean?)]
    (check-true (b-zero? b-zero) "Bit 0 is classified as non-bit-zero")
    (check-false (b-zero? b-one) "Bit 1 is classified as bit-zero")

    ; [bit-not (bit? . -> . bit?)]
    (check-equal? (bit-not #\0) #\1 "Bit 0 nots to bit 1")
    (check-equal? (bit-not #\1) #\0 "Bit 1 nots to bit 0")

    ; [bit-and (bit? bit? . -> . bit?)]
    (check-equal? (bit-and #\0 #\0) #\0 "Bit 0 & 0 ands to bit 0")
    (check-equal? (bit-and #\1 #\1) #\1 "Bit 1 & 1 ands to bit 1")
    (check-equal? (bit-and #\1 #\0) #\0 "Bit 1 & 0 ands to bit 0")
    (check-equal? (bit-and #\0 #\1) #\0 "Bit 0 & 1 ands to bit 0")

    ; [bit-or (bit? bit? . -> . bit?)]
    (check-equal? (bit-or #\0 #\0) #\0 "Bit 0 | 0 ors to bit 0")
    (check-equal? (bit-or #\1 #\1) #\1 "Bit 1 | 1 ors to bit 1")
    (check-equal? (bit-or #\1 #\0) #\1 "Bit 1 | 0 ors to bit 1")
    (check-equal? (bit-or #\0 #\1) #\1 "Bit 0 | 1 ors to bit 1")

    ; [bit-xor (bit? bit? . -> . bit?)]
    (check-equal? (bit-xor #\0 #\0) #\0 "Bit 0 ^ 0 ors to bit 0")
    (check-equal? (bit-xor #\1 #\1) #\0 "Bit 1 ^ 1 ors to bit 0")
    (check-equal? (bit-xor #\1 #\0) #\1 "Bit 1 ^ 0 ors to bit 1")
    (check-equal? (bit-xor #\0 #\1) #\1 "Bit 0 ^ 1 ors to bit 1")

    ;; BITS tests

    ; [string->bits (string? . -> . bits?)]
    (check-equal? (string->bits "0") (list b-zero) "string->bits conversion basic binary zero")
    (check-equal? (string->bits "1") (list b-one) "string->bits conversion basic binary one")
    (check-equal? (string->bits "01010011") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion multiple bits")
    (check-equal? (string->bits "0101 0011") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion ignores space")
    (check-equal? (string->bits "  0101 0011  ") (list b-zero b-one b-zero b-one b-zero b-zero b-one b-one) "string->bits conversion ignores space")
    (check-equal? (string->bits "") empty "string->bits conversion empty string")

    ; [bits? (any/c . -> . boolean?)]
    (check-true (bits? (list b-zero)) "Bit lists is a bits")
    (check-true (bits? (list b-one)) "Bit lists is a bits")
    (check-true (bits? (list b-zero b-one)) "Bit lists is a bits")
    (check-true (bits? empty) "Empty bit list is a bits")
    (check-false (bits? (list b-zero b-one #\2)) "Char list is non-bits")
    (check-false (bits? #\2) "Char is non-bits")
    (check-false (bits? "not bit") "String is non-bits")
    (check-false (bits? "0") "Bit-like string is non-bits")
    (check-false (bits? 'not-bit) "Symbol is non-bits")

    ; [bits-not (bits? . -> . bits?)]
    (check-equal? (bits-not (string->bits "0")) (string->bits "1") "Bits not single")
    (check-equal? (bits-not (string->bits "01")) (string->bits "10") "Bits not ")
    (check-equal? (bits-not (string->bits "11111")) (string->bits "00000") "Bits not")

    ; [bits-and (bits? bits? . -> . bits?)]
    (check-equal? (bits-and (string->bits "0") (string->bits "0")) (string->bits "0") "Bits and")
    (check-equal? (bits-and (string->bits "01") (string->bits "10")) (string->bits "00") "Bits and")
    (check-equal? (bits-and (string->bits "11111") (string->bits "11100")) (string->bits "11100") "Bits and")

    ; [bits-or (bits? bits? . -> . bits?)]
    (check-equal? (bits-or (string->bits "0") (string->bits "0")) (string->bits "0") "Bits or")
    (check-equal? (bits-or (string->bits "01") (string->bits "10")) (string->bits "11") "Bits or")
    (check-equal? (bits-or (string->bits "11111") (string->bits "11100")) (string->bits "11111") "Bits or")

    ; [bits-xor (bits? bits? . -> . bits?)]
    (check-equal? (bits-xor (string->bits "0") (string->bits "0")) (string->bits "0") "Bits xor")
    (check-equal? (bits-xor (string->bits "01") (string->bits "10")) (string->bits "11") "Bits xor")
    (check-equal? (bits-xor (string->bits "11111") (string->bits "11100")) (string->bits "00011") "Bits anxord")

    ; [pad-zero (bits? exact-nonnegative-integer? . -> . bits?)]
    (check-equal? (pad-zero (string->bits "11") 3) (string->bits "00011") "Bits pad")
    (check-equal? (pad-zero (string->bits "") 3) (string->bits "000") "Bits pad")
    (check-equal? (pad-zero (string->bits "0") 1) (string->bits "00") "Bits pad")
    (check-equal? (pad-zero (string->bits "0") 0) (string->bits "0") "Bits pad")

    ; [pad-word (bits? exact-nonnegative-integer? . -> . bits?)]
    (check-equal? (pad-word (string->bits "0") 5) (string->bits "00000") "Bits pad-word")
    (check-equal? (pad-word (string->bits "0")) (build-list 32 (lambda (_) #\0)) "Bits pad-word")
    (check-equal? (pad-word (string->bits "1111")) (append (build-list 28 (lambda (_) #\0)) (string->bits "1111")) "Bits pad-word")


    ; [shift (bits? exact-nonnegative-integer? . -> . bits?)]
    ; note: relies on pad-word and pad-zero
    (check-equal? (shift (string->bits "01001") 2) (pad-word (string->bits "0100100")) "Bits shift")
    (check-equal? (shift (string->bits "01001") 0) (pad-word (string->bits "01001")) "Bits shift")
    (check-equal? (shift (string->bits "01001") 1) (pad-word (string->bits "010010")) "Bits shift")
    (check-equal? (shift (string->bits "00") 5) (pad-word (string->bits "0000000")) "Bits shift")
    (check-equal? (shift (string->bits "11") 3) (pad-word (string->bits "11000")) "Bits shift")
    (check-equal? (shift (string->bits "") 3) (pad-word (string->bits "000")) "Bits shift")


   ))

(module+ main
  (run-tests tests))
