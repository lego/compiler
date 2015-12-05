#lang racket

(define error-logging 3)
(define warning-logging 2)
(define verbose-logging 1)

(define active-log-level (make-parameter error-logging))

;debug-print: String Int -> Void
;purpose: prints the msg, only if the log level is active
(define (debug-print msg [log-level verbose-logging])
  (if (>= log-level (active-log-level))
    (eprintf "~a~n" msg)
    (void)))

(provide (contract-out
  [debug-print (->* (string?) (number?) void?])
  active-log-level
  error-logging
  warning-logging
  verbose-logging)
