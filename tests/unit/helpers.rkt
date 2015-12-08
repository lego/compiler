#lang racket

(provide tests)

(require rackunit
         rackunit/text-ui
         "../../src/helpers.rkt")

(define tests
  (test-suite
    "Tests for ./helpers.rkt"

    void
    (lambda ()
      (set-logger-level! fatal-logging))

    (test-case
      "Default logging level is fatal"
      (check-equal? (get-logger-level) fatal-logging))

    (test-case
      "Setting logging level"
      (set-logger-level! warning-logging)
      (check-equal? (get-logger-level) warning-logging))

    (test-case
      "Logger active on error level, logging on error level. The log should print."
      (define string "Fatal level logging. Yippee!")
      (define port (open-output-string))
      (flogger port fatal-logging string)
      (check-equal? (get-output-string port) string))

    (test-case
      "Output with verbose logging but only warning level logging level. Nothing should print."
      (set-logger-level! warning-logging)
      (define string "Verbose level logging. No?!")
      (define port (open-output-string))
      (flogger port verbose-logging string)
      (check-equal? (get-output-string port) ""))


    (test-case
      "Format usage with logger"
      (define string "~a formatting ~a~n")
      (define formats (list "well we are" (list 'nicely)))
      (define port (open-output-string))
      (apply flogger port fatal-logging string formats)
      (define formatted-string (apply format string formats))
      (check-equal? (get-output-string port) formatted-string))

   ))

(module+ main
  (run-tests tests))
