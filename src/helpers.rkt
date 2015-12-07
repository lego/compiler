#lang racket

(provide (contract-out
  [fatal (->* (string?) #:rest (listof any/c) void?)]
  [warning (->* (string?) #:rest (listof any/c) void?)]
  [verbose (->* (string?) #:rest (listof any/c) void?)]
  [logger (->* (log-level? string?) #:rest (listof any/c) void?)]
  [flogger (->* (port? log-level? string?) #:rest (listof any/c) void?)]
  [set-logger-level! (-> log-level? void?)]
  [get-logger-level (-> log-level?)])
  fatal-logging
  warning-logging
  verbose-logging)

(define fatal-logging 'fatal)
(define warning-logging 'warning)
(define verbose-logging 'verbose)

; parameter for logger level
(define active-log-level (make-parameter fatal-logging))

; short hand versions
(define (fatal form . v)
  (logger fatal-logging form v))
(define (warning form . v)
  (logger warning-logging form v))
(define (verbose form . v)
  (logger verbose-logging form v))
(define (logger log-level form . v)
  (flogger (current-error-port) log-level form v))

; complete logger to output-port
(define (flogger output-port log-level form . v)
  (if (logging-in-priority log-level (get-logger-level))
    (apply fprintf (cons output-port (cons form v)))
    (void)))


; priority (fatal > warning > verbose)
(define logger-priority (list verbose-logging warning-logging fatal-logging))
; checks if the log-level is active (by priority comparison above)
(define (logging-in-priority log-level active-level)
  (member log-level (member active-level logger-priority)))

; sets logger level
(define (set-logger-level! log-level)
  (active-log-level log-level))

; gets logger level
(define (get-logger-level)
  (active-log-level))

; verifies an input is a logger level
(define (log-level? val)
  (member val logger-priority))
