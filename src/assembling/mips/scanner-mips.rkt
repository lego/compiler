#lang racket

(require
  "../scanner.rkt"
  "../../../languages/mips-asm.rkt"
  (only-in "../../helpers.rkt" chartest))

(provide (contract-out
  [tokenize ((listof string?) . -> . (listof token?))]
  [scan-mips (string? . -> . (listof token?))]))

(define scan-mips (scan-generator transitions final-states finalize-token))

(define (set-line line in)
  (token (token-kind in) (token-lexeme in) line (token-char in)))

(define (tokenize lines)
  (foldr append empty
    (map
      (lambda (str line-num) (map (curry set-line line-num) (scan-mips str)))
      lines
      (range (length lines)))))

(module+ main
  (require racket/cmdline)
  (require
          "../helpers.rkt"
          "../../helpers.rkt")
  (define filename (command-line
    #:program "scanner-mips"
    #:once-any
    [("-w" "--warnings") "show warning logs" (set-logger-level! warning-logging)]
    [("-v" "--verbose") "show verbose logs" (set-logger-level! verbose-logging)]
    #:args (filename) ; expect one command-line argument: <filename>
    ; return the argument as a filename to compile
    filename))
  (define all-input (scan-input (open-input-file filename)))
  (define input-lines (string-split all-input "\n"))
  (tokenize input-lines))
