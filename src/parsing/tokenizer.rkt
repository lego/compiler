#lang racket

;;;
;; TYPE DEFINITIONS
;;;

;;;;BINARY DEFINITIONS;;;;
; Bit:
;  either #\0 or #\1
; Bits:
;  is a list of bits, e.g. (list #\0 #\0 #\1)
; Word:
;  bits where the length is exactly 32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND LINE ARGUMENTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/cmdline)

(define error-logging 3)
(define warning-logging 2)
(define verbose-logging 1)

(define pretty-out? (make-parameter #f))
(define active-log-level (make-parameter error-logging))

(command-line #:program "asm"

              ;#:once-any ; the following are mutually exclusive
              ;[("-v" "--warnings") "show warning logs" (active-log-level warning-logging)]
              ;[("-vv" "--verbose") "show verbose logs" (active-log-level verbose-logging)]

              #:once-each
              [("-p" "--pretty-out") "pretty output asm (ascii, not hex)"  (pretty-out? #t)]
              [("-w" "--warnings") "show warning logs" (active-log-level warning-logging)]
              [("-v" "--verbose") "show verbose logs" (active-log-level verbose-logging)])


;;;;;;;;;;;;;
;; HELPERS ;;
;;;;;;;;;;;;;

;; These are generic helpers, unrelated to MIPS

;debug-print: String Int ->
;purpose: prints the msg, only if the log level is active
(define (debug-print msg [log-level verbose-logging])
  (if (>= log-level (active-log-level))
    (eprintf "~a~n" msg)
    (void)))


;;;;;;;;;;;;;;;
;; TOKENIZER ;;
;;;;;;;;;;;;;;;
; provided by CS 241

(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)
(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))


(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

; transistion table
(define asmtrlst
  (list

   ; id
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-alphabetic? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)

   ; numbers
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'start one-to-nine? 'num)
   (make-transition 'num char-numeric? 'num)
   (make-transition 'zero char-numeric? 'BAD)
   (make-transition 'zero char-alphabetic? 'BAD)

   ; comment
   (make-transition 'slash (chartest #\/) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)

   ; grammar
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\;) 'semi)

   ; operators
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'start (chartest #\+) 'plus)
   (make-transition 'start (chartest #\*) 'star)
   (make-transition 'start (chartest #\/) 'slash)
   (make-transition 'start (chartest #\=) 'becomes)
   (make-transition 'start (chartest #\&) 'amp)
   (make-transition 'start (chartest #\%) 'pct)

   ; comparison
   (make-transition 'start (chartest #\>) 'gt)
   (make-transition 'start (chartest #\<) 'lt)
   (make-transition 'gt (chartest #\=) 'ge)
   (make-transition 'lt (chartest #\=) 'le)
   (make-transition 'becomes (chartest #\=) 'eq)
   (make-transition 'start (chartest #\!) 'ne-partial)
   (make-transition 'ne-partial (chartest #\=) 'ne)

   ; brackets
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)
   (make-transition 'start (chartest #\{) 'lbrace)
   (make-transition 'start (chartest #\}) 'rbrace)
   (make-transition 'start (chartest #\[) 'lbrack)
   (make-transition 'start (chartest #\]) 'rbrack)

   ))


; final state table
(define asmfinal
  (list
    'BAD
    'num
    'id
    'label
    'comma
    'lparen
    'rparen
    'lbrace
    'rbrace
    'lbrack
    'rbrack
    'zero
    'comment
    'whitespace
    'gt
    'lt
    'becomes
    'eq
    'ne
    'ge
    'le
    'star
    'plus
    'minus
    'slash
    'becomed
    'amp
    'pct
    'semi
    ))


(define (l->s lst)
  (list->string lst))

(define (s->l lst)
  (string->list lst))

;; scan-acc uses accumulative recursion while running the FSM on the incoming characters
;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)
(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (match (make-token state l)
    [(token 'BAD _) (error 'ERROR "Leading zero on bad things")]
    [(token 'zero _) (make-token 'num 0)]
    [(token 'num val) (make-token 'num (check-int-range (list->number l)))]
    [(token 'id (? is-keyword? word)) (make-token (string->symbol (string-downcase (list->string word))) word)]
    [val val]))

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (is-keyword? word)
  (member (list->string word) '("return" "wain" "new" "delete" "NULL" "println" "while" "if" "else" "int")))

;; scheme supports unbounded integers but MIPS doesn't
(define (check-int-range n)
  (cond
    [(<= -2147483648 n 2147483647) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

; this file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [(> (string-length line) 0) ; ignore blank lines and comment only lines
                                ; when a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else (append scanned (scan-input))])]
    [else (scan-input)]))

; read and create tokens in from STDIN
(define all-input (scan-input))

(for ([token all-input])
  (printf "~a ~a~n"
    (string-upcase (symbol->string (token-kind token)))
    (if (number? (token-lexeme token))
      (token-lexeme token)
      (list->string (token-lexeme token)))))
