#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND LINE ARGUMENTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/cmdline)

(define error-logging 3)
(define warning-logging 2)
(define verbose-logging 1)

(define pretty-out? (make-parameter #f))
(define active-log-level (make-parameter error-logging))

(command-line #:program "lexer"

              ;#:once-any ; the following are mutually exclusive
              ;[("-v" "--warnings") "show warning logs" (active-log-level warning-logging)]
              ;[("-vv" "--verbose") "show verbose logs" (active-log-level verbose-logging)]

              #:once-each
              [("-p" "--pretty-out") "pretty output"  (pretty-out? #t)]
              [("-w" "--warnings") "show warning logs" (active-log-level warning-logging)]
              [("-v" "--verbose") "show verbose logs" (active-log-level verbose-logging)])


;debug-print: String Int ->
;purpose: prints the msg, only if the log level is active
(define (debug-print msg [log-level verbose-logging])
  (if (>= log-level (active-log-level))
    (eprintf "~a" msg)
    (void)))

(require parser-tools/yacc
         parser-tools/lex
         "yacc-to-scheme.rkt"
         (prefix-in : parser-tools/lex-sre))

;;; (define-tokens t (BATATA INT VAR FNCT))
;;; (define-empty-tokens et (newline = OP CP + - * / ^ EOF NEG))
;;; (define-empty-tokens empty-tokens (newline % ( ) * + , - / : =))
;;; (define calcp (trans "languages/beginner.y"))
;;; (eprintf "~a" calcp)

(define-tokens value-tokens (INT VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))

 (upper-letter (:/ #\A #\Z))

 ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
 (digit (:/ "0" "9")))
 
(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
  ;;;  ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-INT (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-INT (string->number lexeme))]))
   

(struct tree (rule data children) #:transparent)
;; RULES:
;; 'OPERATOR
;; 'CONSTANT

;; DATA: rule specific data.
;; 'OPERATOR -> (OPERATOR ARGC)
;;    OPERATOR = One of '(+ * - ...)
;;    ARGC = int
;; 'CONSTANT -> (TYPE)

;; CHILDREN:
;; List of TREE

(define calcp
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(INT) (tree 'CONSTANT '($1) empty)]
        ;;;  [(VAR) (hash-ref vars $1 (lambda () 0))]
        ;;;  [(VAR = exp) (begin (hash-set! vars $1 $3)
        ;;;                      $3)]
        ;;;  [(FNCT OP exp CP) ($1 $3)]

        ;; Provides scoping
         [(OP exp CP) $2]
        ;; BINARY OPERATORS
         [(exp + exp) (tree 'OPERATOR '($2 2) '($1 $3))]
         [(exp - exp) (tree 'OPERATOR '($2 2) '($1 $3))]
         [(exp * exp) (tree 'OPERATOR '($2 2) '($1 $3))]
         [(exp / exp) (tree 'OPERATOR '($2 2) '($1 $3))]
         [(exp / exp) (tree 'OPERATOR '($1 1) '($1))]
         [(exp ^ exp) (tree 'OPERATOR '($1 2) '($1 $3))]
        ;; UNARY OPERATORS
         [(- exp) (prec NEG) (- $2)]
        ))))

(define calcpz (trans "languages/beginner.y"))
(eprintf "~a" calcpz)

           
;; run the calculator on the given input-port       
(define (calc ip)
  (port-count-lines! ip)
  (letrec ((one-line
	    (lambda ()
	      (let ((result (calcp (lambda () (calcl ip)))))
		(when result
                  (printf "~a\n" result)
                  (one-line))))))
    (one-line)))

;;; (calc (open-input-string "x=1\n(x + 2 * 3) - (1+2)*3"))
