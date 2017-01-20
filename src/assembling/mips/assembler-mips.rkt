#lang racket

(require
  "./scanner-mips.rkt"
  (rename-in "../scanner.rkt")
  "../helpers.rkt"
  "../../helpers.rkt")

(define pretty-out? (make-parameter #f))

; reduced versions of token, for ease of use in match
(define-struct tkn (kind lexeme) #:transparent)

;;;;;;;;;;
;; MIPS ;;
;;;;;;;;;;

;; Instruction signatures

; rd rs rt instr
(define add    (string->bits "000000 00000 00000 00000 00000 100000")) ; ADD rd, rs, rt
(define addu   (string->bits "000000 00000 00000 00000 00000 100001")) ; ADDU rd, rs, rt

(define and    (string->bits "000000 00000 00000 00000 00000 100100")) ; AND rd, rs, rt

; rd rt imm instr
(define addi   (string->bits "001000 00000 00000 00000 00000 100000")) ; ADDI rd, rt. imm
(define addui  (string->bits "001001 00000 00000 00000 00000 100000")) ; ADDUI rd, rt. imm

(define andi   (string->bits "001100 00000 00000 00000 00000 000000")) ; ANDI rd, rs, imm

; fp fmt fs fd instr
(define f.abs  (string->bits "010001 00000 00000 00000 00000 000101")) ; ABS.fmt fs,fd
(define f.add  (string->bits "010001 00000 00000 00000 00000 000000")) ; ADD.fmt fd, fs, 1ft

; imm
(define b      (string->bits "000100 00000 00000 00000 00000 001000")) ; B offset
(define ba     (string->bits "000001 00000 10001 00000 00000 001000")) ; BAL rs, offset (not sure what rs is for)
(define bc1f   (string->bits "010001 01000 000 0 0 00000 00000 000000")) ; BC1F cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc1fl  (string->bits "010001 01000 000 1 0 00000 00000 000000")) ; BC1FL cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc1t   (string->bits "010001 01000 000 0 1 00000 00000 000000")) ; BC1T cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc1tl  (string->bits "010001 01000 000 1 1 00000 00000 000000")) ; BC1TL cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc1f   (string->bits "010010 01000 000 0 0 00000 00000 000000")) ; BC2F cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc1fl  (string->bits "010010 01000 000 1 0 00000 00000 000000")) ; BC2FL cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc2t   (string->bits "010010 01000 000 0 1 00000 00000 000000")) ; BC2T cc, offset (or cc = 0 implied if not supplied, so BC1f offset)
(define bc2tl  (string->bits "010010 01000 000 1 1 00000 00000 000000")) ; BC2TL cc, offset (or cc = 0 implied if not supplied, so BC1f offset)

(define sub    (string->bits "000000 00000 00000 00000 00000 100010"))
(define mult   (string->bits "000000 00000 00000 00000 00000 011000"))
(define multu  (string->bits "000000 00000 00000 00000 00000 011001"))
(define div    (string->bits "000000 00000 00000 00000 00000 011010"))
(define divu   (string->bits "000000 00000 00000 00000 00000 011011"))
(define mfhi   (string->bits "000000 00000 00000 00000 00000 010000"))
(define mflo   (string->bits "000000 00000 00000 00000 00000 010010"))
(define lis    (string->bits "000000 00000 00000 00000 00000 010100"))
(define lw     (string->bits "100011 00000 00000 00000 00000 000000"))
(define sw     (string->bits "101011 00000 00000 00000 00000 000000"))
(define slt    (string->bits "000000 00000 00000 00000 00000 101010"))
(define sltu   (string->bits "000000 00000 00000 00000 00000 101011"))
(define beq    (string->bits "000100 00000 00000 00000 00000 000000"))
(define bne    (string->bits "000101 00000 00000 00000 00000 000000"))
(define jr     (string->bits "000000 00000 00000 00000 00000 001000"))
(define jalr   (string->bits "000000 00000 00000 00000 00000 001001"))

;ADD	Add Word
;ADDI	Add Immediate Word
;ADDIU	Add Immediate Unsigned Word
;ADDU	Add Unsigned Word
;CLO	Count Leading Ones in Word
;CLZ	Count Leading Zeros in Word
;DIV	Divide Word
;DIVU	Divide Unsigned Word
;MADD	Multiply and Add Word to Hi, Lo
;MADDU	Multiply and Add Unsigned Word to Hi, Lo
;MSUB	Multiply and Subtract Word to Hi, Lo
;MSUBU	Multiply and Subtract Unsigned Word to Hi, Lo
;MUL	Multiply Word to GPR
;MULT	Multiply Word
;MULTU	Multiply Unsigned Word
;SEB	Sign-Extend Byte
;SEH	Sign-Extend Halftword
;SLT	Set on Less Than
;SLTI	Set on Less Than Immediate
;SLTIU	Set on Less Than Immediate Unsigned
;SLTU	Set on Less Than Unsigned
;SUB	Subtract Word
;SUBU	Subtract Unsigned Word

;; Register signatures
(define s-register (string->bits "0000 0011 1110 0000 0000 0000 0000 0000"))
(define t-register (string->bits "0000 0000 0001 1111 0000 0000 0000 0000"))
(define d-register (string->bits "0000 0000 0000 0000 1111 1000 0000 0000"))
(define i-register (string->bits "0000 0000 0000 0000 1111 1111 1111 1111"))

;; Useful bits
(define one-bits (bits-not (pad-word (list))))
(define zero-bits (pad-word (list)))

;; Instruction specific register masks
(define s-mask (bits-xor one-bits s-register))
(define st-mask (bits-xor s-mask t-register))
(define std-mask (bits-xor st-mask d-register))
(define d-mask (bits-xor one-bits d-register))
(define sti-mask (bits-xor st-mask i-register))

;int->word: Int -> Word
;turn an int into a twos compliment word
(define (int->word n)
  (cond
    ; twos compliment
    [(< n 0)
      (define abs-word (int->word (abs n)))
      (define ones-compliment (bits-not abs-word))
      (define twos-compliment (string->list
        (number->string (+ (string->number (list->string ones-compliment) 2) 1) 2)))
      twos-compliment]
    [else
      (pad-word (string->list (number->string n 2)))]))

;add-register: Word (listof Int Symbol) -> Word
;; Adds the register values into the instruction word
(define (add-register instr regs)
  (cond
    [(empty? regs) instr]
    [(and (symbol=? (cadar regs) 'i) (list? (caar regs)))
      (add-register (bits-or instr (caar regs)) (cdr regs))]
    [else
      (define register-masked
        (shift
          (if (symbol=? (cadar regs) 'i)
            ((lambda ()
              (define parsed-val (int->word (caar regs)))
              (define imm-val (cons (first parsed-val) (drop parsed-val 17)))
              (pad-word imm-val)))
            (string->list (number->string (caar regs) 2)))
          (second (assoc (cadar regs) '((s 21) (t 16) (d 11) (i 0))))))
      (add-register (bits-or instr register-masked) (cdr regs))]))

;print-bits: Bits -> Void
;; prints the bits (which has to be done a byte at a time)
(define (print-bits bits)
 (cond
    ; if no more bits and doing pretty output, print newine
    [(and (empty? bits) (pretty-out?))
          (printf "~n")]
    ; if doing pretty output, print group of 4's
    [(pretty-out?)
      (printf "~a" (list->string (take bits 4)))
      (if (= (length bits) 4) (void) (printf " "))
      (print-bits (drop bits 4))]
    ; if no more bits, return void
    [(empty? bits) (void)]
    ; else output binary
    [else
      (write-byte (string->number (list->string (take bits 8)) 2))
      (print-bits (drop bits 8))]))

(define (build input)
  (define reduced-input (reduce-tokens input))
  (define program (grand-assemble reduced-input))
  (define program-pass1 (first program))
  (define symbol-table (second program))
  (define relocation-table (third program))
  (define program-pass2 (map (lambda (instr-gen) (instr-gen symbol-table)) program-pass1))
  ;(eprintf "~a~n" reduced-input)
  ;(eprintf "~a~n" program-pass1)
  ;(eprintf "~a~n" program-pass2)
  (define complete-program-generator (generate-program program-pass2 symbol-table relocation-table))
  complete-program-generator)

; usage of build
; (map (lambda (x) (x) complete)
; printing out all labels
; (foldl (lambda (x r) (debug-print (format "~a ~a" (first x) (second x)))) (void) (reverse labels))

(define (generate-program instructions symbol-table relocations)
  (define merl-table (generate-merl-table relocations empty empty))
  (append
    (generate-merl-header (length instructions) merl-table)
    instructions
    merl-table))

(define (generate-merl-header program-length merl-table)
  (verbose "MERL file length: ~a~n" (+ 12 program-length (* 4 (length merl-table))))
  (verbose "MERL program length: ~a~n" (+ 12 program-length))
  (list
    ; merl cookie (beq $0, $0, 2)
    (write-bits (lambda (_) (string->bits "0001 0000 0000 0000 0000 0000 0000 0010")) empty)
    ; entire file length
    (write-bits (lambda (_) (int->word (+ 12 program-length (* 4 (length merl-table))))) empty)
    ; merl header + program length
    (write-bits (lambda (_) (int->word (+ 12 program-length))) empty)))

(define (generate-merl-table relocations imports exports)
  (append
    (foldr (lambda (relocation rst)
      (append (list
        (write-bits (lambda (_) (int->word 1)) empty)
        (write-bits (lambda (_) (int->word relocation)))) rst) empty)
      empty
      relocations)
    ; imports (not implemeneted)
    empty
    ; exports (not implemeneted)
    empty))

; (listof token) -> (listof tkn)
(define (reduce-tokens tokens)
  (map (lambda (large-token) (tkn (token-kind large-token) (token-lexeme large-token))) tokens))

;write-bits: Instruction ->
;creates a delayed write out function for when done processing file
(define (write-bits bits symbol-table)
  (lambda ()
    (cond [(string? bits) (eprintf bits)]
          [else (print-bits (bits symbol-table))])))

;create-instruction: Instruction ->
; creates a function which will get evaluated for pass 2 and will possibly error
; otherwise it
; afterwards it returns write-bits, a delayed write out function
(define (create-instruction instruction [catch-error (lambda (symbol-table) (void))])
  (define bits-generator (if (procedure? instruction) instruction (lambda (symbol-table) instruction)))
  (lambda (symbol-table)
    (catch-error symbol-table)
    (curry write-bits bits-generator symbol-table)))

(define (grand-assemble all-input)
  (define (assemble input [assembled-instr empty] [line 0] [symbol-table empty] [relocatables empty])
    (fatal "line: ~a~n" line)
    (define next-line (+ 4 line))
    (match input
      ; nothing left to parse
      [(list)
        (verbose "FINISHED. Length: ~a~n" (length assembled-instr))
        (list (reverse assembled-instr) symbol-table relocatables)]
      ; label definition
      [(list (tkn 'label label))
        (when (member label symbol-table)
          (error 'ERROR (format "Duplicate label: ~a" label)))
        (verbose "LABEL definition: ~a" label)
        (define next-symbol-table (cons (list label line) symbol-table))
        (assemble (rest input) assembled-instr line next-symbol-table relocatables)]
      ; .word LABEL
      [(list (tkn 'dotword "WORD") (tkn 'id label) rest-input ...)
        (verbose ".word LABEL: ~a" label)
        (define instruction
          (create-instruction
            (lambda (symbol-table)
              (int->word (second (assoc label symbol-table))))
            (lambda (symbol-table)
              (unless (assoc label symbol-table)
              (error 'ERROR (format "Label does not exist: ~a" label))))))
        (define next-relocatables (cons line relocatables))
        (assemble rest-input (cons instruction assembled-instr) next-line symbol-table next-relocatables)]
      [_
        (define input-and-instruction
          (match input
            ; word DEC INT
            [(list (tkn 'dotword "WORD") (tkn 'int val) rst ...) ;.WORD
              (verbose ".word DEC: ~a~n" val)
              (cons rst (create-instruction (int->word val)))]
            ; .word HEX
            [(list (tkn 'dotword "WORD") (tkn 'hexint val) rst ...)
              (define bin-val (pad-word (pad-word (string->list (number->string val 2)))))
              (verbose ".word HEX: ~a" bin-val)
              (cons rst (create-instruction bin-val))]
            ; 1 register instructions
            ; jump instructions (s-register)
            [(list (tkn 'id (and op (or "jr" "jalr"))) (tkn 'register reg) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register opcode (list (list reg 's)))))]
             ; math instructions (d-register)
             [(list (tkn 'id (and op (or "mfhi" "mflo" "lis"))) (tkn 'register d) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register opcode (list (list d 'd)))))]
            ; 3 register instructions (s-register, d-register, t-register)
            [(list (tkn 'id (and op (or "add" "add" "slt" "sltu"))) (tkn 'register d) (tkn 'comma _) (tkn 'register s) (tkn 'comma _) (tkn 'register t) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register opcode (list (list d 'd) (list s 's) (list t 't)))))]
            ; 2 register instructions
            ; math instructions (s-register, t-register)
            [(list (tkn 'id (and op (or "mult" "multu" "div" "divu"))) (tkn 'register s) (tkn 'comma _) (tkn 'register t) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register opcode (list (list s 's) (list t 't)))))]
            ; memory instructions (s-register, t-register, immediate value)
            [(list (tkn 'id (and op (or "lw" "sw"))) (tkn 'register t) (tkn 'comma _) (tkn 'int i) (tkn 'lparen _) (tkn 'register s) (tkn 'rparen _) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register opcode (list (list s 's) (list t 't) (list i 'i)))))]
            ; FIXME: allow register to take in a binary string list,
            ;        this will resolve the issue with the HEX inputs not being twos compliment (the error is hexints are converted to twos compliment ints, BAD)
            [(list (tkn 'id (and op (or "lw" "sw"))) (tkn 'register t) (tkn 'comma _) (tkn 'hexint i) (tkn 'lparen _) (tkn 'register s) (tkn 'rparen _) rst ...) ;SUB
              (define opcode (namespace-variable-value (string->symbol op)))
              (cons rst (create-instruction (add-register sw (list (list s 's) (list t 't) (list i 'i)))))]
            ; branch instructions (s-register, t-register, immediate value)
            ; with label offsets
            [(list (tkn 'id (and op (or "beq" "bne"))) (tkn 'register s) (tkn 'comma _) (tkn 'register t) (tkn 'comma _) (tkn 'id label) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (define curr-line line)
              (cons rst (create-instruction
                (lambda (symbol-table)
                  ; -12 for offset due to MERL header3
                  (define jump-diff (/ (- (second (assoc label symbol-table)) curr-line 16) 4))
                  (add-register opcode (list (list s 's) (list t 't) (list jump-diff 'i))))
                (lambda (symbol-table)
                  (unless (assoc label symbol-table)
                    (error 'ERROR (format "Label does not exist: ~a" label))))))]
            ; with HEX/DEC int offsets
            ; FIXME check offset size, due to an upper bound issue (not checked, could be larger than 16 bits?)
            [(list (tkn 'id (and op (or "beq" "bne"))) (tkn 'register s) (tkn 'comma _) (tkn 'register t) (tkn 'comma _) (tkn (and int-type (or 'int 'hexint)) val) rst ...)
              (define opcode (namespace-variable-value (string->symbol op)))
              (define imm-val
                (match int-type
                  ['hexint (pad-word (string->list (number->string val 2)) 32)]
                  ['int val]))
              (cons rst (create-instruction (add-register opcode (list (list s 's) (list t 't) (list imm-val 'i)))))]
            [val
              ; gets the verbose version of the bad tkn to obtain the line and character of the token
              (define verbose-token (nth (- (length all-input (length input))) all-input))
              (error 'ERROR (format "Invalid instruction ~a on line ~a, character ~a" val (token-line verbose-token) (token-char verbose-token)))]))
        (define rest-input (car input-and-instruction))
        (define instruction (cdr input-and-instruction))
        (assemble rest-input (cons instruction assembled-instr) next-line symbol-table relocatables)]))
  (assemble all-input))


(module+ main
  (require racket/cmdline)
  (require
          "./scanner-mips.rkt"
          "../scanner.rkt"
          "../helpers.rkt"
          "../../helpers.rkt")
  (define filename (command-line
    #:program "assembler-mips"
    #:once-each
    [("-p" "--pretty-out") "pretty output asm as ascii, instead of raw binary"  (pretty-out? #t)]
    #:once-any
    [("-w" "--warnings") "show warning logs" (set-logger-level! warning-logging)]
    [("-v" "--verbose") "show verbose logs" (set-logger-level! verbose-logging)]
    #:args (filename) ; expect one command-line argument: <filename>
    ; return the argument as a filename to compile
    filename))
  (define all-input (scan-input (open-input-file filename)))
  (define input-lines (string-split all-input "\n"))
  (define tokenized-input (tokenize input-lines))
  (define built-program (build tokenized-input))
  (void (map (lambda (x) (x)) built-program))
  )
