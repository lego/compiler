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

(command-line #:program "asm"

              ;#:once-any ; the following are mutually exclusive
              ;[("-v" "--warnings") "show warning logs" (active-log-level warning-logging)]
              ;[("-vv" "--verbose") "show verbose logs" (active-log-level verbose-logging)]

              #:once-each
              [("-p" "--pretty-out") "pretty output asm (ascii, not hex)"  (pretty-out? #t)]
              [("-w" "--warnings") "show warning logs" (active-log-level warning-logging)]
              [("-v" "--verbose") "show verbose logs" (active-log-level verbose-logging)])


;debug-print: String Int ->
;purpose: prints the msg, only if the log level is active
(define (debug-print msg [log-level verbose-logging])
  (if (>= log-level (active-log-level))
    (eprintf "~a" msg)
    (void)))



(struct typed-tree (rule tokens children type markers) #:transparent)
(struct tree (rule tokens children) #:transparent)
(struct code-tree (rule tokens children type code) #:transparent)

(define NON-TERMINAL 'nonterminal)
(define NULL "1")


(define ip empty)

(define (my-read-line)
  (read-line))
  ;(if (empty? ip) ((lambda () (set! ip (open-input-file "basic-bad.wlp4i")) (eprintf "WARNING: reading from file~n") (read-line ip))) (read-line ip)))

;; reads tree in
(define (read-tree)
  (define (read-children amt)
    (map (lambda (x) (read-tree)) (range amt)))

  (define line (my-read-line))
  (define tokens (string-split line))
  ;(eprintf "read: ~a~n" line)
  (cond
    [(string=? (first tokens) (string-upcase (first tokens)))
      (define node (tree (first tokens) tokens empty))
      ;(eprintf "leaf: ~a~n" node)
      node]
    [else
      (define children-count (- (length tokens) 1))
      (define children (read-children children-count))
      (define node (tree (first tokens) tokens children))
      ;(eprintf "node: ~a~n" node)
      node]))

; input: tree struct
; fcn : (tree, context), context is a list of each node on the way to this one
(define (traverse input fcn)
  (define (traverse-rec input-tree path)
    (define new-path (cons input-tree path))
    (define children (tree-children input-tree))
    (fcn input-tree path)
    (map (lambda (node) (traverse-rec node new-path)) children))
  (void (traverse-rec input empty)))

;; helpers
(define (tree-type t)
  (first (tree-tokens t)))

; gets the function context, given this nodes context
; returns function name (maybe return function node instead?)
(define (context-func context)
  (cond
    [(empty? context) "error-no-func"]
    [else
      (define first-node (first context))
      (define rule (if (tree? first-node) (tree-rule first-node) (typed-tree-rule first-node)))
      (cond
        [(string=? rule "main") "wain"]
        [(string=? rule "procedure") (second (tree-tokens (second (tree-children first-node))))]
        [else (context-func (rest context))])]))
    ;[else
    ;  (match context
    ;    [(list (tree "main" _ _) ___) "wain"]
    ;    [(list (tree "procedure" _ _)  ___) "name"] ;(list _ (tree "ID" (list _ name) _) ___)
    ;    [_ (context-func (rest context))])]))

(define (context-func-new context)
  (cond
    [(empty? context) "error-no-func"]
    [else
      (define first-node (first context))
      (define rule (if (tree? first-node) (typed-tree-rule first-node) (typed-tree-rule first-node)))
      (cond
        [(string=? rule "main") "wain"]
        [(string=? rule "procedure") (second (typed-tree-tokens (second (typed-tree-children first-node))))]
        [else (context-func-new (rest context))])]))

(define (get-type t)
  (match t
    [(tree "type" (list "type" "INT" "STAR") _) "int*"]
    [(tree "type" (list "type" "INT") _) "int"]
    [_ (error (string-append "type: not supplied a type node, got " (format "~a~n" t)))]))

;; specific problem implementation

(define symbol-tables (make-hash))

(define function-table (make-hash))

(define symbols-used (make-hash))

(define (validate-variables symbol-tables symbols-used)
  (for ([fcn-name (hash-keys symbols-used)])
    (define our-func-table (if (hash-has-key? symbol-tables fcn-name) (hash-ref symbol-tables fcn-name) (make-hash)))
    (for ([var-name (hash-keys (hash-ref symbols-used fcn-name))])
      (cond
        [(not (hash-has-key? our-func-table var-name))
          (eprintf "ERROR: Variable not declared, ~a~n" var-name)
          (exit)]
        [else (void)])
    )))

(define (print-symbol-table table)
  (hash-for-each function-table (lambda (fcn-name _)
    (eprintf "~n~a ~a~n" fcn-name (string-join (second (hash-ref function-table fcn-name)) " "))
    (if (hash-has-key? symbol-tables fcn-name)
      (hash-for-each (hash-ref symbol-tables fcn-name) (lambda (var-name var-type) (eprintf "~a ~a~n" var-name var-type)))
      (void)))))

(define (add-usage name fcn-name)
  ; setup symbol table for fcn if not made already
  (if (not (hash-has-key? symbols-used fcn-name)) (hash-set! symbols-used fcn-name (make-hash)) (void))
  (hash-set! (hash-ref symbols-used fcn-name) name #t))

(define (add-declaration name type fcn-name)
  ; setup symbol table for fcn if not made already
  (if (not (hash-has-key? symbol-tables fcn-name)) (hash-set! symbol-tables fcn-name (make-hash)) (void))

  ; check if we are redeclaring (bad)
  (if (hash-has-key? (hash-ref symbol-tables fcn-name) name) (void (eprintf "ERROR: redeclared variable ~a~n" name) (exit 1)) (void))
  (hash-set! (hash-ref symbol-tables fcn-name) name type))

(define (add-func-declaration name output-type input-types)
  ; check if we are redeclaring (bad)
  (if (hash-has-key? function-table name) (void (eprintf "ERROR: redeclared function ~a~n" name) (exit 1)) (void))

  (hash-set! function-table name (list output-type input-types)))

(define context-use empty)

(define (match-nodes add-func-decl add-decl node context)
  (match node
    ; gets "wain" function signature
    ; main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE
    [(tree "main" _ (list _ _ _ (tree "dcl" _ (list type1-tree _)) _ (tree "dcl" _ (list type2-tree _)) k ...))
      (add-func-decl "wain" "int" (list (get-type type1-tree) (get-type type2-tree)))]
    ; gets procedure function signature
    [(tree "procedure" _ (list _ (tree "ID" (list _ name) _) _ params-tree k ...))
      (define params-list empty)
      (define param-decl (lambda (param-name param-type func-name) (set! params-list (cons (list param-name param-type) params-list))))
      (traverse params-tree (curry match-nodes void param-decl))
      (add-func-decl name "int" (map (lambda (paramlist) (second paramlist)) (reverse params-list)))]
    ; gets all declared variables, and their respective procedure scope
    [(tree "dcl" _ (list
        type-tree
        (tree "ID" (list _ name) _)))
      ;(set! context-use (cons (list name context) context-use))
      (define fcn-name (context-func context))
      (add-decl name (get-type type-tree) fcn-name)]
    ; variable usage
    [(tree "factor" _ (list (tree "ID" (list _ name) _)))
      (define fcn-name (context-func context))
      (add-usage name fcn-name)
      ]
    [(tree "lvalue" _ (list (tree "ID" (list _ name) _)))
        (define fcn-name (context-func context))
        (add-usage name fcn-name)
        ]
    ; function usage without arguments
    [(tree "factor" _ (list (tree "ID" (list _ name) _) (tree "LPAREN" _ _) (tree "RPAREN" _ _)))
      (if (hash-has-key? function-table name) (void) (void (eprintf "ERROR: undeclared function ~a~n" name) (exit 1)))
      (define fcn-name (context-func context))
      (if (hash-has-key? (hash-ref symbol-tables fcn-name) name) (void (eprintf "ERROR: ~a is a variable, overriding the function ~a.~n" name name) (exit 1)) (void))
      ]
    ; function usage with arguments
    [(tree "factor" _ (list (tree "ID" (list _ name) _) (tree "LPAREN" _ _) (tree "arglist" _ _) (tree "RPAREN" _ _)))
      (if (hash-has-key? function-table name) (void) (void (eprintf "ERROR: undeclared function ~a~n" name) (exit 1)))
      (define fcn-name (context-func context))
      (if (hash-has-key? (hash-ref symbol-tables fcn-name) name) (void (eprintf "ERROR: ~a is a variable, overriding the function ~a.~n" name name) (exit 1)) (void))
      ]
    [_ (void)]))

(define (populate-symbol-table node context)
  (match-nodes
    add-func-declaration
    add-declaration
    node
    context))

; actually read it in
(define parse-tree (read-tree))

(traverse parse-tree populate-symbol-table)

(validate-variables symbol-tables symbols-used)

;;;;;;;;;;;;;;;;;;;
;; TYPE CHECKING ;;
;;;;;;;;;;;;;;;;;;;

; input: tree struct
; fcn : (tree, context), context is a list of each node on the way to this one
(define (traverse-rev input fcn)
  (define (traverse-rec input-tree path)
    (define new-path (cons input-tree path))
    (define children (tree-children input-tree))
    (define new-node (tree (tree-rule input-tree) (tree-tokens input-tree) (map (lambda (node) (traverse-rec node new-path)) children)))
    (fcn new-node path))
  (traverse-rec input empty))

(define (is-int node)
  (or
    (and (typed-tree? node) (string? (typed-tree-type node)) (string=? "int" (typed-tree-type node)))
    (and (code-tree? node) (string? (code-tree-type node)) (string=? "int" (code-tree-type node)))
    ))


(define (is-intstar node)
  (or
    (and (typed-tree? node) (string? (typed-tree-type node)) (string=? "int*" (typed-tree-type node)))
    (and (code-tree? node) (string? (code-tree-type node)) (string=? "int*" (code-tree-type node)))
    ))

(define (is-const node)
  (member "const" (typed-tree-markers node)))

; gets constant from a constant tree (expr -> term -> factor -> CONST reduction)
(define (get-const node)
  (string->number (match node
    [(typed-tree "expr" (list "expr" "term") (list term-tree) _ _)
      (get-const term-tree)]
    [(typed-tree "term" (list "term" "factor") (list factor-tree) _ _)
      (get-const factor-tree)]
    [(typed-tree "factor" (list "factor" "NUM") (list num-tree) _ _)
      (get-const num-tree)]
    [(typed-tree "NUM" (list "NUM" const-val) _ _ _)
      const-val])))

; creates constant parse tree with reduction (expr -> term -> factor CONST) or any partial of it
(define (make-const-tree start const-val)
  (match start
    ["expr"
      (typed-tree "expr" (list "expr" "term") (list (make-const-tree const-val "term")) "int" (list "const"))]
    ["term"
      (typed-tree "term" (list "term" "factor") (list (make-const-tree const-val "factor")) "int" (list "const"))]
    ["factor"
      (typed-tree "factor" (list "factor" "NUM") (list (make-const-tree const-val "NUM")) "int" (list "const"))]
    ["NUM"
      (typed-tree "NUM" (list "NUM" const-val) empty "int" (list "const"))]))

(define (type-check type expected msg)
  (cond
    [(not (equal? type expected))
      (eprintf "ERROR: Invalid types usage, expected ~a got ~a. ~a~n" expected type msg)
      (exit 1)]
    [else (void)]))

(define (error-if-true will-error msg)
  (cond
    [will-error
      (eprintf "ERROR: ~a~n" msg)
      (exit 1)]
    [else (void)]))


(define (add-type node type [markers empty])
  (typed-tree (tree-rule node) (tree-tokens node) (tree-children node) type markers))

; adds RHS marker to a typed tree
(define (add-rhs node)
  (typed-tree (typed-tree-rule node) (typed-tree-tokens node) (typed-tree-children node) (typed-tree-type node) (cons "rhs" (typed-tree-markers node))))
; adds LHS marker to a typed tree
(define (add-lhs node)
  (typed-tree (typed-tree-rule node) (typed-tree-tokens node) (typed-tree-children node) (typed-tree-type node) (cons "lhs" (typed-tree-markers node))))

(define (add-op-sides node)
  (define childs (typed-tree-children node))
  (define new-children (list (add-lhs (first childs)) (second childs) (add-rhs (third childs))))
  (if (= (length childs) (length new-children))
    (void)
    (error 'FATAL "Add operation sides failed, lengths mismatched."))
  (typed-tree (typed-tree-rule node) (typed-tree-tokens node) new-children (typed-tree-type node) empty))

(define (validate-types node context)
  ;(debug-print (format "matching: ~a~n" node))
  ;(debug-print (format "matching: ~a => ~a~n" (tree-rule node) (drop (tree-tokens node) 1)))
  (match node

    ;;;;;;;
    ;;; CONSTANT REDUCTION OPIMIZATION (added for A9/A10)
    ;;;;;;
    ; constant marker carry throughs
    [(tree "expr" (list "expr" "term") (list (? is-const term-tree)))
      (debug-print (format "CONST expr~n"))
      (add-type node (typed-tree-type term-tree) (list "const"))]
    [(tree "term" (list "term" "factor") (list (? is-const factor-tree)))
      (debug-print (format "CONST term~n"))
      (add-type node (typed-tree-type factor-tree) (list "const"))]
    [(tree "factor" (list "factor" "NUM") _)
      (debug-print (format "CONST factor~n"))
      (add-type node "int" (list "const"))]

    ; const + const
    [(tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-int is-const expr) _ (? is-int is-const term)))
      (debug-print (format "CONST expr PLUS term~n"))
      (define const-val (+ (get-const expr) (get-const term)))
      (make-const-tree "expr" const-val)]
    ; const - const
    [(tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-int is-const expr) _ (? is-int is-const term)))
      (define const-val (- (get-const expr) (get-const term)))
      (make-const-tree "expr" const-val)]
    ; const - const
    [(tree "term" (list "term" "term" "STAR" "factor") (list (? is-int is-const term) _ (? is-int is-const factor)))
      (define const-val (* (get-const term) (get-const factor)))
      (make-const-tree "term" const-val)]
    ; const / const
    [(tree "term" (list "term" "term" "SLASH" "factor") (list (? is-int is-const term) _ (? is-int is-const factor)))
      (define const-val (quotient (get-const term) (get-const factor))) ; TODO is this the right operation?
      (make-const-tree "term" const-val)]
    ; const % const
    [(tree "term" (list "term" "term" "PCT" "factor") (list (? is-int is-const term) _ (? is-int is-const factor)))
      (define const-val (remainder (get-const term) (get-const factor))) ; TODO is this the right operation?
      (make-const-tree "term" const-val)]

    ;;; REGULAR TYPE CHECKING (all A8)

    ; literals and identifiers
    [(tree "factor" (list _ "ID") (list (typed-tree "ID" (list _ name) _ _ _)))
      (define fcn-name (context-func context))
      (add-type node (hash-ref (hash-ref symbol-tables fcn-name) name))]
    [(tree "factor" (list _ "NULL") _)
      (add-type node "int*")]

    ; parenthesized expressions
    [(tree "factor" (list "factor" "LPAREN" "expr" "RPAREN") (list _ expr1 _))
      (add-type node (typed-tree-type expr1))]
    ; pointers
    [(tree "factor" (list "factor" "AMP" "lvalue") (list _ (? is-int)))
      (add-type node "int*")]
    [(tree "factor" (list "factor" "STAR" "factor") (list _ (? is-intstar)))
      (add-type node "int")]
    [(tree "factor" (list "factor" "NEW" _ _ _ _) (list _ _ _ (? is-int) _))
      (add-type node "int*")]
    ; addition
    [(tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-int lhs) _ (? is-int rhs)))
      (add-op-sides (add-type node "int"))]
    [(tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-intstar) _ (? is-int)))
      (add-op-sides (add-type node "int*"))]
    [(tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-int) _ (? is-intstar)))
      (add-op-sides (add-type node "int*"))]
    ; subtraction
    [(tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-int) _ (? is-int)))
      (add-op-sides (add-type node "int"))]
    [(tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-intstar) _ (? is-int)))
      (add-op-sides (add-type node "int*"))]
    [(tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-intstar) _ (? is-intstar)))
      (add-op-sides (add-type node "int"))]
    ; multiplication and division
    [(tree "term" (list "term" "term" "STAR" "factor") (list (? is-int) _ (? is-int)))
      (add-op-sides (add-type node "int"))]
    [(tree "term" (list "term" "term" "SLASH" "factor") (list (? is-int) _ (? is-int)))
      (add-op-sides (add-type node "int"))]
    [(tree "term" (list "term" "term" "PCT" "factor") (list (? is-int) _ (? is-int)))
      (add-op-sides (add-type node "int"))]
    ; procedure calls
    [(tree "factor" (list "factor" "ID" "LPAREN" "RPAREN") (list (typed-tree "ID" (list _ fcn-name) _ _ _) _ _))
      (define func-arg-types (second (hash-ref function-table fcn-name)))
      (error-if-true (not (empty? func-arg-types)) (format "Function ~a expects parameter types ~a. None provided." fcn-name (string-join func-arg-types ", ")))
      (add-type node "int")]
    [(tree "factor" (list "factor" "ID" "LPAREN" "arglist" "RPAREN") (list (typed-tree "ID" (list _ fcn-name) _ _ _) _ arglist-tree _))
      (define func-arg-types (second (hash-ref function-table fcn-name)))
      (define provided-arg-types (typed-tree-type arglist-tree))
      (error-if-true (not (equal? func-arg-types provided-arg-types))
        (format "Function ~a expected types ~a. Instead provided ~a." fcn-name (string-join func-arg-types ", ") (string-join provided-arg-types ", ")))
      (add-type node "int")]
    ; extra cases (derivation)
    [(tree "expr" (list "expr" "term") (list term-tree))
      (add-type node (typed-tree-type term-tree))]
    [(tree "term" (list "term" "factor") (list factor-tree))
      (add-type node (typed-tree-type factor-tree))]
    [(tree "lvalue" (list "lvalue" "ID") (list (typed-tree "ID" (list _ name) _ _ _)))
      (define fcn-name (context-func context))
      (define var-type (hash-ref (hash-ref symbol-tables fcn-name) name))
      (add-type node (hash-ref (hash-ref symbol-tables fcn-name) name))]
    [(tree "lvalue" (list "lvalue" "STAR" "factor") (list _ (? is-intstar)))
      (add-type node "int")]
    [(tree "lvalue" (list "lvalue" "LPAREN" "lvalue" "RPAREN") (list _ lvalue1 _))
      (add-type node (typed-tree-type lvalue1))]
    ; argument list
    [(tree "arglist" (list "arglist" "expr" "COMMA" "arglist") (list expr1 _ arglistrest))
     (add-type node (cons (typed-tree-type expr1) (typed-tree-type arglistrest)))]
    [(tree "arglist" (list "arglist" "expr") (list expr1))
      (add-type node (list (typed-tree-type expr1)))]
    ; tests
    [(tree "test"
      (list "test" "expr" (or "EQ" "NE" "LT" "LE" "GE" "GT") "expr")
      (list lhs _ rhs))
      (define lhs-type (typed-tree-type lhs))
      (define rhs-type (typed-tree-type rhs))
      (error-if-true (not (equal? lhs-type rhs-type)) (format "Mismatched types in comparison. ~a cmp ~a" lhs-type rhs-type))
      (add-op-sides (add-type node "VALID"))]
    ;;; statements
    ; delete
    [(tree "statement" (list "statement" "DELETE" _ _ _ _) (list _ _ _ (? is-intstar) _))
      (add-type node "VALID")]
    ; control flow (no need to check types)
    [(tree "statement" (list "statement" "WHILE" _ _ _ _ _ _) _)
      (add-type node "VALID")]
    [(tree "statement" (list "statement" "IF" _ _ _ _ _ _ _ _ _ _) _)
      (add-type node "VALID")]
    ; builtin fcn
    [(tree "statement" (list "statement" "PRINTLN" _ _ _ _) (list _ _ (? is-int) _ _))
      (add-type node "VALID")]
    ; assignment
    [(tree "statement" (list "statement" "lvalue" "BECOMES" "expr" "SEMI") (list lvalue1 _ expr1 _))
      (define lhs-type (typed-tree-type lvalue1))
      (define rhs-type (typed-tree-type expr1))
      (error-if-true (not (equal? lhs-type rhs-type)) (format "Attempting to assign type ~a to variable of type ~a." rhs-type lhs-type))
      (add-type node "VALID")]
    ; declaration type checking
    [(tree "dcls" (list "dcls" "dcls" "dcl" "BECOMES" "NULL" "SEMI") (list _ (? is-intstar) _ _z _))
      (add-type node "VALID")]
    [(tree "dcls" (list "dcls" "dcls" "dcl" "BECOMES" "NUM" "SEMI") (list _ (? is-int) _ _ _))
      (add-type node "VALID")]
    [(tree "dcls" (list "dcls") _)
      (add-type node "VALID")]

    [(tree "dcl" _ (list type-tree _))
      (add-type node (typed-tree-type type-tree))]
    [(tree "type" (list "type" "INT") _)
      (add-type node "int")]
    [(tree "type" (list "type" "INT" "STAR") _)
      (add-type node "int*")]

    ; return type validation
    [(tree "main" _ (list _ _ _ _ _ _ _ _ _ _ _ (? is-int) _ _))
      (add-type node "VALID")]
    [(tree "procedure" _ (list _ _ _ _ _ _ _ _ _ (? is-int) _ _))
      (add-type node "VALID")]

    ; catches for rules without any type derivations
    [(tree (or "start" "statements" "paramlist" "params" "procedures") _ _)
      (add-type node "VALID")]

    ; catches for tokens that dont really contribute to typing
    ; but are valid so pass them on
    [(tree (or
      "EOF" "BOF" ; arbitrary stuff
      "LPAREN" "RPAREN"  "LBRACE" "RBRACE"  "LBRACK" "RBRACK" ; syntax stuff
      "INT" ; types
      "COMMA" "SEMI" ; syntax tokens
      "NEW" "DELETE" "PRINTLN" "WAIN" "RETURN" "BECOMES" ; reserved keywords
      "NUM" "NULL" "ID" ; constant tokens
      "IF" "ELSE" "WHILE" ; control flow keywords
      "EQ" "NE" "LT" "GT" "LE" "GE" "PLUS" ; cmp operators
      "MINUS" "SLASH" "PCT" "AMP" "STAR"; operators
      ) _ _) (add-type node "VALID")]

    ; catches for rules with no type checks but still "passed"

    [_
      (define tokens
        (string-join
          (map
            (lambda (node)
              (if
                (and (string? (typed-tree-type node)) (string=? (typed-tree-type node) "VALID"))
                (format "~a" (typed-tree-rule node))
                (format "~a:~a" (typed-tree-rule node) (typed-tree-type node))))
            (tree-children node)) " "))
      (eprintf "ERROR: type check failed for ~a => ~a~n" (tree-rule node) tokens) (exit 1)]))

(define typed-parse-tree (traverse-rev parse-tree validate-types))


;;;;;;;;;;;;;;;;;;;;;
;; CODE GENERATION ;;
;;;;;;;;;;;;;;;;;;;;;

; input: tree struct
; fcn : (tree, context), context is a list of each node on the way to this one
(define (traverse-type input fcn)
  (define (traverse-rec input-tree path)
    (define new-path (cons input-tree path))
    (define children (typed-tree-children input-tree))
    (define new-node (typed-tree (typed-tree-rule input-tree) (typed-tree-tokens input-tree)
      (map (lambda (node)
      (define new-child (traverse-rec node new-path))
      (debug-print (format "code: ~a~n" (code-tree-code new-child)))
      new-child
      ) children) (typed-tree-type input-tree) (typed-tree-markers input-tree)))
    (fcn new-node path))
  (traverse-rec input empty))

(struct lru (elems size) #:transparent)

(struct register (loc) #:transparent)
(struct stack (loc) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;
;; code conventions ;;
;;;;;;;;;;;;;;;;;;;;;;
; expr, term, factor, test rules produce lambdas where the argument specifies (target ops)
; target -> register target for the expression
; ops -> additional operations after expression (e.g. "sw ....") to then store result to mem, for the case where the var assn is in the stack
; this will be useful for reduction instructions in changing destination registers or memory

; we may want to adjust return regs dynamically for non-exported functions through a similar process as above


;;;;;;;;;;;;;;;;;;;;;;
;; MIPS conventions ;;
;;;;;;;;;;;;;;;;;;;;;;

; $0 -> 0
; $3 -> return reg (multi-purpose)
; $1-$20, variable
; $21-$23, constant spaces
; $24 -> LHS result / result
; $25 -> RHS result
; $26 -> 4
; $27 -> 1/true
; $28 -> tests (also for very temp LIS, will not impact stuff)
; $29 -> frame pointer
; $30 -> stack pointer
; $31 -> return pointer

; CONSTS: based on out conventions (can be modified for changing optimization/parameters)
(define RET-REG 3)
(define FOUR-CONST 26)
(define ONE-CONST 27)
(define ZERO-CONST 0)
(define RESULT-REG 24)
(define LHS-REG 24)
(define RHS-REG 25)
(define TEST-REG 28)
(define TEMP-REG 28)
(define FRAME-POINTER 29)
(define STACK-POINTER 30)
(define RETURN-POINTER 31)

(define VAR-ALLOCATABLE-REGS empty) ;(reverse (range 1 21))) ; ordered by priority, which is desc reg order
(define CONST-ALLOCATABLE-REGS empty); (reverse (range 21 24))) ; ordered by priority, which is desc reg order
(define VAR-LRU-MAX (length VAR-ALLOCATABLE-REGS))
(define CONST-LRU-MAX (length CONST-ALLOCATABLE-REGS))

; NOTE: need to track used registers for backup in subroutines (A10)

(define while-count 0)
(define if-count 0)

;(define func-regs VAR-ALLOCATABLE-REGS) ; free registered unused so far for variables
;(define func-lru (lru empty VAR-LRU-MAX)) ; LRU of variables and registers
;(define func-const-lru (lru empty CONST-LRU-MAX)) ; LRU of variables and registers

;(define func-locations (make-hash)) ; mapping of variable name to it stack position if it ends up requiring the stack

(define func-stacks (make-hash))
(define func-params (make-hash))
(for ([func-name (hash-keys function-table)])
  (hash-set! func-stacks func-name (make-hash))
  (hash-set! func-params func-name 0))


; A10 (subroutines)
; (define location-tables (make-hash))

; -> (list NewLRU empty) if size not exceeded
; -> (list NewLRU removed-elem) if exceeded
(define (lru-add lru-inst elem)
  (cond
    [(= (lru-size lru-inst) (length (lru-elems lru-inst)))
      (define removed-last (drop (lru-elems lru-inst) 1))
      (list (lru (cons elem removed-last) (lru-size lru-inst))
            (last (lru-elems lru-inst)))]
    [else
      (list (lru (cons elem (lru-elems lru-inst)) (lru-size lru-inst))
            empty)]))

; -> (list NewLRU empty) if size not exceeded
; -> (list NewLRU removed-elem) if exceeded
(define (lru-remove lru-inst elem)
  (define removed-last (drop (lru-elems lru-inst) 1))
  (list (lru (cons elem removed-last (lru-size lru-inst)))
        (last (lru-elems lru-inst))))


; initialize constants stored to 0's (empty)
;(for ([i (range 21 24)]) (set! func-const-lru (first (lru-add func-const-lru (list 0 i)))))

; helper for simplifying parse tree modifcation on traversal, by incrememntally adding new fields
(define (add-code node code)
  (code-tree (typed-tree-rule node) (typed-tree-tokens node) (typed-tree-children node) (typed-tree-type node) code))

; creates inital location table
;(define (make-location-table)
;  (foldr
;    (lambda (fcn-name fcn-table) (hash-set fcn-table fcn-name (make-hash)))
;    (make-hash)
;    (hash-keys symbol-tables)))

;(define (allocate-reg)
;  (define allocating (first func-regs))
;  (set! func-regs (drop func-regs 1))
;  allocating)

; mem instr (sw/lw)
(define (mem op reg offset loc)
  (format "~a $~a, ~a($~a)" op reg offset loc))

(define (b-lbl op reg1 reg2 label)
  (format "~a $~a, $~a, ~a" op reg1 reg2 label))

(define (op3 op reg1 reg2 reg3)
  (format "~a $~a, $~a, $~a" op reg1 reg2 reg3))
(define (op2 op reg1 reg2)
  (format "~a $~a, $~a" op reg1 reg2))
(define (op1 op reg1)
  (format "~a $~a" op reg1))
(define (word val)
  (format ".word ~a" val))


; ensures the variable is in a register, no code if it already is
; output: Code
;(define (ensure-var var-name fcn-name)
;  (cond
;    ; variable already in register, do nothing
;    [(register? (hash-ref func-locations var-name)) ""]
;    ; variable not in register, means LRU is also full (so dont need LRU is not full case)
;    [else
;      ;(define lru-result (lru-add func-lru var-name))
;      (define reg (second lru-result))
;      (define pushed-var-name (first lru-result))
;      (define location-new (hash-ref func-stack var-name))
;      (define location-old (hash-ref func-stack pushed-var-name))
;      (define op-msg (format "; (ensure-var ~a ~a)~n; push ~a:~a out for ~a" var-name fcn-name pushed-var-name reg var-name))
;      (format "~a~n~a~n~a~n"
;        op-msg
;        (mem "sw" reg location-old FRAME-POINTER)
;        (mem "lw" reg location-new FRAME-POINTER))]))

;(define (backup-var reg)
;  (define var-lru (findf (lambda (elem) (= (second elem) reg)) (lru-elems func-lru)))
;  (cond
;    [var-lru
;      (define var-name (first var-lru))
;      (set! func-lru (lru (remove (list var-name reg) (lru-elems func-lru)) (lru-size func-lru)))
;      (hash-set! func-locations var-name (stack (hash-ref func-stack var-name)))
;      (format "~a~n~a"
;        (format "; backup var ~a, in ~a" var-name reg)
;        ; NOTE: no need to adjust stack-pointer, as this variable has already reserved stack space (fuck yea!)
;        (mem "sw" reg (- (hash-ref func-stack var-name)) FRAME-POINTER))]
;    [else (format "; backup: no var in ~a, so no backup required" reg)]))

;(define (recover-var var-name reg)
;  (set! func-lru (lru (cons (list var-name reg) (lru-elems func-lru)) (lru-size func-lru)))
;  (hash-set! func-locations var-name (register reg))
;  (format "~a~n~a"
;    (format "; recover var ~a, into ~a" var-name reg)
;    (mem "lw" reg (- (hash-ref func-stack var-name)) FRAME-POINTER)))
;

; puts this register to the top of the stackdcl
(define (backup-reg reg)
  (string-join (list
    (format "; backup reg ~a" reg)
    (mem "sw" reg -4 STACK-POINTER)
    (string-append (op3 "sub" STACK-POINTER STACK-POINTER FOUR-CONST) " ; decrement sp (added to it)")
  ) "\n"))

; assumes this register is on the top of the stack
(define (recover-reg reg)
  (string-join (list
    (format "; recover reg ~a" reg)
    (string-append (op3 "add" STACK-POINTER STACK-POINTER FOUR-CONST)  " ; increment sp (popped from it)")
    (mem "lw" reg -4 STACK-POINTER)
  ) "\n"))

; returns the register value that the variable is in. this will ALWAYS be used after "ensure-var" so it should be present. if not, errors - my bad
;(define (var-reg var-name fcn-name)
;  (define loc (hash-ref func-locations var-name))
;  (if (register? loc) (register-loc loc) (error 'FATAL "var-reg used when variable not in register. missing ensure-reg. func: ~a, var: ~a~n" fcn-name var-name)))

(define (adjust-stack-pointer words)
  (string-join (list
    "; set frame pointer"
    "; ADD 4, fixes an error when using offset 0 (bad memory??)"
    (op3 "sub" STACK-POINTER STACK-POINTER FOUR-CONST)
    (op3 "add" FRAME-POINTER ZERO-CONST STACK-POINTER)
    "; load frame size"
    (op1 "lis" TEMP-REG)
    (word (* words 4))
    "; adjust stack pointer size for frame"
    (op3 "sub" STACK-POINTER STACK-POINTER TEST-REG)
    ) "\n"))

(define (expr-backup lhs-rule rhs-rule lhs-code rhs-code)
  (string-join (list
    (format "; execute LHS (~a)" lhs-rule)
    (lhs-code LHS-REG)
    (backup-reg LHS-REG)
    (format "; excute RHS (~a)" rhs-rule)
    (rhs-code RHS-REG)
    (recover-reg LHS-REG)
    ) "\n"))

;(define (find-var stack-locs stack-pos)
;  (define (find-var-rec stack-loc-keys)
;    (define first-key (first stack-loc-keys))
;    (cond
;      [(empty? stack-loc-keys) empty]
;      [(equal? (hash-ref stack-locs first-key) stack-pos) first-key]
;      [else (find-var-rec (rest stack-loc-keys))]))
;  (find-var-rec (hash-keys stack-locs)))

(define (generate-code node context)
  (define func-name (context-func-new context))
  (define func-stack (if (equal? "error-no-func" func-name) empty (hash-ref func-stacks func-name)))
  (define func-paramcount (if (equal? "error-no-func" func-name) 0 (hash-ref func-params func-name)))

  ;(debug-print (format "matching: ~a => ~a~n" (typed-tree-rule node) (drop (typed-tree-tokens node) 1)))
  (match node
    ; no code generated for:
    ; terminals
    [(typed-tree (or
      "BOF" "EOF" "WAIN" "ID" "INT" "LPAREN"
      "COMMA" "RPAREN" "LBRACE" "RETURN" "SEMI" "AMP" "LBRACK" "RBRACK"
      "RBRACE" "PRINTLN" "NUM" "NULL" "PLUS" "MINUS" "PCT"
      "SLASH" "STAR" "BECOMES" "WHILE" "IF" "ELSE" "NEW" "DELETE"
      "LT" "LE" "EQ" "NE" "GE" "GT") _ _ _ _)
      (add-code node empty)]
    ; type → INT
    ; type → INT STAR
    [(typed-tree "type" _ _ _ _)
      (add-code node empty)]
    ; start → BOF procedures EOF
    ; puts main first as entry point
    [(typed-tree "start" _ (list _ procedures _) _ _)
      (define func-codes (code-tree-code procedures))
      (define code (string-join (cons
          (first (hash-ref func-codes "wain"))
          (map (lambda (func-name) (hash-ref func-codes func-name)) (remove "wain" (hash-keys func-codes)))
        ) "\n"))
      (add-code node code)]

    ; carry forward code for:
    ; expr → term
    [(typed-tree "expr" (list "expr" "term") (list term) _ _)
      (add-code node (code-tree-code term))]
    ; term → factor
    [(typed-tree "term" (list "term" "factor") (list factor) _ _)
      (add-code node (code-tree-code factor))]
    ; factor → LPAREN expr RPAREN
    [(typed-tree "factor" (list "factor" "LPAREN" "expr" "RPAREN") (list _ expr _) _ _)
      (add-code node (code-tree-code expr))]

      ; factor → STAR factor
      [(typed-tree "factor" (list "factor" "STAR" "factor") (list _ (code-tree "factor" _ _ _ factor-code)) _ _)
        (define code (lambda (destination-reg)
          (string-join (list
              (format "; evaluate into and load ~a" destination-reg)
              (factor-code destination-reg)
              (mem "lw" destination-reg 0 destination-reg)
            ) "\n")))
        (add-code node code)]

      ;lvalue → STAR factor
      [(typed-tree "lvalue" (list "lvalue" "STAR" "factor") (list _ factor) _ _)
        (define code (lambda (destintation-reg source-reg)
          (string-join (list
              (format "; lvalue -> STAR factor.")
              (backup-reg source-reg)
              ((code-tree-code factor) destintation-reg)
              (recover-reg source-reg)
              (mem "sw" source-reg 0 destintation-reg)
            ) "\n")))
        (add-code node code)]

      ; factor → AMP lvalue:ID
      ; store address of lvalue
      [(typed-tree "factor" (list "factor" "AMP" "lvalue") (list _ (code-tree "lvalue" (list "lvalue" "ID") _ _ var-name)) _ _)
        (define code (lambda (destination-reg)
          (define var-location (hash-ref func-stack var-name))
          (string-join (list
              (format "; factor -> lvalue:ID, pointer to ~a into reg $~a" var-name destination-reg)
              (op1 "lis" destination-reg)
              (word (* 4 var-location))
              (op3 "sub" destination-reg FRAME-POINTER destination-reg)
            ) "\n")))
        (add-code node code)]

      ; factor → AMP lvalue:STAR factor
      [(typed-tree "factor" (list "factor" "AMP" "lvalue") (list _ (code-tree "lvalue" (list "lvalue" "STAR" "factor") (list _ factor) _ lvalue-code)) _ _)
        (add-code node (code-tree-code factor))]

      ; dcls → dcls dcl BECOMES NULL SEMI
      [(typed-tree "dcls" (list "dcls" "dcls" "dcl" "BECOMES" "NULL" "SEMI")
        (list
          dcls
          (code-tree "dcl" _ (list _ (code-tree "ID" (list "ID" var-name) _ _ _)) _ _)
          _
          _
          _) _ _)
        (define var-location (hash-ref func-stack var-name))
        (define code
          (string-join (list
            (format "; declare ~a:stack = ~a" var-name null)
            (mem "sw" ONE-CONST (* 4 (- var-location)) FRAME-POINTER)
          ) "\n" ))
        (add-code node (string-append (code-tree-code dcls) "\n" code))]

      ; dcls → dcls dcl BECOMES NUM SEMI
      [(typed-tree "dcls" (list "dcls" "dcls" "dcl" "BECOMES" "NUM" "SEMI")
        (list
          dcls
          (code-tree "dcl" _ (list _ (code-tree "ID" (list "ID" var-name) _ _ _)) _ _)
          _
          (code-tree "NUM" (list "NUM" const-val) _ _ _)
          _) _ _)
        (define var-location (hash-ref func-stack var-name))
        (define code
          (string-join (list
              (format "; declare ~a:stack = ~a" var-name const-val)
              (op1 "lis" RESULT-REG)
              (word const-val)
              (mem "sw" RESULT-REG (* 4 (- var-location)) FRAME-POINTER)
            ) "\n" ))
        (add-code node (string-append (code-tree-code dcls) "\n" code))]

      ; statement → lvalue BECOMES expr SEMI
      ; assn to a variable
      [(typed-tree "statement" (list "statement" "lvalue" "BECOMES" "expr" "SEMI")
       (list (code-tree "lvalue" (list "lvalue" "ID") _ _ var-name) _ expr _) _ _)
        (define var-location (hash-ref func-stack var-name))
        (define code
          ;(cond
          ;[(register? var-location)
          ;  (string-join (list
          ;    (format "; assn ~a:$~a = expr" var-name (register-loc var-location))
          ;    ((code-tree-code expr) (register-loc var-location))
          ;  ) "\n" )]
          ;[(stack? var-location)
            (string-join (list
              (format "; assn ~a:stack = expr" var-name)
              ((code-tree-code expr) RESULT-REG)
              (mem "sw" RESULT-REG (* 4 (- var-location)) FRAME-POINTER)
            ) "\n" )
            ;])
            )
        (add-code node code)]

      ; statement → lvalue BECOMES expr SEMI
      ; assn to a pointer location
      [(typed-tree "statement" (list "statement" "lvalue" "BECOMES" "expr" "SEMI")
       (list (code-tree "lvalue" (list "lvalue" "STAR" "factor") _ _ lvalue-code) _ (code-tree "expr" _ _ _ expr-code) _) _ _)
        (define code
            (string-join (list
              (format "; assn to a mem location.")
              (expr-code RHS-REG) ; store the expr value to assn into RHS
              (lvalue-code LHS-REG RHS-REG) ; temporarily use the LHS reg for mem location, store RHS into that mem
            ) "\n" ))
        (add-code node code)]

      ; lvalue → ID
      [(typed-tree "lvalue" (list "lvalue" "ID") (list (code-tree "ID" (list "ID" var-name) _ _ _)) _ _)
        (add-code node var-name)]
      ; lvalue → LPAREN lvalue RPAREN
      ; pass down lvalue node, helps simplify rules so this is not a case, e.g. factor -> lvalue:(lvalue)
      [(typed-tree "lvalue" (list "lvalue" "LPAREN" "lvalue" "RPAREN") (list _ lvalue _) _ _)
        lvalue]

      ; test → expr LT expr
      [(typed-tree "test" (list "test" "expr" "LT" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int < term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int < term:int)"
            (op3 cmp TEST-REG LHS-REG RHS-REG)
          ) "\n"))
          (add-code node code)]

      ; test → expr EQ expr
      [(typed-tree "test" (list "test" "expr" "EQ" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int == term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int == term:int)"
            (op3 cmp TEST-REG LHS-REG RHS-REG)
            (op3 cmp LHS-REG RHS-REG LHS-REG)
            (op3 "add" TEST-REG LHS-REG TEST-REG)
            (op3 "sub" TEST-REG ONE-CONST TEST-REG)
          ) "\n"))
          (add-code node code)]
      ; test → expr NE expr
      [(typed-tree "test" (list "test" "expr" "NE" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int != term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int != term:int)"
            (op3 cmp TEST-REG LHS-REG RHS-REG)
            (op3 cmp LHS-REG RHS-REG LHS-REG)
            (op3 "add" TEST-REG LHS-REG TEST-REG)
          ) "\n"))
          (add-code node code)]
      ; test → expr LE expr
      [(typed-tree "test" (list "test" "expr" "LE" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int <= term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int <= term:int)"
            (op3 cmp TEST-REG RHS-REG LHS-REG)
            (op3 "sub" TEST-REG ONE-CONST TEST-REG)
          ) "\n"))
          (add-code node code)]
      ; test → expr GE expr
      [(typed-tree "test" (list "test" "expr" "GE" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int >= term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int >= term:int)"
            (op3 cmp TEST-REG LHS-REG RHS-REG)
            (op3 "sub" TEST-REG ONE-CONST TEST-REG)
          ) "\n"))
          (add-code node code)]
      ; test → expr GT expr
      [(typed-tree "test" (list "test" "expr" "GT" "expr") (list expr1 _ expr2) _ _)
        (define cmp (if (equal? (code-tree-type expr1) "int*") "sltu" "slt"))
        (define code (string-join (list
            "; test -> expr:int > term:int"
            (expr-backup "expr" "expr" (code-tree-code expr1) (code-tree-code expr2))
            "; execute (expr:int > term:int)"
            (op3 cmp TEST-REG RHS-REG LHS-REG)
          ) "\n"))
          (add-code node code)]

      ; statement → IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE
      [(typed-tree "statement" (list "statement" "IF" "LPAREN" "test" "RPAREN" "LBRACE" "statements" "RBRACE" "ELSE" "LBRACE" "statements" "RBRACE")
        (list _ _ test _ _ true-statements _ _ _ false-statements _) _ _)
        (define if-num if-count)
        (set! if-count (add1 if-count))
        (define code (string-join (list
            (format "; if #~a" if-num)
            (format "ifStart~a:" if-num)
            (code-tree-code test)
            (b-lbl "beq" 0 TEST-REG (format "ifElseBody~a" if-num))
            (format "; if True body #~a" if-num)
            (code-tree-code true-statements)
            (b-lbl "beq" 0 0 (format "ifEnd~a" if-num))
            (format "; else body #~a" if-num)
            (format "ifElseBody~a:" if-num)
            (code-tree-code false-statements)
            (format "; if end #~a" if-num)
            (format "ifEnd~a:" if-num)
          ) "\n"))
          (add-code node code)]

      ; statement → WHILE LPAREN test RPAREN LBRACE statements RBRACE
      [(typed-tree "statement" (list "statement" "WHILE" "LPAREN" "test" "RPAREN" "LBRACE" "statements" "RBRACE") (list _ _ test _ _ statements _) _ _)
        (define while-num while-count)
        (set! while-count (add1 while-count))
        (define code (string-join (list
            ; FIXME: hack to prevent issue where the stack/register locations vary from the testBody and statementsBody
            ; need to identify some way to make it consistent between test and body.
            ; primay issue is either over-allocation (too many variables, likely not a huge problem)
            ;  and also calling procedures (so moving registers 1-...)
            (format "; while loop #~a" while-num)
            (format "whileStart~a:" while-num)
            (code-tree-code test)
            (b-lbl "beq" 0 TEST-REG (format "whileEnd~a" while-num))
            (format "; while body #~a" while-num)
            (code-tree-code statements)
            (b-lbl "beq" 0 0 (format "whileStart~a" while-num))
            (format "; while loop end #~a" while-num)
            (format "whileEnd~a:" while-num)
          ) "\n"))
          (add-code node code)]

      ; expr → expr:int PLUS term:int
      [(typed-tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-int expr) _ (? is-int term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int + term:int"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int + term:int)"
            (op3 "add" destination-reg LHS-REG RHS-REG)
          ) "\n")))
        (add-code node code)]
      ; expr → expr:int MINUS term:int
      [(typed-tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-int expr) _ (? is-int term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int - term:int"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int - term:int)"
            (op3 "sub" destination-reg LHS-REG RHS-REG)
          ) "\n")))
        (add-code node code)]

      ; expr → expr:int* PLUS term:int
      [(typed-tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-intstar expr) _ (? is-int term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int* + term:int"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int* + term:int)"
            (op2 "mult" RHS-REG FOUR-CONST)
            (op1 "mflo" RHS-REG)
            (op3 "add" destination-reg LHS-REG RHS-REG)
          ) "\n")))
        (add-code node code)]
      ; expr → expr:int PLUS term:int*
      [(typed-tree "expr" (list "expr" "expr" "PLUS" "term") (list (? is-int expr) _ (? is-intstar term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int + term:int*"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int + term:int*)"
            (op2 "mult" LHS-REG FOUR-CONST)
            (op1 "mflo" LHS-REG)
            (op3 "add" destination-reg LHS-REG RHS-REG)
          ) "\n")))
        (add-code node code)]

      ; expr → expr:int* MINUS term:int*
      [(typed-tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-intstar expr) _ (? is-intstar term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int* - term:int*"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int - term:int)"
            (op3 "sub" destination-reg LHS-REG RHS-REG)
            (op2 "div" destination-reg FOUR-CONST)
            (op1 "mflo" destination-reg)
          ) "\n")))
        (add-code node code)]
      ; expr → expr:int* MINUS term:int
      [(typed-tree "expr" (list "expr" "expr" "MINUS" "term") (list (? is-intstar expr) _ (? is-int term)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; expr -> expr:int* - term:int"
            (expr-backup "expr" "term" (code-tree-code expr) (code-tree-code term))
            "; execute (expr:int - term:int)"
            (op2 "mult" RHS-REG FOUR-CONST)
            (op1 "mflo" RHS-REG)
            (op3 "sub" destination-reg LHS-REG RHS-REG)
          ) "\n")))
        (add-code node code)]

      ; term → term STAR term
      [(typed-tree "term" (list "term" "term" "STAR" "factor") (list (? is-int term) _ (? is-int factor)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; term -> term:int * factor:int"
            (expr-backup "term" "factor" (code-tree-code term) (code-tree-code factor))
            "; execute (term:int * factor:int)"
            (op2 "mult" LHS-REG RHS-REG)
            (op1 "mflo" destination-reg)
          ) "\n")))
        (add-code node code)]

      ; term → term SLASH factor
      [(typed-tree "term" (list "term" "term" "SLASH" "factor") (list (? is-int term) _ (? is-int factor)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; term -> term:int / factor:int"
            (expr-backup "term" "factor" (code-tree-code term) (code-tree-code factor))
            "; execute (term:int / factor:int)"
            (op2 "div" LHS-REG RHS-REG)
            (op1 "mflo" destination-reg)
          ) "\n")))
        (add-code node code)]

      ; term → term PCT factor
      [(typed-tree "term" (list "term" "term" "PCT" "factor") (list (? is-int term) _ (? is-int factor)) _ _)
        (define code (lambda (destination-reg) (string-join (list
            "; term -> term:int % factor:int"
            (expr-backup "term" "factor" (code-tree-code term) (code-tree-code factor))
            "; execute (expr:int % term:int)"
            (op2 "div" LHS-REG RHS-REG)
            (op1 "mfhi" destination-reg)
          ) "\n")))
        (add-code node code)]

    ; factor → NUM
    [(typed-tree "factor" (list "factor" "NUM") (list (code-tree "NUM" (list "NUM" const-val) _ _ _)) _ markers)
      (define code (lambda (destination-reg)
        ; 1. we just LIS into destination reg
        ; 2. we ensure-reg to move the constant to registers for future uses

        ; strategy 1. LIS into destination reg
        (define const-num (string->number const-val))
        (cond
          ; if const is already loaded, dont LIS
          [(member const-num '(1 4))
            (define CONST-LOCATIONS (make-hash (list (list 1 ONE-CONST) (list 4 FOUR-CONST))))
            (define source-reg (first (hash-ref CONST-LOCATIONS const-num)))
            (string-join (list
                (format "; using already stored const ~a:$~a" const-val source-reg)
                (op3 "add" destination-reg source-reg ZERO-CONST)
              ) "\n")]
          [else
            (string-join (list
                (format "; loading constant ~a into ~a" const-val destination-reg)
                (op1 "lis" destination-reg)
                (word const-val)
              ) "\n")])))
      (add-code node code)]

    ; factor → NULL
    [(typed-tree "factor" (list "factor" "NULL") _ _ _)
      (define code (lambda (destination-reg)
        (string-join (list
            (format "; loading NULL into ~a"destination-reg)
            (op3 "add" destination-reg ONE-CONST ZERO-CONST)
          ) "\n")))
      (add-code node code)]


    ; factor → ID
    [(typed-tree "factor" (list "factor" "ID") (list (code-tree "ID" (list "ID" var-name) _ _ _)) _ markers)
      (define code (lambda (destination-reg)

        ; two possible strategies for variables in the stack:
        ; 1. we just LW into destination reg
        ; 2. we ensure-reg to move the variable to registers

        ; strategy 1. LW into destination reg
        (define var-loc (hash-ref func-stack var-name))
        ;(cond
          ;[(stack? var-loc)
            (string-join (list
              "; factor -> ID"
              (format "; ~a is in stack: -~a offset" var-name var-loc)
              (mem "lw" destination-reg (* 4 (- var-loc)) FRAME-POINTER)
              ) "\n")
            ;]
          ;[else
          ;  (string-join (list
          ;    "; factor -> ID"
          ;    (format "; ~a is in register $~a" var-name (register-loc var-loc))
          ;    (op3 "add" destination-reg (register-loc var-loc) ZERO-CONST)
          ;    ) "\n")])
              ))
      (add-code node code)]

    ; statements → statements statement
    [(typed-tree "statements" (list "statements" "statements" "statement") (list statements statement) _ _)
      (add-code node (format "~a~n~a" (code-tree-code statements) (code-tree-code statement)))]

    ; factor → NEW INT LBRACK expr RBRACK
    [(typed-tree "factor" (list "factor" "NEW" "INT" "LBRACK" "expr" "RBRACK") (list _ _ _ expr _) _ _)
      ;(define var-in-1 (first (filter (lambda (var-name) (= 0 (hash-ref func-stack var-name))) (hash-keys func-stack))))
      (define code (lambda (destination-reg) (string-join (list
          "; NEW use"
          (backup-reg 1)
          ((code-tree-code expr) 1) ; lazy code generate expr with destination into $1
          (backup-reg 31)
          (op1 "lis" TEMP-REG)
          (word "new")
          (op1 "jalr" TEMP-REG)
          (recover-reg 31)
          (op3 "add" destination-reg 3 ZERO-CONST)
          "; check if new failed"
          (b-lbl "bne" ZERO-CONST destination-reg 1)
          (op3 "add" destination-reg ZERO-CONST ONE-CONST)
          (recover-reg 1)
        ) "\n")))
      (add-code node code)]

    ; statement → DELETE LBRACK RBRACK expr SEMI
    [(typed-tree "statement" (list "statement" "DELETE" "LBRACK" "RBRACK" "expr" "SEMI") (list _ _ _ expr _) _ _)
      ;(define var-in-1 (first (filter (lambda (var-name) (= 0 (hash-ref func-stack var-name))) (hash-keys func-stack))))
      (define code (string-join (list
          "; DELETE use"
          (backup-reg 1)
          ((code-tree-code expr) 1) ; lazy code generate expr with destination into $1
          (backup-reg 31)
          (op1 "lis" TEMP-REG)
          (word "delete")
          "; skip if value in $1 (addr deleting) is NULL"
          (b-lbl "beq" 1 ONE-CONST 1)
          (op1 "jalr" TEMP-REG)
          (recover-reg 31)
          (recover-reg 1)
        ) "\n"))
      (add-code node code)]

    ; statement → PRINTLN LPAREN expr RPAREN SEMI
    [(typed-tree "statement" (list "statement" "PRINTLN" "LPAREN" "expr" "RPAREN" "SEMI") (list _ _ expr _ _) _ _)
      ;(define var-in-1 (first (filter (lambda (var-name) (= 0 (hash-ref func-stack var-name))) (hash-keys func-stack))))
      (define code (string-join (list
          "; PRINTLN use"
          ;(backup-var 1)
          ((code-tree-code expr) 1) ; lazy code generate expr with destination into $1
          (backup-reg 31)
          (op1 "lis" TEMP-REG)
          (word "print")
          (op1 "jalr" TEMP-REG)
          (recover-reg 31)
          ;(recover-var var-in-1 1)
        ) "\n"))
      (add-code node code)]

    ; dcls →
    [(typed-tree "dcls" (list "dcls") _ _ _)
      (add-code node "")]
    ; statements →
    [(typed-tree "statements" (list "statements") _ _ _)
      (add-code node "")]
    ; dcl → type ID
    [(typed-tree "dcl" _ (list _ (code-tree "ID" (list "ID" var-name) _ _ _)) _ _)
      (define is-paramlist (member "paramlist" (map (lambda (x) (typed-tree-rule x)) context)))
      (define is-main (string=? "main"  (typed-tree-rule (first context))))
      (define code (cond
        [is-paramlist
          (define stack-position (hash-count func-stack))
          (hash-set! func-stack var-name stack-position)
          ; TODO: stack-pos is a function of how many paramlist parents exist
          ;(define stack-post-reg (cond [is-paramlist (hash-set! func-params func-name (add1 func-paramcount)) func-paramcount] [else (allocate-reg)]))
          ;(set! func-lru (lru (cons (list var-name using-reg) (lru-elems func-lru)) (lru-size func-lru)))
          ;(hash-set! func-locations var-name (register using-reg))
          ;(mem "sw" using-reg (- stack-position) FRAME-POINTER)
          ":c"
        ]
        [is-main
          (define stack-position (hash-count func-stack))
          (hash-set! func-stack var-name stack-position)
          (hash-set! func-params func-name  (add1 func-paramcount))
          (define using-reg func-paramcount)
          ;(hash-set! func-locations var-name (stack stack-position))
          (mem "sw" using-reg (- (* 4 stack-position)) FRAME-POINTER)
          ]
        ; maxed LRU size, variable now goes to stack
        [else
          (define stack-position (hash-count func-stack))
          (hash-set! func-stack var-name stack-position)
          ;(hash-set! func-locations var-name (stack stack-position))
          ""]))
      (add-code node code)]

    ; procedures → main
    ; code: hash-table of fcn -> fcn-body
    [(typed-tree "procedures" (list "procedures" "main") (list main-tree) _ _)
      (add-code node (make-immutable-hash (list (list "wain" (code-tree-code main-tree)))))]

    ; procedures → procedure procedures
    ; code: hash-table of fcn -> fcn-body
    [(typed-tree "procedures" (list "procedures" "procedure" "procedures")
      (list (code-tree "procedure"
        (list "procedure" "INT" "ID" "LPAREN" "params" "RPAREN" "LBRACE" "dcls" "statements" "RETURN" "expr" "SEMI" "RBRACE")
        (list _ (code-tree "ID" (list "ID" func-name) _ _ _) _ _ _ _ _ _ _ _ _ _) _ func-code) rest-tree) _ _)
      (define code (hash-set (code-tree-code rest-tree) func-name func-code))
      (add-code node code)]


    ; params →
    [(typed-tree "params" (list "params") _ _ _)
      (add-code node "")]

    ; factor → ID LPAREN RPAREN
    [(typed-tree "factor" (list "factor" "ID" "LPAREN" "RPAREN") (list (code-tree "ID" (list "ID" call-name) _ _ _) _ _) _ _)
      (define code (lambda (destintation-reg)
        (string-join (list
            (format "; function call to ~a, no params" call-name)
            (op1 "lis" TEMP-REG)
            (word (format "f~a" call-name))
            (backup-reg 29)
            (op1 "jalr" TEMP-REG)
            (recover-reg 29)
            (op3 "add" destintation-reg 3 ZERO-CONST)
          ) "\n")))
      (add-code node code)]

    ; procedure → INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE
    [(typed-tree "procedure"
      (list "procedure" "INT" "ID" "LPAREN" "params" "RPAREN" "LBRACE" "dcls" "statements" "RETURN" "expr" "SEMI" "RBRACE")
      (list _ (code-tree "ID" (list "ID" func-name) _ _ _) _ params _ _ dcls statements _ rtr-expr _ _) _ _)
      (define stack-size (length (hash-keys (hash-ref func-stacks func-name))))
      (define frame-pointer-code (if (zero? stack-size) "; no stack required \n" (adjust-stack-pointer stack-size)))
      (define declaration-code (code-tree-code dcls))
      (define statements-code (code-tree-code statements))
      (define return-code ((code-tree-code rtr-expr) 3))
      (define code (format
        (string-join
          (list
            ";;;;;;;;;"
            "; ~A"
            ";;;;;;;;;"
            "f~a:"
            "; SETUP FRAME AND STACK POINTER"
            "~a"
            (backup-reg 31)
            "; DECLARATION CODE"
            "~a"
            "; STATEMENTS CODE"
            "~a"
            "; RETURN EXPRESSION"
            "~a"
            (recover-reg 31)
            (op1 "jr" 31))
          "~n")
          func-name
          func-name
          frame-pointer-code
          declaration-code
          statements-code
          return-code))
      (add-code node code)]

    ; main → INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE
    [(typed-tree "main" _ (list _ _ _ dcl1 _ dcl2 _ _ dcls statements _ rtr-expr _ _) _ _)
      (define stack-size (length (hash-keys (hash-ref func-stacks "wain"))))
      (define heap-setup-code (cond
          ;[#t ""]
          [(equal? "int*" (code-tree-type dcl1))
            (string-join (list
                "; init with $2"
                (op1 "lis" RESULT-REG)
                (word "init")
                (backup-reg RETURN-POINTER)
                (op1 "jalr" RESULT-REG)
                (recover-reg RETURN-POINTER)
              ) "\n")]
          [else
            (string-join (list
                "; init with 0"
                (op1 "lis" RESULT-REG)
                (word "init")
                (backup-reg RETURN-POINTER)
                (backup-reg 2)
                (op3 "add" 2 ZERO-CONST ZERO-CONST)
                (op1 "jalr" RESULT-REG)
                (recover-reg 2)
                (recover-reg RETURN-POINTER)
              ) "\n")]))
      (define frame-pointer-code (if (zero? stack-size) "; no stack required \n" (adjust-stack-pointer stack-size)))
      (define declaration-code (code-tree-code dcls))
      (define parameters-code (string-append (code-tree-code dcl1) "\n" (code-tree-code dcl2)))
      (define statements-code (code-tree-code statements))
      (define return-code ((code-tree-code rtr-expr) 3))
      (define code (format
        (string-join
          (list
            "; MAIN"
            "fmain:"
            "; SETUP HEAP ALLOCATIONS"
            "~a"
            "; SETUP FRAME AND STACK POINTER"
            "~a"
            ;(backup-reg 31)
            "; PROCEDURE PARAMETERS"
            "~a"
            "; DECLARATION CODE"
            "~a"
            "; STATEMENTS CODE"
            "~a"
            "; RETURN EXPRESSION"
            "~a"
            ;(recover-reg 31)
            (op1 "jr" 31))
          "~n")
          heap-setup-code
          frame-pointer-code
          parameters-code
          declaration-code
          statements-code
          return-code))
      (add-code node code)]

    ; error catch case
    [_
      (eprintf "ERROR: no code generation case for rule ~a => ~a~n" (typed-tree-rule node) (string-join (drop (typed-tree-tokens node) 1) " "))
      (exit 1)]))

(define (generate-mips parse-tree)
  (define generated-code (code-tree-code (traverse-type parse-tree generate-code)))
  (define finalized-code (format "~a~n~a~n"
    (string-join (list
      "; PROGRAM START (part of generate-mips)"
      "; imports"
      ".import print"
      ".import new"
      ".import init"
      ".import delete"
      "; global convention constants"
      (op1 "lis" 26)
      (word 4)
      (op1 "lis" 27)
      (word 1)
      "; PARSE TREE CODE (traverse generate-code)"
      ) "\n")
    generated-code))
    finalized-code)

(define mips-code (generate-mips typed-parse-tree))

(printf "~a~n" mips-code)

;(print-symbol-table symbol-tables)
