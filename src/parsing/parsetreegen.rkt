#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND LINE ARGUMENTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require net/http-client)
(require racket/cmdline)
;(require profile)

(define error-logging 3)
(define warning-logging 2)
(define verbose-logging 1)

(define pretty-out? (make-parameter #f))
(define active-log-level (make-parameter error-logging))

(command-line #:program "lr"

              ;#:once-any ; the following are mutually exclusive
              ;[("-v" "--warnings") "show warning logs" (active-log-level warning-logging)]
              ;[("-vv" "--verbose") "show verbose logs" (active-log-level verbose-logging)]

              #:once-each
              [("-p" "--pretty-out") "pretty output asm (ascii, not hex)"  (pretty-out? #t)]
              [("-w" "--warnings") "show warning logs" (active-log-level warning-logging)]
              [("-v" "--verbose") "show verbose logs" (active-log-level verbose-logging)])


(define-struct token (kind lexeme children) #:transparent)

(define (debug msg [log-level verbose-logging])
  ;(void))
  (if (>= log-level (active-log-level))
    (eprintf "~a" msg)
    (void)))


(define ip (open-input-bytes
  #"35\nAMP\nBECOMES\nBOF\nCOMMA\nDELETE\nELSE\nEOF\nEQ\nGE\nGT\nID\nIF\nINT\nLBRACE\nLBRACK\nLE\nLPAREN\nLT\nMINUS\nNE\nNEW\nNULL\nNUM\nPCT\nPLUS\nPRINTLN\nRBRACE\nRBRACK\nRETURN\nRPAREN\nSEMI\nSLASH\nSTAR\nWAIN\nWHILE\n17\nstart\ndcl\ndcls\nexpr\nfactor\nlvalue\nprocedure\nprocedures\nmain\nparams\nparamlist\nstatement\nstatements\nterm\ntest\ntype\narglist\nstart\n49\nstart BOF procedures EOF\nprocedures procedure procedures\nprocedures main\nprocedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE\nmain INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE\nparams\nparams paramlist\nparamlist dcl\nparamlist dcl COMMA paramlist\ntype INT\ntype INT STAR\ndcls\ndcls dcls dcl BECOMES NUM SEMI\ndcls dcls dcl BECOMES NULL SEMI\ndcl type ID\nstatements\nstatements statements statement\nstatement lvalue BECOMES expr SEMI\nstatement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE\nstatement WHILE LPAREN test RPAREN LBRACE statements RBRACE\nstatement PRINTLN LPAREN expr RPAREN SEMI\nstatement DELETE LBRACK RBRACK expr SEMI\ntest expr EQ expr\ntest expr NE expr\ntest expr LT expr\ntest expr LE expr\ntest expr GE expr\ntest expr GT expr\nexpr term\nexpr expr PLUS term\nexpr expr MINUS term\nterm factor\nterm term STAR factor\nterm term SLASH factor\nterm term PCT factor\nfactor ID\nfactor NUM\nfactor NULL\nfactor LPAREN expr RPAREN\nfactor AMP lvalue\nfactor STAR factor\nfactor NEW INT LBRACK expr RBRACK\nfactor ID LPAREN RPAREN\nfactor ID LPAREN arglist RPAREN\narglist expr\narglist expr COMMA arglist\nlvalue ID\nlvalue STAR factor\nlvalue LPAREN lvalue RPAREN\n132\n863\n72 RPAREN reduce 45\n112 paramlist shift 1\n85 LPAREN shift 2\n40 factor shift 3\n3 EQ reduce 47\n74 EQ reduce 46\n21 INT shift 4\n100 EQ reduce 48\n106 STAR shift 5\n86 STAR shift 5\n88 STAR shift 5\n95 SEMI shift 6\n51 SEMI shift 7\n118 NULL shift 8\n128 STAR reduce 11\n2 NULL shift 8\n108 GT shift 9\n57 RETURN reduce 16\n6 INT reduce 13\n7 INT reduce 12\n19 factor shift 10\n71 factor shift 10\n14 term shift 11\n44 term shift 11\n17 term shift 11\n25 term shift 11\n13 term shift 11\n9 term shift 11\n117 BECOMES reduce 14\n94 LPAREN reduce 11\n100 GT reduce 48\n3 GT reduce 47\n74 GT reduce 46\n85 expr shift 12\n108 GE shift 13\n74 GE reduce 46\n3 GE reduce 47\n100 GE reduce 48\n128 PRINTLN reduce 11\n108 EQ shift 14\n2 STAR shift 5\n118 STAR shift 5\n106 NEW shift 15\n86 NEW shift 15\n88 NEW shift 15\n123 RPAREN shift 16\n108 LT shift 17\n23 procedure shift 18\n70 term shift 11\n31 PLUS shift 19\n32 PLUS shift 19\n33 PLUS shift 19\n34 PLUS shift 19\n35 PLUS shift 19\n36 PLUS shift 19\n94 WHILE reduce 11\n29 COMMA shift 20\n94 dcls shift 21\n70 ID shift 22\n0 BOF shift 23\n5 NULL shift 8\n56 LPAREN shift 24\n108 LE shift 25\n101 COMMA shift 26\n121 SEMI shift 27\n83 DELETE reduce 18\n59 RETURN reduce 15\n57 WHILE reduce 16\n18 main shift 28\n82 DELETE reduce 19\n127 STAR reduce 39\n102 STAR reduce 40\n74 LE reduce 46\n30 STAR reduce 36\n8 STAR reduce 37\n112 dcl shift 29\n90 STAR reduce 43\n93 STAR reduce 42\n97 STAR reduce 38\n116 STAR reduce 41\n73 NUM shift 30\n14 expr shift 31\n44 expr shift 32\n17 expr shift 33\n25 expr shift 34\n13 expr shift 35\n9 expr shift 36\n37 EOF reduce 4\n87 RBRACE shift 37\n26 INT shift 4\n24 INT shift 4\n5 STAR shift 5\n59 STAR reduce 15\n130 DELETE reduce 17\n68 DELETE reduce 20\n131 DELETE reduce 21\n59 WHILE reduce 15\n11 LT reduce 28\n61 LT reduce 30\n60 LT reduce 29\n129 LPAREN shift 38\n121 PLUS shift 19\n46 NEW shift 15\n52 NEW shift 15\n66 NEW shift 15\n4 ID reduce 9\n38 LPAREN shift 38\n85 STAR shift 5\n18 procedures shift 39\n85 NUM shift 30\n40 NULL shift 8\n61 RPAREN reduce 30\n60 RPAREN reduce 29\n11 RPAREN reduce 28\n59 ID reduce 15\n59 IF reduce 15\n57 RBRACE reduce 16\n61 NE reduce 30\n60 NE reduce 29\n11 NE reduce 28\n77 IF reduce 15\n76 IF reduce 15\n115 STAR shift 40\n78 ID reduce 15\n78 IF reduce 15\n74 RBRACK reduce 46\n3 RBRACK reduce 47\n100 RBRACK reduce 48\n128 LPAREN reduce 11\n77 ID reduce 15\n76 ID reduce 15\n15 INT shift 41\n113 STAR shift 40\n114 STAR shift 40\n20 type shift 42\n22 RPAREN reduce 35\n24 type shift 42\n26 type shift 42\n129 WHILE shift 43\n108 NE shift 44\n11 LE reduce 28\n70 factor shift 10\n105 NEW shift 15\n104 NEW shift 15\n80 term shift 11\n80 expr shift 12\n113 LPAREN shift 38\n114 LPAREN shift 38\n117 RPAREN reduce 14\n115 LPAREN shift 38\n61 LE reduce 30\n60 LE reduce 29\n63 BECOMES shift 45\n90 PCT reduce 43\n116 PCT reduce 41\n30 PCT reduce 36\n8 PCT reduce 37\n127 PCT reduce 39\n102 PCT reduce 40\n93 PCT reduce 42\n97 PCT reduce 38\n47 MINUS reduce 32\n48 MINUS reduce 33\n49 MINUS reduce 34\n10 MINUS reduce 31\n10 STAR reduce 31\n11 STAR shift 46\n61 STAR shift 46\n60 STAR shift 46\n70 LPAREN shift 2\n46 factor shift 47\n52 factor shift 48\n66 factor shift 49\n14 STAR shift 5\n44 STAR shift 5\n17 STAR shift 5\n25 STAR shift 5\n13 STAR shift 5\n9 STAR shift 5\n80 arglist shift 50\n12 RPAREN reduce 44\n19 NEW shift 15\n71 NEW shift 15\n45 NUM shift 51\n61 GE reduce 30\n60 GE reduce 29\n11 GE reduce 28\n10 RBRACK reduce 31\n47 RBRACK reduce 32\n48 RBRACK reduce 33\n49 RBRACK reduce 34\n90 COMMA reduce 43\n116 COMMA reduce 41\n61 GT reduce 30\n60 GT reduce 29\n11 SLASH shift 52\n11 GT reduce 28\n61 SLASH shift 52\n60 SLASH shift 52\n30 COMMA reduce 36\n8 COMMA reduce 37\n127 COMMA reduce 39\n102 COMMA reduce 40\n47 STAR reduce 32\n48 STAR reduce 33\n49 STAR reduce 34\n80 ID shift 22\n93 COMMA reduce 42\n97 COMMA reduce 38\n70 NULL shift 8\n59 RBRACE reduce 15\n46 AMP shift 53\n52 AMP shift 53\n66 AMP shift 53\n23 main shift 28\n94 ID reduce 11\n78 RBRACE reduce 15\n94 IF reduce 11\n94 PRINTLN reduce 11\n23 INT shift 54\n5 ID shift 22\n46 STAR shift 5\n52 STAR shift 5\n66 STAR shift 5\n113 DELETE shift 55\n114 DELETE shift 55\n116 SLASH reduce 41\n129 STAR shift 40\n90 SLASH reduce 43\n112 RPAREN reduce 5\n54 WAIN shift 56\n127 SLASH reduce 39\n102 SLASH reduce 40\n93 SLASH reduce 42\n97 SLASH reduce 38\n30 SLASH reduce 36\n8 SLASH reduce 37\n19 STAR shift 5\n71 STAR shift 5\n118 NUM shift 30\n109 statement shift 57\n2 NUM shift 30\n73 LPAREN shift 2\n53 LPAREN shift 38\n47 COMMA reduce 32\n48 COMMA reduce 33\n49 COMMA reduce 34\n14 factor shift 10\n44 factor shift 10\n17 factor shift 10\n25 factor shift 10\n13 factor shift 10\n9 factor shift 10\n10 COMMA reduce 31\n100 STAR reduce 48\n3 STAR reduce 47\n74 STAR reduce 46\n115 DELETE shift 55\n109 PRINTLN shift 58\n78 WHILE reduce 15\n77 RBRACE reduce 15\n76 RBRACE reduce 15\n113 statement shift 57\n114 statement shift 57\n22 PLUS reduce 35\n115 statement shift 57\n105 AMP shift 53\n104 AMP shift 53\n77 WHILE reduce 15\n76 WHILE reduce 15\n128 dcls shift 59\n118 term shift 11\n2 term shift 11\n19 term shift 60\n71 term shift 61\n128 WHILE reduce 11\n19 LPAREN shift 2\n71 LPAREN shift 2\n128 RETURN reduce 11\n57 ID reduce 16\n57 IF reduce 16\n23 procedures shift 62\n2 LPAREN shift 2\n118 LPAREN shift 2\n21 dcl shift 63\n19 NULL shift 8\n71 NULL shift 8\n78 PRINTLN reduce 15\n77 PRINTLN reduce 15\n76 PRINTLN reduce 15\n2 expr shift 64\n118 expr shift 65\n11 PCT shift 66\n21 ID reduce 15\n21 IF reduce 15\n6 LPAREN reduce 13\n7 LPAREN reduce 12\n113 lvalue shift 67\n114 lvalue shift 67\n14 NULL shift 8\n44 NULL shift 8\n17 NULL shift 8\n25 NULL shift 8\n13 NULL shift 8\n9 NULL shift 8\n118 AMP shift 53\n115 lvalue shift 67\n2 AMP shift 53\n16 SEMI shift 68\n80 factor shift 10\n94 INT reduce 11\n5 NEW shift 15\n73 STAR shift 5\n57 PRINTLN reduce 16\n80 NEW shift 15\n74 LT reduce 46\n3 LT reduce 47\n46 NUM shift 30\n52 NUM shift 30\n66 NUM shift 30\n10 PCT reduce 31\n3 LE reduce 47\n100 LE reduce 48\n85 NEW shift 15\n21 WHILE reduce 15\n47 PCT reduce 32\n48 PCT reduce 33\n49 PCT reduce 34\n100 LT reduce 48\n21 RBRACE reduce 15\n22 COMMA reduce 35\n129 PRINTLN shift 58\n22 SEMI reduce 35\n100 NE reduce 48\n3 NE reduce 47\n74 NE reduce 46\n40 AMP shift 53\n14 ID shift 22\n44 ID shift 22\n17 ID shift 22\n25 ID shift 22\n13 ID shift 22\n9 ID shift 22\n94 RETURN reduce 11\n46 NULL shift 8\n52 NULL shift 8\n66 NULL shift 8\n19 AMP shift 53\n71 AMP shift 53\n118 ID shift 22\n2 ID shift 22\n54 ID shift 69\n80 NULL shift 8\n129 RETURN shift 70\n61 PLUS reduce 30\n60 PLUS reduce 29\n11 PLUS reduce 28\n59 DELETE reduce 15\n22 PCT reduce 35\n31 MINUS shift 71\n32 MINUS shift 71\n33 MINUS shift 71\n34 MINUS shift 71\n35 MINUS shift 71\n36 MINUS shift 71\n105 NUM shift 30\n104 NUM shift 30\n85 arglist shift 72\n22 RBRACK reduce 35\n6 DELETE reduce 13\n7 DELETE reduce 12\n109 RETURN shift 73\n88 LPAREN shift 2\n12 PLUS shift 19\n53 ID shift 74\n105 LPAREN shift 2\n104 LPAREN shift 2\n106 LPAREN shift 2\n86 LPAREN shift 2\n22 MINUS reduce 35\n90 BECOMES reduce 43\n93 BECOMES reduce 42\n97 BECOMES reduce 38\n127 BECOMES reduce 39\n102 BECOMES reduce 40\n30 BECOMES reduce 36\n8 BECOMES reduce 37\n70 NUM shift 30\n116 BECOMES reduce 41\n19 NUM shift 30\n71 NUM shift 30\n73 expr shift 75\n94 STAR reduce 11\n126 LBRACE shift 76\n125 LBRACE shift 77\n100 SLASH reduce 48\n3 SLASH reduce 47\n96 LBRACE shift 78\n74 SLASH reduce 46\n55 LBRACK shift 79\n40 STAR shift 5\n128 INT reduce 11\n2 NEW shift 15\n22 LPAREN shift 80\n127 NE reduce 39\n102 NE reduce 40\n93 NE reduce 42\n97 NE reduce 38\n90 NE reduce 43\n116 NE reduce 41\n116 MINUS reduce 41\n38 STAR shift 40\n78 RETURN reduce 15\n112 INT shift 4\n30 NE reduce 36\n8 NE reduce 37\n30 MINUS reduce 36\n8 MINUS reduce 37\n118 NEW shift 15\n127 MINUS reduce 39\n102 MINUS reduce 40\n93 MINUS reduce 42\n97 MINUS reduce 38\n90 MINUS reduce 43\n77 RETURN reduce 15\n76 RETURN reduce 15\n39 EOF reduce 1\n6 WHILE reduce 13\n7 WHILE reduce 12\n28 EOF reduce 2\n59 PRINTLN reduce 15\n113 RBRACE shift 81\n114 RBRACE shift 82\n127 RBRACK reduce 39\n102 RBRACK reduce 40\n93 RBRACK reduce 42\n97 RBRACK reduce 38\n109 lvalue shift 67\n90 RBRACK reduce 43\n116 RBRACK reduce 41\n115 RBRACE shift 83\n30 RBRACK reduce 36\n8 RBRACK reduce 37\n21 PRINTLN reduce 15\n14 LPAREN shift 2\n44 LPAREN shift 2\n17 LPAREN shift 2\n25 LPAREN shift 2\n13 LPAREN shift 2\n9 LPAREN shift 2\n112 params shift 84\n12 COMMA shift 85\n75 MINUS shift 71\n22 SLASH reduce 35\n103 RPAREN reduce 8\n5 NUM shift 30\n67 BECOMES shift 86\n11 RBRACK reduce 28\n61 COMMA reduce 30\n60 COMMA reduce 29\n61 RBRACK reduce 30\n60 RBRACK reduce 29\n11 COMMA reduce 28\n74 PCT reduce 46\n3 PCT reduce 47\n100 PCT reduce 48\n75 PLUS shift 19\n5 LPAREN shift 2\n112 type shift 42\n85 ID shift 22\n3 BECOMES reduce 47\n74 BECOMES reduce 46\n100 BECOMES reduce 48\n85 term shift 11\n116 SEMI reduce 41\n64 MINUS shift 71\n93 SEMI reduce 42\n97 SEMI reduce 38\n65 MINUS shift 71\n90 SEMI reduce 43\n80 LPAREN shift 2\n75 SEMI shift 87\n130 RBRACE reduce 17\n68 RBRACE reduce 20\n131 RBRACE reduce 21\n14 AMP shift 53\n44 AMP shift 53\n17 AMP shift 53\n25 AMP shift 53\n13 AMP shift 53\n9 AMP shift 53\n82 RBRACE reduce 19\n79 RBRACK shift 88\n127 SEMI reduce 39\n102 SEMI reduce 40\n30 SEMI reduce 36\n8 SEMI reduce 37\n83 RBRACE reduce 18\n68 PRINTLN reduce 20\n131 PRINTLN reduce 21\n130 PRINTLN reduce 17\n47 PLUS reduce 32\n48 PLUS reduce 33\n49 PLUS reduce 34\n82 PRINTLN reduce 19\n10 PLUS reduce 31\n109 WHILE shift 43\n83 PRINTLN reduce 18\n65 PLUS shift 19\n64 PLUS shift 19\n77 STAR reduce 15\n76 STAR reduce 15\n10 SEMI reduce 31\n78 STAR reduce 15\n73 NULL shift 8\n46 LPAREN shift 2\n52 LPAREN shift 2\n66 LPAREN shift 2\n4 STAR shift 89\n47 SEMI reduce 32\n48 SEMI reduce 33\n49 SEMI reduce 34\n116 RPAREN reduce 41\n40 NUM shift 30\n30 RPAREN reduce 36\n8 RPAREN reduce 37\n127 RPAREN reduce 39\n102 RPAREN reduce 40\n50 RPAREN shift 90\n93 RPAREN reduce 42\n97 RPAREN reduce 38\n105 NULL shift 8\n104 NULL shift 8\n90 RPAREN reduce 43\n109 DELETE shift 55\n18 procedure shift 18\n100 PLUS reduce 48\n74 PLUS reduce 46\n3 PLUS reduce 47\n85 factor shift 10\n10 NE reduce 31\n47 NE reduce 32\n48 NE reduce 33\n49 NE reduce 34\n94 DELETE reduce 11\n116 EQ reduce 41\n100 COMMA reduce 48\n30 GT reduce 36\n8 GT reduce 37\n3 COMMA reduce 47\n93 GT reduce 42\n97 GT reduce 38\n127 GT reduce 39\n102 GT reduce 40\n1 RPAREN reduce 6\n74 COMMA reduce 46\n27 RBRACE shift 91\n90 GE reduce 43\n93 GE reduce 42\n97 GE reduce 38\n116 GE reduce 41\n83 WHILE reduce 18\n30 GE reduce 36\n8 GE reduce 37\n127 GE reduce 39\n102 GE reduce 40\n21 LPAREN reduce 15\n6 STAR reduce 13\n7 STAR reduce 12\n82 WHILE reduce 19\n68 WHILE reduce 20\n131 WHILE reduce 21\n130 WHILE reduce 17\n61 PCT shift 66\n60 PCT shift 66\n80 NUM shift 30\n20 dcl shift 29\n99 RPAREN shift 92\n128 ID reduce 11\n57 LPAREN reduce 16\n80 RPAREN shift 93\n92 LBRACE shift 94\n14 NUM shift 30\n44 NUM shift 30\n17 NUM shift 30\n25 NUM shift 30\n13 NUM shift 30\n9 NUM shift 30\n90 GT reduce 43\n116 GT reduce 41\n45 NULL shift 95\n128 IF reduce 11\n81 ELSE shift 96\n57 DELETE reduce 16\n64 RPAREN shift 97\n73 AMP shift 53\n62 EOF shift 98\n21 STAR reduce 15\n118 factor shift 10\n105 factor shift 10\n104 factor shift 10\n2 factor shift 10\n29 RPAREN reduce 7\n100 RPAREN reduce 48\n105 ID shift 22\n104 ID shift 22\n82 LPAREN reduce 19\n20 INT shift 4\n88 factor shift 10\n130 LPAREN reduce 17\n106 factor shift 10\n86 factor shift 10\n68 LPAREN reduce 20\n131 LPAREN reduce 21\n26 dcl shift 99\n19 ID shift 22\n71 ID shift 22\n74 RPAREN reduce 46\n111 RPAREN shift 100\n3 RPAREN reduce 47\n83 LPAREN reduce 18\n24 dcl shift 101\n5 factor shift 102\n20 paramlist shift 103\n107 LPAREN shift 104\n43 LPAREN shift 105\n58 LPAREN shift 106\n12 MINUS shift 71\n11 MINUS reduce 28\n61 MINUS reduce 30\n60 MINUS reduce 29\n59 INT shift 4\n93 LT reduce 42\n97 LT reduce 38\n90 LT reduce 43\n30 LT reduce 36\n8 LT reduce 37\n127 LT reduce 39\n102 LT reduce 40\n91 INT reduce 3\n116 LT reduce 41\n93 LE reduce 42\n97 LE reduce 38\n108 PLUS shift 19\n127 LE reduce 39\n102 LE reduce 40\n30 LE reduce 36\n8 LE reduce 37\n129 ID shift 74\n116 LE reduce 41\n129 IF shift 107\n90 LE reduce 43\n46 ID shift 22\n52 ID shift 22\n66 ID shift 22\n70 NEW shift 15\n88 NULL shift 8\n106 NULL shift 8\n86 NULL shift 8\n10 EQ reduce 31\n47 EQ reduce 32\n48 EQ reduce 33\n49 EQ reduce 34\n80 STAR shift 5\n6 PRINTLN reduce 13\n7 PRINTLN reduce 12\n22 LT reduce 35\n59 LPAREN reduce 15\n59 type shift 42\n73 NEW shift 15\n22 LE reduce 35\n109 STAR shift 40\n105 expr shift 108\n104 expr shift 108\n93 PLUS reduce 42\n97 PLUS reduce 38\n90 PLUS reduce 43\n116 PLUS reduce 41\n30 PLUS reduce 36\n8 PLUS reduce 37\n127 PLUS reduce 39\n102 PLUS reduce 40\n10 GT reduce 31\n47 GT reduce 32\n48 GT reduce 33\n49 GT reduce 34\n109 IF shift 107\n109 ID shift 74\n80 AMP shift 53\n108 MINUS shift 71\n10 GE reduce 31\n47 GE reduce 32\n48 GE reduce 33\n49 GE reduce 34\n21 statements shift 109\n109 LPAREN shift 38\n59 dcl shift 63\n128 DELETE reduce 11\n40 ID shift 22\n84 RPAREN shift 110\n88 NUM shift 30\n106 NUM shift 30\n86 NUM shift 30\n40 LPAREN shift 2\n38 lvalue shift 111\n22 BECOMES reduce 35\n40 NEW shift 15\n69 LPAREN shift 112\n77 statements shift 113\n76 statements shift 114\n73 term shift 11\n78 statements shift 115\n53 STAR shift 40\n57 STAR reduce 16\n93 EQ reduce 42\n97 EQ reduce 38\n90 EQ reduce 43\n30 EQ reduce 36\n8 EQ reduce 37\n127 EQ reduce 39\n102 EQ reduce 40\n85 AMP shift 53\n47 LT reduce 32\n48 LT reduce 33\n49 LT reduce 34\n10 LT reduce 31\n117 COMMA reduce 14\n18 INT shift 54\n83 RETURN reduce 18\n10 LE reduce 31\n3 MINUS reduce 47\n129 statement shift 57\n74 MINUS reduce 46\n22 NE reduce 35\n10 RPAREN reduce 31\n100 MINUS reduce 48\n47 LE reduce 32\n48 LE reduce 33\n49 LE reduce 34\n106 term shift 11\n86 term shift 11\n130 RETURN reduce 17\n47 RPAREN reduce 32\n48 RPAREN reduce 33\n49 RPAREN reduce 34\n70 AMP shift 53\n88 term shift 11\n68 RETURN reduce 20\n131 RETURN reduce 21\n82 RETURN reduce 19\n105 term shift 11\n104 term shift 11\n82 STAR reduce 19\n22 STAR reduce 35\n83 STAR reduce 18\n77 LPAREN reduce 15\n76 LPAREN reduce 15\n65 RBRACK shift 116\n70 STAR shift 5\n68 STAR reduce 20\n131 STAR reduce 21\n78 LPAREN reduce 15\n130 STAR reduce 17\n129 DELETE shift 55\n121 MINUS shift 71\n78 DELETE reduce 15\n42 ID shift 117\n77 DELETE reduce 15\n76 DELETE reduce 15\n113 ID shift 74\n114 ID shift 74\n113 IF shift 107\n114 IF shift 107\n115 ID shift 74\n115 IF shift 107\n38 ID shift 74\n123 PLUS shift 19\n124 PLUS shift 19\n122 PLUS shift 19\n47 SLASH reduce 32\n48 SLASH reduce 33\n49 SLASH reduce 34\n10 SLASH reduce 31\n41 LBRACK shift 118\n14 NEW shift 15\n44 NEW shift 15\n17 NEW shift 15\n25 NEW shift 15\n13 NEW shift 15\n9 NEW shift 15\n105 STAR shift 5\n104 STAR shift 5\n105 test shift 119\n104 test shift 120\n5 AMP shift 53\n122 MINUS shift 71\n123 MINUS shift 71\n124 MINUS shift 71\n70 expr shift 121\n100 SEMI reduce 48\n73 ID shift 22\n3 SEMI reduce 47\n74 SEMI reduce 46\n113 WHILE shift 43\n114 WHILE shift 43\n88 ID shift 22\n106 ID shift 22\n86 ID shift 22\n22 GE reduce 35\n115 WHILE shift 43\n88 expr shift 122\n106 expr shift 123\n86 expr shift 124\n120 RPAREN shift 125\n119 RPAREN shift 126\n22 GT reduce 35\n129 lvalue shift 67\n11 SEMI reduce 28\n6 IF reduce 13\n7 IF reduce 12\n61 SEMI reduce 30\n60 SEMI reduce 29\n53 lvalue shift 127\n6 ID reduce 13\n7 ID reduce 12\n88 AMP shift 53\n89 ID reduce 10\n110 LBRACE shift 128\n106 AMP shift 53\n86 AMP shift 53\n83 IF reduce 18\n83 ID reduce 18\n82 IF reduce 19\n130 IF reduce 17\n82 ID reduce 19\n68 IF reduce 20\n131 IF reduce 21\n31 RPAREN reduce 22\n32 RPAREN reduce 23\n33 RPAREN reduce 24\n34 RPAREN reduce 25\n35 RPAREN reduce 26\n36 RPAREN reduce 27\n21 type shift 42\n6 RETURN reduce 13\n7 RETURN reduce 12\n59 statements shift 129\n68 ID reduce 20\n131 ID reduce 21\n130 ID reduce 17\n22 EQ reduce 35\n11 EQ reduce 28\n73 factor shift 10\n61 EQ reduce 30\n60 EQ reduce 29\n85 NULL shift 8\n115 PRINTLN shift 58\n124 SEMI shift 130\n122 SEMI shift 131\n21 RETURN reduce 15\n113 PRINTLN shift 58\n114 PRINTLN shift 58\n21 DELETE reduce 15\n"
  ))
;(define ip (open-input-file "wlp4.lr1"))

(define (readline)
  ;(read-line))
  (read-line ip))

(define ip2 (open-input-file "sumtokens.in"))


(define (readline-stdin)
    (read-line))
    ;(read-line ip2))



;;
;; Helper functions
;;

(define (readsyms n t)  ;; read n symbols into hash-table t
  (cond
    [(zero? n) empty]
    [else  (hash-set! t (readline) 0)
           (readsyms (sub1 n) t)]))

(define production-rules empty)
(define (readrules n t)  ;; read n symbols into hash-table t
  (cond
    [(zero? n) empty]
    [else
      (define rule (readline))
      (set! production-rules (cons rule production-rules))
      (hash-set! t rule 0)
      (readrules (sub1 n) t)]))

(define (readln)        ;; read single line containing integer
  (string->number (readline)))

(define (read-lr n t)
  (cond
    [(zero? n) empty]
    [else
      (define line (string-split (readline) " "))
      (define state-from (first line))
      (define input (second line))
      (define operation (third line))
      (define state-to (fourth line))

      (hash-set! t (format "~a ~a" state-from input) (list operation state-to))
      (read-lr (sub1 n) t)]))
;;
;; Main program
;;

(define T (make-hash))  ;; terminals
(define N (make-hash))  ;; nonterminals
(define R (make-hash))  ;; production rules
(define LR1 (make-hash))
(define S empty)
(define (read-grammar)

  (void (readsyms (readln) T))  ;; read terminals into hashtable T
  (void (readsyms (readln) N))  ;; read nonterminals into hashtable N
  (set! S (readline))        ;; start symbol
  (void (readrules (readln) R))  ;; read production rules (as strings)
  (void (set! production-rules
    (make-hash
      (map
        (lambda (x y) (list x y (string-split y " ")))
        (range (length production-rules))
        (reverse production-rules)))))

  (define lr-states (readln))
  (define lr-transistion-count (readln))

  (void (read-lr lr-transistion-count LR1))

)

;(collect-garbage)
;(collect-garbage)
;(collect-garbage)

;(profile-thunk (thunk (read-grammar)) #:repeat 1)
(read-grammar)

(define (lr-operation token state)
  (define result (hash-ref LR1 (format "~a ~a" state token) empty))
  (debug (format "result: ~a~n" result))
  (cond
    [(empty? result)
      ;(debug (format "~a~n" "error"))
      'error-operation-no-match]
    [(equal? (first result) "shift")
      ;(debug (format "shift ~a~n" (second result)))
      (list 'shift (second result))]
    [(equal? (first result) "reduce")
      ;(debug (format "reduce ~a~n" (nth (string->number (second result)) production-rules)))
      (list 'reduce (string->number (second result)))]))

(define (first-if x)
  (if (list? x) (first x) x))

(define (lr-parse initial-tokens)
  (define (prettify stack)
    (cond
      [(empty? stack) ""]
      [(list? (first stack))
        (prettify (map (lambda (x) (first x)) stack))]
      [else
        (define tokenz (first stack))
        (string-append
          (if
            (empty? (token-children tokenz))
            (string-append (token-kind tokenz) " " (first-if (token-lexeme tokenz)))
            (string-append (first-if (first (token-lexeme tokenz)))))
          "~n"
          (prettify (token-children (first stack)))
          (prettify (rest stack)))]))

  (define (reduce-stack stack reductions keep)
    (cond
      [(empty? reductions) (list stack keep)]
      [(empty? stack) 'erorr-reduce-no-stack]
      [(equal? (token-kind (first (first stack))) (first reductions))
        (reduce-stack (rest stack) (rest reductions) (cons (first (first stack)) keep))]
      [else
      (debug (format "stack:~n~a~nreductions:~n~a~n" stack reductions))
      'error-stack-no-match]))

  (define (lr-parse-recurse state reductions stack tokens processed)
    (cond
      ; TODO: check that the result stack is the initial state
      [(empty? tokens)
        (string-append "start BOF procedures EOF~n" (prettify (reverse stack)))]
      [else
        (define result (lr-operation (token-kind (first tokens)) state))
        (debug (format "= input, state:~a token:~a~n" state (token-kind (first tokens))))
        (debug (format "- stack: ~a~n" stack))
        (cond
          [(and (symbol? result) (symbol=? 'error-operation-no-match result))
            (format "ERROR at ~a~n" (- (length initial-tokens) (length tokens) -1))]
          [(symbol=? (first result) 'shift)

            (debug (format "shift:  ~a from ~a to ~a~n" (token-kind (first tokens)) state (second result)))

            (define next-state (second result))
            (define next-stack (cons (list (first tokens) next-state) stack))
            (define next-tokens (rest tokens))
            (lr-parse-recurse next-state reductions next-stack next-tokens (sub1 processed))
          ]
          [(symbol=? (first result) 'reduce)
            (define next-reductions (cons (second result) reductions))
            (define reduction-rule (hash-ref production-rules (second result)))
            (define reduce-result (reduce-stack stack (reverse (drop (second reduction-rule) 1)) empty))
            (define next-stack (first reduce-result))
            (define next-tokens (cons (token (first (second reduction-rule)) reduction-rule (second reduce-result)) tokens))
            (define next-state (second (first next-stack)))

            (debug (format "+ reduce on ~a with ~a into ~a ~n" stack (first reduction-rule) next-stack))
            (debug (format "reduce: ~a from ~a to ~a~n" (token-kind (first tokens)) state next-state))

            (lr-parse-recurse next-state next-reductions next-stack next-tokens processed)
          ])
        ]))

  (lr-parse-recurse 0 empty empty initial-tokens 0))

(define (evaluate tokens)
  (define result (lr-parse tokens))
  (if (not (list? result))
    (eprintf result)
    (printf (string-append (string-join result "~n") "~n"))))

(define (read-tokens tokens)
  (define line (readline-stdin))
  (cond
    [(eof-object? line) (reverse tokens)]
    [else
      (define split-line (string-split line " "))
      (read-tokens (cons (token (first split-line) (second split-line) empty) tokens))]))

(define tokens (append (list (token "BOF" "BOF" empty)) (read-tokens empty) (list (token "EOF" "EOF" empty))))
(void (evaluate tokens))
