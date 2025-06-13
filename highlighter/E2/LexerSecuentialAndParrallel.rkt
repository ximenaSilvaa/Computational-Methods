#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require web-server/servlet
         web-server/servlet-env)

;; LEXER DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Letter, capital, digit, underscore, character 
(define-lex-abbrev letter (:or (char-range #\a #\z) (char-range #\A #\Z)))
(define-lex-abbrev digit (char-range #\0 #\9)) 
(define-lex-abbrev underscore #\_)

; Identifier
(define-lex-abbrev ident-start (:or letter underscore))
(define-lex-abbrev ident-rest (:or letter digit underscore))
(define-lex-abbrev identifier (:seq ident-start (:* ident-rest)))

; Type
(define-lex-abbrev type (:or "int" "float" "double" "char" "long" "short" "unsigned" "signed" "void" "string"))

; Literals -> Integer-literal, float-literal, char-literal, string-literal, boolean-literal
(define-lex-abbrev int-literal (:+ digit))
(define-lex-abbrev float-literal (:seq (:* digit) #\. (:+ digit)))
(define-lex-abbrev char-literal (:seq #\' (:* (:or letter digit underscore #\space #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\+ #\= #\{ #\} #\[ #\] #\| #\: #\; #\< #\> #\, #\. #\? #\/)) #\'))
(define-lex-abbrev string-literal (:seq #\" (:* (:or letter digit underscore #\space #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\+ #\= #\{ #\} #\[ #\] #\| #\: #\; #\< #\> #\, #\. #\? #\/)) #\"))
(define-lex-abbrev bool-literal (:or "true" "false"))
(define-lex-abbrev literal (:or int-literal float-literal char-literal string-literal bool-literal))

; Operators
(define-lex-abbrev ref-op "&")
(define-lex-abbrev arithmetic-operator 
  (:or "+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">="))
(define-lex-abbrev logical-operator 
  (:or "&&" "||"))
(define-lex-abbrev comparison-operator 
  (:or "==" "!=" "<" ">" "<=" ">="))
(define-lex-abbrev assignment-operator 
  (:or "=" "+=" "-=" "*=" "/=" "%="))

; Preprocessor
(define-lex-abbrev hash "#")
(define-lex-abbrev include "include")
(define-lex-abbrev def "define")
(define-lex-abbrev ifdef "ifdef")
(define-lex-abbrev ifndef "ifndef")
(define-lex-abbrev endif "endif")
(define-lex-abbrev prepro (:seq (:or hash include def ifdef ifndef endif) (:? (:seq #\< identifier #\>))))

; Iostream
(define-lex-abbrev iostream-object 
  (:or "std::cout" "std::cin" "std::cerr" "std::clog"))
(define-lex-abbrev iostream-manipulator 
  (:or "std::endl" "std::flush"))
(define-lex-abbrev iostream-op (:or ">>" "<<"))

; Decision
(define-lex-abbrev decision
  (:or "if" "else"))

; Loop keywords
(define-lex-abbrev loop
  (:or "for" "while" "do"))

; Exception Handling
(define-lex-abbrev exception-handling
  (:or "try" "catch" "throw"))

; Return
(define-lex-abbrev return
  "return")

; Type and access
(define-lex-abbrev struct-class
  (:or "struct" "class"))
(define-lex-abbrev access-modifier
  (:or "public" "private" "protected"))

; Class extras
(define-lex-abbrev class-special
  (:or "override" "final"))

; Virtual
(define-lex-abbrev virtual-keyword "virtual")
(define-lex-abbrev virtual-destructor (:seq "virtual" (:+ #\space) "~" identifier (:+ #\space)))

; Function noexcept
(define-lex-abbrev function-special
  "noexcept")

; Namespace and using <using-directive>
(define-lex-abbrev using-directive
  (:seq "using" (:+ #\space) "namespace" (:+ #\space) identifier))

; Method keywords
(define-lex-abbrev method-keyword
  (:or "default" "delete"))

; <main-function>
(define-lex-abbrev main-function (:seq ("int" #\space "main()")))

; <variable-assignment> and declaration; <object-assignment>; <object-declaration>
(define-lex-abbrev var-dec (:seq type (:+ #\space) identifier))
(define-lex-abbrev var-assign (:seq var-dec (:+ #\space) assignment-operator (:+ #\space) literal))

; Statement
(define-lex-abbrev statement (:or var-assign var-dec))

; <function>
(define-lex-abbrev param 
  (:seq type (:+ #\space) identifier))
(define-lex-abbrev param-list 
  (:* (:seq param (:* #\space) (:? (:seq "," (:* #\space) param)))))
(define-lex-abbrev funct
  (:seq type (:+ #\space) identifier "(" (:+ #\space) ")"))

; <pre-compiler-system-include> <pre-compiler-local-include>
(define-lex-abbrev path (:+ (:or identifier #\/ #\.)))
(define-lex-abbrev pre-comp-sys-in (:seq hash include (:+ #\space) "<" path ">"))
(define-lex-abbrev pre-comp-local-inc (:seq hash include (:+ #\space) "\"" path "\""))

; <class-definition> <class-header> <class-definition-Inheritance>
(define-lex-abbrev class-definition (:seq "class" (:+ #\space) identifier))
(define-lex-abbrev class-definition-Inheritance (:seq "class" (:+ #\space) identifier (:+ #\space) ":" access-modifier (:+ #\space) identifier))

; GETTER AND SETTER
(define-lex-abbrev getter (:seq "get" identifier))
(define-lex-abbrev setter (:seq "set" identifier))

; Boolean expression
(define-lex-abbrev boolean-exp (:seq (:or literal identifier) (:+ #\space) comparison-operator (:+ #\space) (:or literal identifier))) 


;; LEXER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (basic-lexer input-port)
  (define lexer-instance
    (lexer
     ; VIRTUAL-DESTRUCTOR
     [virtual-destructor
      (cons `(VIRTUAL-DESTRUCTOR ,lexeme) (basic-lexer input-port))]
     ; VIRTUAL-KEYWORD
     [virtual-keyword
      (cons `(VIRTUAL-KEYWORD ,lexeme) (basic-lexer input-port))]
     
     ; BOOLEAN-EXPRESSION
     [boolean-exp
      (cons `(BOOLEAN-EXPRESSION ,lexeme) (basic-lexer input-port))]

     ; VAR ASSIGNMENT
     [statement
      (cons `(VARIABLE-ASSIGNMENT ,lexeme) (basic-lexer input-port))]

     ; GETTER AND SETTER
     [getter
      (cons `(GETTER ,lexeme) (basic-lexer input-port))]
     [setter
      (cons `(SETTER ,lexeme) (basic-lexer input-port))]

     ; CLASS DEFINITION and its inheritance
     [class-definition
      (cons `(CLASS-DEFINITION ,lexeme) (basic-lexer input-port))]
     [class-definition-Inheritance
      (cons `(CLASS-DEFINITION-INHERITANCE ,lexeme) (basic-lexer input-port))]

     ; <pre-compiler-system-include>
     [pre-comp-sys-in
      (cons `(PRE-COMPILER-SYS-INC ,lexeme) (basic-lexer input-port))]

     ; <pre-compiler-local-include>
     [pre-comp-local-inc
      (cons `(PRE-COMPILER-LOCAL-INC ,lexeme) (basic-lexer input-port))]

     ; FUNCTION
     [funct
      (cons `(FUNCTION ,lexeme) (basic-lexer input-port))]

     ; USING DIRECTIVE
     [using-directive
      (cons `(USING-DIRECTIVE ,lexeme) (basic-lexer input-port))]

     ; METHOD KEYWORD
     [method-keyword
      (cons `(METHOD-KEYWORD ,lexeme) (basic-lexer input-port))]

     ; FUNCTION NOEXCEPT
     [function-special
      (cons `(FUNCTION-NOEXCEPT ,lexeme) (basic-lexer input-port))]
     
     ; TYPE AND ACCESS, others
     [struct-class
      (cons `(STRUCT-CLASS ,lexeme) (basic-lexer input-port))]
     [access-modifier
      (cons `(ACCESS-MODIFIER ,lexeme) (basic-lexer input-port))]
     [class-special
      (cons `(CLASS-SPECIAL ,lexeme) (basic-lexer input-port))]
     
     ; RETURN KEYWORD
     [return
      (cons `(RETURN-KEYWORD ,lexeme) (basic-lexer input-port))]

     ; EXCEPTION HANDLING
     [exception-handling
      (cons `(EXCEPTION-HANDLING ,lexeme) (basic-lexer input-port))]

     ;; LOOP CONTROL
     [loop
      (cons `(LOOP ,lexeme) (basic-lexer input-port))]

     ;; FLOW CONTROL
     [decision
      (cons `(FLOW-CONTROL ,lexeme) (basic-lexer input-port))]
     
     ;; IOSTREAM
     [iostream-object
      (cons `(IOSTREAM-OBJECT ,lexeme) (basic-lexer input-port))]
     [iostream-manipulator 
      (cons `(IOSTREAM-MANIPULATOR ,lexeme) (basic-lexer input-port))]
     [iostream-op
      (cons `(IOSTREAM-OP ,lexeme) (basic-lexer input-port))]

     ;; PREPROCESSORS
     [prepro
      (cons `(PREPROCESSOR ,lexeme) (basic-lexer input-port))]
     
     ; TYPES
     [type
      (cons `(TYPE ,lexeme) (basic-lexer input-port))]

     ;; LITERAL
     [literal
      (cons `(LITERAL ,lexeme) (basic-lexer input-port))]
     
     ;; IDENTIFIERS
     [identifier
      (cons `(ID ,lexeme) (basic-lexer input-port))]

     ;; OPERATORS
     [ref-op 
      (cons `(REFERENCE-OP ,lexeme) (basic-lexer input-port))]
     [arithmetic-operator 
      (cons `(ARITHMETIC-OP ,lexeme) (basic-lexer input-port))]
     [logical-operator 
      (cons `(LOGICAL-OP ,lexeme) (basic-lexer input-port))]
     [comparison-operator
      (cons `(COMPARISON-OP ,lexeme) (basic-lexer input-port))]
     [assignment-operator 
      (cons `(ASSIGNMENT-OP ,lexeme) (basic-lexer input-port))]

     ;; WHITESPACE 
     [#\space
      (cons `(SPACE ,lexeme)(basic-lexer input-port))]
     [#\tab
      (cons `(TAB ,lexeme)(basic-lexer input-port))]
     [#\newline
      (cons `(NEWLINE ,lexeme)(basic-lexer input-port))]
     [#\return
      (cons `(RETURN ,lexeme)(basic-lexer input-port))]
    
     ; OTHER PUNCTUATION
     ["{"
      (cons `(LBRACE ,lexeme) (basic-lexer input-port))]
     ["}"
      (cons `(RBRACE ,lexeme) (basic-lexer input-port))]
     ["("
      (cons `(LPAREN ,lexeme) (basic-lexer input-port))]
     [")"
      (cons `(RPAREN ,lexeme) (basic-lexer input-port))]
     ["["
      (cons `(LBRACKET ,lexeme) (basic-lexer input-port))]
     ["]"
      (cons `(RBRACKET ,lexeme) (basic-lexer input-port))]
     [";"
      (cons `(SEMICOLON ,lexeme) (basic-lexer input-port))]
     [","
      (cons `(COMMA ,lexeme) (basic-lexer input-port))]
     ["."
      (cons `(DOT ,lexeme) (basic-lexer input-port))]
     [":"
      (cons `(COLON ,lexeme) (basic-lexer input-port))]

     [(eof)
      '()]
     ))
  (lexer-instance input-port))

(define (analyze-c++-code filename)
  (define in (open-input-file filename))
  (define tokens (basic-lexer in))
  (close-input-port in)
  tokens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 IN THIS SECTION OF THE CODE BOTH SEQUENTIAL AND PARALLEL ARE RUN
|#
(require racket/future)

; Function for sequential analysis
(define (sequential-analysis code-snippets)
  (map analyze-c++-code code-snippets))

; Function for parallel analysis. Here, "Futures" and "Touch" allow us to automate execution in several processors. Also, "code-snippets" is a list of codes to analyze.
(define (parallel-analysis code-snippets)
  (define futures (map (lambda (code) (future (lambda () (analyze-c++-code code)))) code-snippets))
  (map touch futures))

; Function to measure time
(define (main)
  (define code-snippets
    (list "source10.cpp"
          "source20.cpp"
          "source30.cpp"
          "source50.cpp"
          "source100.cpp"))
  
  ; Sequential execution - fixed to handle 4 return values from time-apply
  (define-values (seq-result seq-cpu-time seq-real-time seq-gc-time)
    (time-apply (lambda () (sequential-analysis code-snippets)) '()))
  (displayln "Result from sequential analysis:")
  (displayln (format "Sequential CPU time: ~a ms" seq-cpu-time))
  (displayln (format "Sequential real time: ~a ms" seq-real-time))
  
  ; Parallel execution - fixed to handle 4 return values from time-apply
  (define-values (par-result par-cpu-time par-real-time par-gc-time)
    (time-apply (lambda () (parallel-analysis code-snippets)) '()))
  (displayln "Result from parallel analysis:")
  (displayln (format "Parallel CPU time: ~a ms" par-cpu-time))
  (displayln (format "Parallel real time: ~a ms" par-real-time))
  
  ; Display performance comparison
  (displayln (format "Speedup: ~a" (/ seq-real-time par-real-time))))

; Run the main function
(main)