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

(define (run-lexer file-path)
  (define in (open-input-file file-path))
  (define result (basic-lexer in))
  (close-input-port in)
  result)


;; WEB INTERFACE FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; read file content
(define (read-file-content file-path)
  (call-with-input-file file-path
    (lambda (in)
      (port->string in))))

(define (generate-token-css)
  `(style ((type "text/css"))
          "
    body { 
      font-family: 'Fira Code', 'Courier New', monospace; 
      background-color: #1e1e1e; 
      color: #d4d4d4; 
      margin: 20px;
      line-height: 1.6;
    }
    h1, h2 { 
      color: #569cd6; 
      text-align: center; 
    }
    
    .token-section {
      background-color: #252526; 
      border-radius: 8px; 
      padding: 20px;
      border: 1px solid #3e3e42;
      overflow-x: auto;
    }
    
    .token-section h3 {
      color: #4ec9b0;
      margin-top: 0;
      border-bottom: 1px solid #4ec9b0;
      padding-bottom: 10px;
    }
    
    .token-container { 
      background-color: #1e1e1e;
      border: 1px solid #404040;
      border-radius: 4px;
      padding: 15px;
      max-height: 400px;
      overflow-y: auto;
    }
    
    .token { 
      display: inline-block; 
      margin: 2px 4px; 
      padding: 2px 6px; 
      border-radius: 4px; 
      font-weight: bold; 
      font-size: 12px;
    }
    
    .code-reconstruction {
      background-color: #252526; 
      border-radius: 8px; 
      padding: 20px;
      border: 1px solid #3e3e42;
      margin-top: 20px;
    }
    
    .code-reconstruction h3 {
      color: #4ec9b0;
      margin-top: 0;
      border-bottom: 1px solid #4ec9b0;
      padding-bottom: 10px;
    }
    
    .reconstructed-code {
      background-color: #1e1e1e;
      border: 1px solid #404040;
      border-radius: 4px;
      padding: 15px;
      max-height: 500px;
      overflow: auto;
      font-family: 'Fira Code', 'Courier New', monospace;
      font-size: 14px;
      line-height: 1.4;
      white-space: pre-wrap;
      color: #d4d4d4;
    }
    
    .reconstructed-code .token {
      display: inline;
      margin: 0;
      padding: 0;
      border-radius: 0;
      font-weight: normal;
      font-size: inherit;
    }
    
    .TYPE { background-color: #264f78; color: #4ec9b0; }
    .LITERAL { background-color: #1a5b23; color: #b5cea8; }
    .ID { background-color: #3b3b3b; color: #9cdcfe; }
    .ARITHMETIC-OP { background-color: #5a2d2d; color: #ff6b6b; }
    .LOGICAL-OP { background-color: #4a4a2d; color: #ffd93d; }
    .COMPARISON-OP { background-color: #2d4a4a; color: #6bcf7f; }
    .ASSIGNMENT-OP { background-color: #4a2d4a; color: #c586c0; }
    .FLOW-CONTROL { background-color: #2d3f4a; color: #569cd6; }
    .LOOP { background-color: #4a3f2d; color: #ce9178; }
    .RETURN-KEYWORD { background-color: #3f2d4a; color: #d197d9; }
    .IOSTREAM-OBJECT { background-color: #2d4a3f; color: #4fc1ff; }
    .IOSTREAM-OP { background-color: #4a4a2d; color: #ffcc02; }
    .IOSTREAM-MANIPULATOR { background-color: #2d4a4a; color: #4fc1ff; }
    .PREPROCESSOR { background-color: #4a2d2d; color: #ff8c94; }
    .STRUCT-CLASS { background-color: #2d2d4a; color: #86b3ff; }
    .ACCESS-MODIFIER { background-color: #4a2d3f; color: #ff86b3; }
    .CLASS-SPECIAL { background-color: #4a2d3f; color: #ff86b3; }
    .FUNCTION-NOEXCEPT { background-color: #4a3f2d; color: #ce9178; }
    .EXCEPTION-HANDLING { background-color: #3f4a2d; color: #b3ff86; }

    /* Unique colors for each token type */
    .COLON { background-color: #2d4a2d; color: #90ee90; }
    .DOT { background-color: #4a2d4a; color: #da70d6; }
    .COMMA { background-color: #2d2d4a; color: #87ceeb; }
    .SEMICOLON { background-color: #4a4a2d; color: #ffd700; }
    .RBRACKET { background-color: #4a2d2d; color: #ffa07a; }
    .LBRACKET { background-color: #2d4a4a; color: #40e0d0; }
    .RPAREN { background-color: #3d2d4a; color: #dda0dd; }
    .LPAREN { background-color: #4a3d2d; color: #f0e68c; }
    .RBRACE { background-color: #2d3d4a; color: #add8e6; }
    .LBRACE { background-color: #4a2d3d; color: #f08080; }

    .NEWLINE { color: #16537e; }
    .TAB {  color: #e94560; }
    .SPACE { color: #f39c12; }

    .REFERENCE-OP { background-color: #2d4a3d; color: #98fb98; }
    .METHOD-KEYWORD { background-color: #3d4a2d; color: #32cd32; }
    .USING-DIRECTIVE { background-color: #4a2d4a; color: #ff1493; }
    .FUNCTION { background-color: #2d4a2d; color: #7fff00; }
    .PRE-COMPILER-LOCAL-INC { background-color: #4a4a3d; color: #ffff00; }
    .PRE-COMPILER-SYS-INC { background-color: #3d4a4a; color: #00ffff; }
    .CLASS-DEFINITION-INHERITANCE { background-color: #4a3d2d; color: #deb887; }
    .CLASS-DEFINITION { background-color: #3d2d4a; color: #ba55d3; }
    .GETTER { background-color: #2d4a4a; color: #20b2aa; }
    .SETTER { background-color: #4a2d4a; color: #ff6347; }
    .VARIABLE-ASSIGNMENT { background-color: #2d3d2d; color: #9acd32; }
    .BOOLEAN-EXPRESSION { background-color: #3d2d3d; color: #ff4500; }
    .VIRTUAL-KEYWORD { background-color: #4a4a4a; color: #dc143c; }
    .VIRTUAL-DESTRUCTOR { background-color: #2d2d2d; color: #00ced1; }
    
    .file-section {
      margin: 30px 0;
      border: 2px solid #569cd6;
      border-radius: 10px;
      padding: 20px;
    }
    
    .file-title {
      color: #4ec9b0;
      border-bottom: 2px solid #4ec9b0;
      padding-bottom: 10px;
      margin-bottom: 20px;
    }
    
    .token-stats {
      background-color: #2d2d30;
      padding: 15px;
      border-radius: 6px;
      margin: 15px 0;
      font-size: 14px;
    }
    
    .stats-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 10px;
      margin-top: 10px;
    }
    
    .legend {
      margin-top: 40px;
      text-align: center;
      color: #608b4e;
    }
    
    .legend-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
      gap: 10px;
      margin-top: 15px;
    }
    
    .toggle-button {
      background-color: #569cd6;
      color: white;
      border: none;
      padding: 8px 16px;
      border-radius: 4px;
      cursor: pointer;
      margin-bottom: 10px;
      font-family: inherit;
    }
    
    .toggle-button:hover {
      background-color: #4ec9b0;
    }
    
    .collapsible {
      overflow: hidden;
      transition: max-height 0.3s ease;
    }
    
    .collapsed {
      max-height: 0;
    }
    
    .expanded {
      max-height: 1000px;
    }
    
    @media (max-width: 768px) {
      .token-section, .code-reconstruction {
        padding: 15px;
      }
    }
    "))

; reconstruct original code from tokens
(define (reconstruct-code tokens)
  (define (token-value token)
    (cond
      [(list? token)
       (cond
         [(>= (length token) 2) 
          (let ([val (second token)])
            (cond
              [(string? val) val]
              [(symbol? val) (symbol->string val)]
              [(number? val) (number->string val)]
              [(char? val) (string val)]
              [else (format "~a" val)]))]
         [else ""])]
      [else (format "~a" token)]))
  
  (define (process-token-value value)
    (cond
      [(equal? value "\\n") "\n"]
      [(equal? value "\\t") "\t"]
      [(equal? value "\\r") "\r"]
      [(equal? value "\n") "\n"]
      [(equal? value "\t") "\t"]  
      [(equal? value "\r") "\r"]
      [else value]))
  
  (apply string-append 
         (map (lambda (token)
                (let ([val (token-value token)])
                  (if (string? val)
                      (process-token-value val)
                      (format "~a" val))))
              tokens)))

; convert a token to HTML with colors
(define (token-to-html token)
  (cond
    [(list? token)
     (let ([token-type (symbol->string (first token))]
           [token-value (if (> (length token) 1) (second token) "")])
       `(span ((class ,(string-append "token " token-type)))
              ,(format "~a" token)))]
    [else
     `(span ((class "token UNKNOWN"))
            ,(format "~a" token))]))

; count token types
(define (count-token-types tokens)
  (define counts (make-hash))
  (for ([token tokens])
    (when (list? token)
      (let ([token-type (first token)])
        (hash-set! counts token-type
                   (+ 1 (hash-ref counts token-type 0))))))
  counts)

; token statistics
(define (generate-token-stats tokens)
  (define counts (count-token-types tokens))
  (define total-tokens (length tokens))
  `(div ((class "token-stats"))
        (h3 "Token Statistics")
        (p ,(format "Total tokens: ~a" total-tokens))
        (div ((class "stats-grid"))
             ,@(for/list ([(type count) (in-hash counts)])
                 `(div ,(format "~a: ~a" type count))))))

;token to HTML
(define (token-to-reconstructed-html token)
  (cond
    [(list? token)
     (let ([token-type (symbol->string (first token))]
           [token-value (if (> (length token) 1) 
                           (let ([val (second token)])
                             (cond
                               [(string? val) val]
                               [(symbol? val) (symbol->string val)]
                               [(number? val) (number->string val)]
                               [(char? val) (string val)]
                               [else (format "~a" val)]))
                           "")])
       (let ([processed-value (cond
                                [(equal? token-value "\\n") "\n"]
                                [(equal? token-value "\\t") "\t"]
                                [(equal? token-value "\\r") "\r"]
                                [(equal? token-value "\n") "\n"]
                                [(equal? token-value "\t") "\t"]
                                [(equal? token-value "\r") "\r"]
                                [else token-value])])
         `(span ((class ,(string-append "token " token-type)))
                ,processed-value)))]
    [else
     `(span ((class "token UNKNOWN"))
            ,(format "~a" token))]))

; highlighted version of the code put togetter
(define (generate-code-reconstruction tokens file-name)
  `(div ((class "code-reconstruction"))
        (h3 "Highlighted Code")
        (p ,(format "Original code reconstructed from tokens of ~a:" file-name))
        (button ((class "toggle-button") 
                 (onclick ,(format "toggleCodeVisibility('~a')" file-name)))
                "Show/Hide Code")
        (div ((class "collapsible collapsed") (id ,(format "code-~a" file-name)))
             (div ((class "reconstructed-code"))
                  ,@(map token-to-reconstructed-html tokens)))))

; Tokens analysis
(define (generate-file-analysis file-path file-name)
  (define tokens (run-lexer file-path))

  `(div ((class "file-section"))
        (h2 ((class "file-title")) ,(format "File: ~a" file-name))
        ,(generate-token-stats tokens)
        ,(generate-code-reconstruction tokens file-name)
        (div ((class "token-section"))
             (h3 "Analyzed Tokens")
             (button ((class "toggle-button") 
                      (onclick ,(format "toggleTokenVisibility('~a')" file-name)))
                     "Show/Hide Tokens")
             (div ((class "collapsible collapsed") (id ,(format "tokens-~a" file-name)))
                  (div ((class "token-container"))
                       ,@(map token-to-html tokens))))))

; JavaScript
(define (generate-javascript)
  `(script
    "
    function toggleCodeVisibility(fileName) {
      const element = document.getElementById('code-' + fileName);
      if (element.classList.contains('collapsed')) {
        element.classList.remove('collapsed');
        element.classList.add('expanded');
      } else {
        element.classList.remove('expanded');
        element.classList.add('collapsed');
      }
    }
    
    function toggleTokenVisibility(fileName) {
      const element = document.getElementById('tokens-' + fileName);
      if (element.classList.contains('collapsed')) {
        element.classList.remove('collapsed');
        element.classList.add('expanded');
      } else {
        element.classList.remove('expanded');
        element.classList.add('collapsed');
      }
    }
    "))


(define (start req)
  ; HI SERGIO, MAKE SURE THE SOURCE PATH OF THE CPP FILES IS CORRECT FOR YOUR COMPUTER
  (define file-paths '("C:/Users/PC/Documents/Tec/4to semestre/ComputationalMethods/webFinal/source10.cpp"
                       "C:/Users/PC/Documents/Tec/4to semestre/ComputationalMethods/webFinal/source20.cpp"
                       "C:/Users/PC/Documents/Tec/4to semestre/ComputationalMethods/webFinal/source30.cpp"
                       "C:/Users/PC/Documents/Tec/4to semestre/ComputationalMethods/webFinal/source50.cpp"
                       "C:/Users/PC/Documents/Tec/4to semestre/ComputationalMethods/webFinal/source100.cpp"))
  (define file-names '("source10.cpp" "source20.cpp" "source30.cpp" "source50.cpp" "source100.cpp"))

  (response/xexpr
   `(html
     (head
      (title "Lexer TC2037 - Syntax highlighter")
      (meta ((charset "utf-8")))
      (link ((rel "preconnect") (href "https://fonts.googleapis.com")))
      (link ((rel "preconnect") (href "https://fonts.gstatic.com") (crossorigin "")))
      (link ((href "https://fonts.googleapis.com/css2?family=Fira+Code:wght@300;400;500;600&display=swap") (rel "stylesheet")))
      ,(generate-token-css)
      ,(generate-javascript))
     (body
      (h1 "Syntax highlighter")
      (p ((style "text-align: center; font-size: 18px; color: #ce9178;"))
         "Ximena Silva Bárcena A01785518 | Arturo Utrilla Hernández A01174331 | Daniel Esteban Hernández García A01652966 | Rodrigo Martínez Vallejo A00573055")

      ,@(map generate-file-analysis file-paths file-names)

      ))))

; Iniciar el servidor
(serve/servlet start
               #:port 8022
               #:servlet-path "/"
               #:extra-files-paths (list (current-directory))
               #:launch-browser? #t)