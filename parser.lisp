(in-package :c-compiler)

(defun quote-nonalpha (token)
  (coerce (loop for c across token
	     appending (if (alphanumericp c)
			   `(,c)
			   `(#\\ ,c))) 'string))

;; Tokeny podzielone na 3 kategorie - 
(defconstant +tokens+ '((char double do else float for if int long return 
			 short sizeof void while)
			(<= >= == != \; { } \, = \( \) [ ] ! ~ -- ++ - + * /
			 % < > ^ \|\| \&\& \| \&)
			(identifier constant string)))

(defmacro create-c-lexer (name)
  `(define-string-lexer ,name

					; komentarze
     ("/\\*(\\*[^/]|[^\\*])*\\*/")

					; slowa kluczowe i operatory
     ,@(loop for op in (append (car +tokens+) (cadr +tokens+))
	    collecting `(,(quote-nonalpha (string-downcase (string op))) 
			  (return (values ',op ',op))))
     ("[A-Za-z_]\\w*" (return (values 'identifier 
				      (intern (regex-replace-all "_" $@ "-")))))
					; literaly liczbowe i znakowe
     ,@(loop for pattern in '("\\d+[uUlL]?" "0[0-7]+[uUlL]?" "0x|X[0-9A-Fa-f]+[uUlL]?"
			      "\\d+\\.\\d*([eE][+-]?\\d+)?[fFlL]?"
			      "\\d*\\.\\d+([eE][+-]?\\d+)?[fFlF]?" 
			      "\\d+([eE][+-]?\\d+)?[fFlF]?"
			      "L?'(\\.|[^\\'])+'")
	  collecting `(,pattern (return (values 'constant 
						(read-from-string $@)))))
     ("L?\"(\\.|[^\\\"])*\"" (return (values 'string (intern $@))))
					; biale znaki
     ("\\s")))

(defun c-stream-lexer (stream lexer-fun)
  (labels ((reload-closure (stream) 
	     (let ((line (read-line stream nil)))
	       (if (null line)
		   nil
		   (funcall lexer-fun line)))))
    (let ((lexer-closure (reload-closure stream)))
      (labels ((get-nonempty-token ()
		 (multiple-value-bind (token value) (funcall lexer-closure)
		   (if (null token)
		       (progn
			 (setf lexer-closure (reload-closure stream))
			 (if (null lexer-closure)
			     (values nil nil)
			     (get-nonempty-token)))
		       (values token value)))))
	(lambda ()
	  (if lexer-closure
	      (get-nonempty-token)
	      nil))))))


;; cl-yacc parser
(define-parser *c-parser*
  ;(:muffle-conflicts t)
  (:start-symbol file)
  (:terminals (char double do else float for if int long return 
	      short sizeof void while identifier constant
	      string << >> ++ -- \&\& \|\| <= >= == != \; { }
	      \, = \( \) [ ] ! ~ - + * / % < > ^ \|))
  ;; Zdefiniowanie priorytetow i lacznosci operatorow
  ;; pozwala zredukowac ilosc produkcji
  (:precedence ((:left * / %) (:left + -) (:left << >>)
               (:left < > <= >=) (:left == !=) (:left &)
               (:left ^) (:left \|) (:left \&\&) (:left \|\|)
               (:right =) (:left \,) (:nonassoc if else)))

  (file 
    (declaration-line)
    (file declaration-line)
    function
    (file function))
  
  (declaration-line
    (declaration \;))

  (declaration
    (type var-init-list))
  
  (var-init-list
    (var-init-list \, pointer-declarator = initializer)
    (pointer-declarator = initializer)
    (var-init-list \, pointer-declarator)
    pointer-declarator)
  
  (pointer-declarator
    declarator
    (pointer declarator))
  
  (declarator
    identifier
    (\( declarator \))
    (declarator [ expression ])
    (declarator [ ] )
    (declarator \( param-list \))
    (declarator \( \)))
  
  (pointer
    *
    (pointer *))
  
  (initializer
    ({ initializer-list })
    expression)
  
  (initializer-list
    (initializer-list \, initializer))
  
  (function
    (type pointer-declarator block)
    (type pointer-declarator block))
  
  (type 
    char
    double
    float
    int
    long
    short
    void)
  
  (param-list
    (param-list \, declaration)
    declaration)
  
  (block
    ({ })
    ({ instruction-list })
    ({ declaration-list })
    ({ declaration-list instruction-list }))
  
  (declaration-list
    declaration-line
    (declaration-list declaration-line))
  
  (instruction-list
    (instruction-list instruction)
    instruction)
  
  (instruction
    block
    (expression-instr)
    conditional
    loop
    (return expression \;)
    (return \;))
  
  (expression-instr
    \;
    (expression \;))
  
  ;; Pomimo, iz ponizsza produkcja wprowadza niejednoznacznosc,
  ;; jest ona dopuszczalna, dzieki zdefiniowaniu priorytetow
  ;; operatorow.
  (expression
    cast-expression
    (expression * expression)
    (expression / expression)
    (expression % expression)
    (expression + expression)
    (expression - expression)
    (expression << expression)
    (expression >> expression)
    (expression > expression)
    (expression < expression)
    (expression >= expression)
    (expression <= expression)
    (expression == expression)
    (expression != expression)
    (expression & expression)
    (expression ^ expression)
    (expression \| expression)
    (expression \&\& expression)
    (expression \|\| expression)
    (unary-expression = expression)
    (expression \, expression))
  
  (cast-expression
    unary-expression
    (\( type \) cast-expression))
  
  (unary-expression
    postfix-expression
    (++ unary-expression)
    (-- unary-expression)
    (+ cast-expression)
    (- cast-expression)
    (* cast-expression)
    (& cast-expression)
    (! cast-expression)
    (~ cast-expression)
    (sizeof unary-expressiion)
    (sizeof \( lvalue \)))
  
  (postfix-expression
    (postfix-expression \( arument-list \))
    (postfix-expression \( \))
    (postfix-expression [ expression ])
    (postfix-expression ++)
    (postfix-expression --)
    (highest-expression))
    
  (argument-list
    expression
    (argument-list \, expression))
  
  (highest-expression
    identifier
    constant
    string-literal
    (\( expression \)))
    
  (conditional
    (if \( expression \) instruction else instruction)
    (if \( expression \) instruction))
    
  (repeat
    (for \( expression-instr expression-instr expression \) instruction)
    (for \( expression-instr expression-instr \) instruction)
    (while \( expression \) instruction)
    (do instruction while \( expression \))))

(defun read-file (path)
  (with-open-file (is path :direction :input)
    (with-output-to-string (os)
      (let (c)
	(loop do (setf c (read-char is nil))
	   while c
	   do (write-char c os))))))

(defun parse-file (path)
  (create-c-lexer c-lexer)
  (parse-with-lexer (c-lexer (read-file path)) *c-parser*))