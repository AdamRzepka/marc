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
     ,@(loop for op in (append (car +tokens+) 
			       (cadr +tokens+))
	    collecting `(,(quote-nonalpha 
			   (string-downcase (string op))) 
			  (return (values ',op ',op))))
     ("[A-Za-z_]\\w*" (return 
			(values 
			 'identifier 
			 (intern 
			  (regex-replace-all "_" $@ "-")))))
					; literaly liczbowe i znakowe
     ,@(loop for pattern in '("\\d+[uUlL]?" "0[0-7]+[uUlL]?" "0x|X[0-9A-Fa-f]+[uUlL]?"
			      "\\d+\\.\\d*([eE][+-]?\\d+)?[fFlL]?"
			      "\\d*\\.\\d+([eE][+-]?\\d+)?[fFlF]?" 
			      "\\d+([eE][+-]?\\d+)?[fFlF]?"
			      "L?'(\\.|[^\\'])+'")
	  collecting `(,pattern 
		       (return (values 
				'constant 
				(read-from-string $@)))))
     ("L?\"(\\.|[^\\\"])*\"" (return 
			       (values 'string (intern $@))))
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
    (file declaration-line (lambda (file declaration-line)
			     (list (append 
				    (car file)
				    declaration-line)
				   (cadr file))))
    (function (lambda (function) 
                      (list (car function) 
			    (substitute-globals 
			     (car function (cadr function))))))
    (file function (lambda (file function)
		     (let ((symbol-list 
			    (append (car file)
				    (car function))))
		       (list symbol-list 
			     (append (cadr file)
				     '(())
				     (substitute-globals 
				      symbol-list 
				      (cadr function))))))))
  
  (declaration-line
    (declaration \; (lambda (a b) a)))

  (declaration
    (type var-init-list #'set-type))
  
  (var-init-list
    (var-init-list \, var-init #'skip-and-append)
    var-init)
  
  (var-init
    (pointer-declarator = initializer)
    (pointer-declarator))

  (pointer-declarator
    declarator
    (pointer declarator (lambda (pointer declarator) 
			  declarator)))
  
  (declarator
    (identifier (lambda (identifier) 
		  (make-symbol-info :name identifier))) 
    (\( declarator \))
    (declarator [ expression ])
    (declarator [ ] )
    (declarator \( param-list \) #'set-function)
    (declarator \( \) #'set-function))
  
  (pointer
    *
    (pointer *))
  
  (initializer
    ({ initializer-list })
    expression)
  
  (initializer-list
    (initializer-list \, initializer))
  
  (function
    (type pointer-declarator block #'gen-function))
  
  (type 
    char
    double
    float
    int
    long
    short
    void)
  
  (param-list
    (param-list \, parameter #'skip-and-append)
    parameter)

  (parameter
    (type var-init #'set-type))
  
  (block
    ({ } (lambda (a b) '()))
    ({ instruction-list } (lambda (a b c) (list '() b)))
    ({ declaration-list } (lambda (a b c) (list b '())))
    ({ declaration-list instruction-list } 
       (lambda (a b c d) (list b c))))
  
  (declaration-list
    declaration-line
    (declaration-list declaration-line #'append-line))
  
  (instruction-list
    (instruction-list instruction #'append-line)
    instruction)
  
  (instruction
    (block #'gen-block) 
    expression-instr
    conditional
    loop
    (return expression \; (lambda (r expression s)
			    (gen-expression expression)))
    (return \;))
  
  (expression-instr
    (\; (lambda () nil))
    (expression \; (lambda (expression s) 
		     (gen-expression expression))))
  
  ;; Pomimo, iz ponizsza produkcja wprowadza niejednoznacznosc,
  ;; jest ona dopuszczalna, dzieki zdefiniowaniu priorytetow
  ;; operatorow.
  (expression
    cast-expression
    (expression * expression #'to-onp)
    (expression / expression #'to-onp)
    (expression % expression #'to-onp)
    (expression + expression #'to-onp)
    (expression - expression #'to-onp)
    (expression << expression #'to-onp)
    (expression >> expression #'to-onp)
    (expression > expression #'to-onp)
    (expression < expression #'to-onp)
    (expression >= expression #'to-onp)
    (expression <= expression #'to-onp)
    (expression == expression #'to-onp)
    (expression != expression #'to-onp)
    (expression & expression #'to-onp)
    (expression ^ expression #'to-onp)
    (expression \| expression #'to-onp)
    (expression \&\& expression #'to-onp)
    (expression \|\| expression #'to-onp)
    (unary-expression = expression 
		      (lambda (a b c)
			(append c a (list b))))
    (expression \, expression #'to-onp))
  
  (cast-expression
    unary-expression
    (\( type \) cast-expression))
  
  (unary-expression
    postfix-expression
    (++ unary-expression #'swap)
    (-- unary-expression #'swap)
    (+ cast-expression (lambda (a b) b))
    (- cast-expression (lambda (a b) (append b (list 'un-))))
    (* cast-expression (lambda (a b) (append b (list 'un*))))
    (& cast-expression (lambda (a b) (append b (list 'un&))))
    (! cast-expression #'swap)
    (~ cast-expression #'swap)
    (sizeof unary-expressiion #'swap)
    (sizeof \( lvalue \) (lambda (a b c d) 
			   (append c (list a)))))
  
  (postfix-expression
    (postfix-expression \( expression \) 
			(lambda (a b c d) 
			  (append '(fun-start) c '(|()|) a)))
    (postfix-expression \( \) (lambda (a b c) 
				(append a (list '|()|))))
    (postfix-expression [ expression ] 
			(lambda (a b c d) 
			  (append c a (list '+ 'un*))))
    (postfix-expression ++ (lambda (a b) 
			     (append a (list 'post++))))
    (postfix-expression -- (lambda (a b) 
			     (append a (list 'post--))))
    highest-expression)
    
  #|(argument-list
    expression
    (argument-list \, expression #'skip-and-append))|#
  
  (highest-expression
    (identifier (lambda (name) (list 'symbol name)))
    (constant (lambda (value) (list 'constant value)))
    (string-literal (lambda (string) (list 'string string)))
    (\( expression \) (lambda (a b c) b)))
    
  (conditional
    (if \( expression \) instruction else instruction
	(lambda (t1 t2 expression t3 instr-if t4 instr-else)
	  (gen-if expression instr-if instr-else)))
    (if \( expression \) instruction
	(lambda (t1 t2 expression t3 instr-if)
	  (gen-if expression instr-if instr-else))))
    
  (repeat
    (for \( expression-instr expression-instr expression \) instruction)
    (for \( expression-instr expression-instr \) instruction)
    (while \( expression \) instruction)
    (do instruction while \( expression \) \;)))

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