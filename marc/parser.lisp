;;; Lexer and parser definitions.

(in-package :marc)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun quote-nonalpha (token)
   (coerce (loop for c across token
	      appending (if (alphanumericp c)
			    `(,c)
			    `(#\\ ,c))) 'string))

 ;;; Tokens split into 3 categories.
 (define-constant +tokens+ '((char double do else float for if int long return 
			  short sizeof void while)
			 (<= >= == != \; { } \, = \( \) [ ] ! ~ -- ++ - + * /
			  % < > ^ \|\| \&\& \| \&)
			 (identifier constant string)) :test #'equal))

(defclass token-info ()
  ((value :type symbol
	  :accessor value
	  :initarg :value)
   (line :type integer
	 :accessor line
	 :initarg :line
	 :initform (error "Line unspecified.")))
  (:documentation "It holds token value and some additional info (line number for now)."))

(define-condition lexer-error (error)
  ((char :initarg :character :reader char)
   (line :initarg :line :reader line))
  (:report (lambda (c stream)
	     (format stream "Line ~D: Forbiden character ~C" (line c) (char c)))))

(defmacro create-c-lexer (name)
  `(let ((line-number 1)) 
     (define-string-lexer ,name
	 ;; comments
	 ("/\\*(\\*[^/]|[^\\*])*\\*/")
       ;; keywords and operators
       ,@(loop for op in (append (car +tokens+)
				 (cadr +tokens+))
	    collecting `(,(quote-nonalpha 
			   (string-downcase (string op))) 
			  (return (values ',op
					  (make-instance 'token-info 
							 :value ',op :line line-number)))))
       ("[A-Za-z_]\\w*" (return
			  (values
			   'identifier 
			   (make-instance 'token-info
					  :value
					  (intern 
					   (regex-replace-all "_" $@ "-"))
					  :line line-number))))
       ;; literals (integers, floats and characters)
       ,@(loop for pattern in '("\\d+[uUlL]?" "0[0-7]+[uUlL]?" "0x|X[0-9A-Fa-f]+[uUlL]?"
				"\\d+\\.\\d*([eE][+-]?\\d+)?[fFlL]?"
				"\\d*\\.\\d+([eE][+-]?\\d+)?[fFlF]?" 
				"\\d+([eE][+-]?\\d+)?[fFlF]?"
				"L?'(\\.|[^\\'])+'")
	    collecting `(,pattern 
			 (return (values 
				  'constant
				  (make-instance 'token-info
						 :value (read-from-string $@)
						 :line line-number)))))
       ;; string literals
       ("L?\"(\\.|[^\\\"])*\"" (return 
				 (values 'string (make-instance 'token-info
								:value (intern $@)
								:line line-number))))
       ;; end of line
       ("\\n" (incf line-number))
       ;;other characters
       ("." (with-simple-restart (continue "Continue reading input.")
	      (error "Forbidden character ~S." $@))))))

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


;;; cl-yacc parser
(define-parser *c-parser*
  (:muffle-conflicts t)
  (:start-symbol file)
  (:terminals (char double do else float for if int long return 
	      short sizeof void while identifier constant
	      string << >> ++ -- \&\& \|\| <= >= == != \; { }
	      \, = \( \) [ ] ! ~ - + * / % < > ^ \|))
  (:precedence ((:left * / %) (:left + -) (:left << >>)
               (:left < > <= >=) (:left == !=) (:left &)
               (:left ^) (:left \|) (:left \&\&) (:left \|\|)
               (:right =) (:left \,) (:nonassoc if else)))

  (file 
    (declaration-line (lambda (declaration-line)
			(list declaration-line
			      (gen-global-variables 
			       declaration-line))))
    (file declaration-line (lambda (file declaration-line)
			     (list (append 
				    (car file)
				    declaration-line)
				   (append 
				    (cadr file)
				    (gen-global-variables
				     declaration-line)))))
    (function (lambda (function) 
                      (list (car function) 
			    (substitute-globals 
			     (car function) (cadr function)))))
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
    (declaration \; (lambda (a b) (declare (ignore b)) a)))

  (declaration
    (type var-init-list #'set-type))
  
  (var-init-list
    (var-init-list \, var-init #'skip-and-append)
    var-init)
  
  (var-init
    (pointer-declarator = initializer #'unsupported)
    (pointer-declarator))

  (pointer-declarator
    declarator
    (pointer declarator (lambda (pointer declarator)
			  (declare (ignore pointer))
			  declarator)))
  
  (declarator
    (identifier (lambda (identifier) 
		  (make-symbol-info :name identifier))) 
    (\( declarator \))
    (declarator [ expression ] #'unsupported)
    (declarator [ ] #'unsupported)
    (declarator \( param-list \) #'set-function)
    (declarator \( \) #'set-function))
  
  (pointer
    (* #'unsupported)
    (pointer * #'unsupported))
  
  (initializer
    ({ initializer-list })
    expression)
  
  (initializer-list
    (initializer-list \, initializer))
  
  (function
    (type pointer-declarator block #'gen-function))
  
  (type 
    char
    (double #'unsupported)
    (float #'unsupported)
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
    ({ } (lambda (a b) (declare (ignore a b)) '()))
    ({ instruction-list } (lambda (a b c) (declare (ignore a c)) (list '() b)))
    ({ declaration-list } (lambda (a b c) (declare (ignore a c)) (list b '())))
    ({ declaration-list instruction-list } 
       (lambda (a b c d) (declare (ignore a d)) (list b c))))
  
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
			    (declare (ignore r s))
			    (gen-expression expression)))
    (return \; (lambda (a b)
		 (declare (ignore a b))
		 nil)))
  
  (expression-instr
    (\; (lambda (a)
	  (declare (ignore a))
	  nil))
    (expression \; (lambda (expression s) 
		     (declare (ignore s))
		     (gen-expression expression))))
  
  ;; Pomimo, iz ponizsza produkcja wprowadza niejednoznacznosc,
  ;; jest ona dopuszczalna, dzieki zdefiniowaniu priorytetow
  ;; operatorow.
  (expression
    cast-expression
    (expression * expression #'to-onp)
    (expression / expression #'unsupported)
    (expression % expression #'unsupported)
    (expression + expression #'to-onp)
    (expression - expression #'to-onp)
    (expression << expression  #'unsupported)
    (expression >> expression #'unsupported)
    (expression > expression #'to-onp)
    (expression < expression #'to-onp)
    (expression >= expression #'to-onp)
    (expression <= expression #'to-onp)
    (expression == expression #'to-onp)
    (expression != expression #'to-onp)
    (expression & expression  #'unsupported)
    (expression ^ expression  #'unsupported)
    (expression \| expression  #'unsupported)
    (expression \&\& expression  #'unsupported)
    (expression \|\| expression  #'unsupported)
    (unary-expression = expression 
		      (lambda (a b c)
			(append c a (list b))))
    (expression \, expression #'to-onp))
  
  (cast-expression
    unary-expression
    (\( type \) cast-expression #'unsupported))
  
  (unary-expression
    postfix-expression
    (++ unary-expression #'unsupported)
    (-- unary-expression #'unsupported)
    (+ cast-expression (lambda (a b) 
			 (declare (ignore a))
			 b))
    (- cast-expression #'unsupported;(lambda (a b) (append b (list 'un-)))
       )
    (* cast-expression #'unsupported;(lambda (a b) (append b (list 'un*)))
       )
    (& cast-expression #'unsupported;(lambda (a b) (append b (list 'un&)))
       )
    (! cast-expression #'unsupported;#'swap
       )
    (~ cast-expression #'unsupported;#'swap
       )
    (sizeof unary-expressiion #'unsupported)
    (sizeof \( lvalue \) #'unsupported))
  
  (postfix-expression
    (postfix-expression \( expression \) 
			(lambda (a b c d) 
			  (declare (ignore b d))			    
			  (append '(fun-start) c '(|()|) a)))
    (postfix-expression \( \) (lambda (a b c)
				(declare (ignore b c))
 				(append a (list '|()|))))
    (postfix-expression [ expression ] 
			#'unsupported)
    (postfix-expression ++ #'unsupported)
    (postfix-expression -- #'unsupported)
    highest-expression)
    
  #|(argument-list
    expression
    (argument-list \, expression #'skip-and-append))|#
  
  (highest-expression
    (identifier (lambda (name) (list 'symbol name)))
    (constant (lambda (value) (list 'constant value)))
    (string-literal #'unsupported;(lambda (string) (list 'string string))
     )
    (\( expression \) (lambda (a b c)
			(declare (ignore a c))
			b)))
    
  (conditional
    (if \( expression \) instruction else instruction
	(lambda (t1 t2 expression t3 instr-if t4 instr-else)
	  (declare (ignore t1 t2 t3 t4))
	  (gen-if expression instr-if instr-else)))
    (if \( expression \) instruction
	(lambda (t1 t2 expression t3 instr-if)
	  (declare (ignore t1 t2 t3))
 	  (gen-if expression instr-if))))
    
  (repeat
    (for \( expression-instr expression-instr expression \) instruction)
    (for \( expression-instr expression-instr \) instruction)
    (while \( expression \) instruction)
    (do instruction while \( expression \) \; #'unsupported)))

