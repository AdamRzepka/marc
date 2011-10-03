(in-package :c-compiler)

(defstruct symbol-info
  name type function-p init-value args)

(defun add-element (list a &optional (b nil b-supplied-p))
  (if b-supplied-p
      (append list (list b))
      (append list (list a))))

(defun skip-and-append (a b c)
  (append a c))

(defun set-type (type symbol-list)
  (mapcar (lambda (symbol) 
	    (setf (symbol-info-type symbol) type)
	    symbol) 
	  symbol-list))
(defun set-function (symbol a b &optional 
		     (c nil c-supplied-p))
  (setf (symbol-info-function-p symbol) t)
  (setf (symbol-info-args symbol) 
	(if c-supplied-p
	    b
	    nil))
  symbol)

(defun gen-param-list ())
(defun gen-function-header (type pointer-declarator)
  )

(defun gen-stack-variables (block &optional fun-args)
  block)
(defun gen-function (type symbol block)
  (setf (symbol-info-type symbol) type)
  (let ((fun-name (symbol-info-name symbol)))
    `(,symbol
      (|.global| ,fun-name)
      (|.type| ,fun-name |%function|)
      (,fun-name \:)
      ,(gen-stack-variables block))))

(defun to-onp (a op b)
  (append a b (list op)))

(defun swap (a b)
  (append b (list a)))

(defun symbol-concat (a b)
  (intern 
   (concatenate 'string
		(string a)
		(string b))))

(defun gen-register (level)
  (symbolicate 'r (write-to-string level)))

(defun gen-symbol (onp-expr level)
  `((,(case (caddr onp-expr)
	    ('= 'address)
	    ('un* 'address)
	    ('un& 'address)
	    ('-- 'address)
	    ('++ 'address)
	    ('post++ 'address)
	    ('post-- 'address)
	    ('|()| 'function)
	    (otherwise 'load))
      ,(cadr onp-expr) \,
      ,(gen-register level))))

(defun gen-constant (onp-expr level)
  `((mov ,(gen-register level) \,
	 ,(symbolicate '\# (write-to-string
			    (cadr onp-expr))))))

(defun gen-+ (onp-expr level)
  `((add ,(gen-register (- level 2)) \, 
	 ,(gen-register (- level 2)) \,
	 ,(gen-register (1- level)))))

(defun gen-- (onp-expr level)
  `((sub ,(gen-register (- level 2)) \, 
	 ,(gen-register (- level 2)) \,
	 ,(gen-register (1- level)))))

(defun gen-* (onp-expr level)
  `((mul ,(gen-register (- level 2)) \, 
	 ,(gen-register (- level 2)) \,
	 ,(gen-register (1- level)))))

(defun gen-= (onp-expr level)
  `((str ,(gen-register (- level 2)) \,
	 ,(symbolicate '[ 
		       (gen-register (1- level)) 
		       '|, #0]|))))

#|(defun gen-== (onp-expr level)
  `((cmp ,(gen-register (- level 2)) \, 
	 ,(gen-register (1- level)))
    (moveq ,(gen-register (- level 2)) \, #1)
    (movne ,(gen-register (- level 2)) \, #0))) |#

(defmacro gen-cmp (cmp-symbol cmp-suffix n-cmp-suffix)
  `(defun ,(symbolicate 'gen- cmp-symbol) (onp-expr level)
     (list (list 'cmp (gen-register (- level 2)) '\, 
		 (gen-register (1- level)))
	   (list ',(symbolicate 'mov cmp-suffix)
	    (gen-register (- level 2)) '\, '|#1|)
	   (list ',(symbolicate 'mov n-cmp-suffix)
	    (gen-register (- level 2)) '\, '|#0|))))

(gen-cmp == eq ne)
(gen-cmp != ne eq)
(gen-cmp < lt ge)
(gen-cmp > gt le)
(gen-cmp <= le gt)
(gen-cmp >= ge lt)

(defmacro expr-types (onp-expr types level)
  `(let ((next-expr (car ,onp-expr)))
     (case next-expr
       ,@(loop for type in types
	      collecting
	      `(',(car type) 
		(append 
		 (,(symbolicate 'gen- (car type)) 
		   ,onp-expr ,level)
		 (gen-expression* (cdr ,onp-expr)
				  (+ ,level ,(cadr type))))))
       (otherwise (gen-expression* (cdr ,onp-expr) ,level)))))

(defun gen-expression* (onp-expr level)
  (when onp-expr
    (expr-types onp-expr 
		((symbol 1) (constant 1) (+ -1) (- -1) (* -1)
		 (= -1) (== -1) (!= -1) (< -1) (> -1) (<= -1)
		 (>= -1)) 
		level)))

(defun gen-expression (expression)
  (gen-expression* expression 0))

(defun gen-if (expression instr-if &optional instr-else)
  (let ((label1 (gensym "L"))
	(label2 (gensym "L")))
    (append (gen-expression expression)
	    `((cmp r0 \, |#0|)
	      (beq ,label1))
	    instr-if
	    (when instr-else `((b ,label2)))
	    `((,(symbolicate label1 ":")))
	    instr-else
	    (when instr-else `((,(symbolicate label2 ":")))))))