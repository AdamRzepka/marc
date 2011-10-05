(in-package :c-compiler)

(defstruct symbol-info
  name type function-p init-value args address)

(defun add-element (list a &optional (b nil b-supplied-p))
  (if b-supplied-p
      (append list (list b))
      (append list (list a))))

(defun skip-and-append (a b c)
  (append a c))

(defun append-line (a b)
  (append a b))

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

(defun gen-stack-addr (offset)
  (symbolicate '|[fp, #-| (write-to-string offset) '|]|))

(defun gen-register (level)
  (symbolicate 'r (write-to-string level)))

(defun substitute-stack-variables (variables instructions)
(labels ((find-variable (name variables)
	   (when variables
	     (if (eq (symbol-info-name (car variables)) name)
		 (car variables)
		 (find-variable name (cdr variables)))))
	 (substitute-address (line)
	   (let ((variable (find-variable 
			    (cadr line) variables)))
	     (if variable
		 `(sub ,(cadddr line) \, fp \, 
		       ,(symbol-info-address variable))
		 line)))
	 (substitute-load (line)
	   (let ((variable (find-variable
			    (cadr line) variables)))
	     (if variable
		 `(ldr ,(cadddr line) \, 
		       ,(gen-stack-addr 
			 (symbol-info-address 
			  (find-variable (cadr line) 
					 variables))))
		 line)))
	 (subst-line (done to-do)
	   (if to-do
	     (case (caar to-do)
		   ('address 
		    (subst-line (cons 
				 (substitute-address 
				  (car to-do))
				 done)
				(cdr to-do)))
		   ('load
		    (subst-line (cons
				 (substitute-load
				  (car to-do))
				 done)
				(cdr to-do)))
		   (otherwise 
		    (subst-line (cons
				 (car to-do)
				 done)
				(cdr to-do))))
	     (reverse done))))
  (subst-line () instructions)))

(defun gen-block (block &optional fun-args)
  (let ((variables (append fun-args (car block)))
	(instructions (cadr block)))
    (loop for var in variables
       for i from 1
       do (setf (symbol-info-address var) (* 4 i)))
    (append
     `((str fp \, |[sp, #-4]!|)
       (mov fp \, sp)
       (sub sp \, sp \, ,(* 4 (length variables)))
       ,@(loop for arg in variables
	    for i from 1 to (length fun-args)
	    collecting `(str 
			,(gen-register i) \, 
			,(symbolicate '|[sp, #-| 
				      (write-to-string 
				       (* 4 i)) 
				      '|]|))))
     (substitute-stack-variables variables instructions)
     '((mov sp \, fp)
       (ldmfd sp \, {fp})))))

(defun gen-function (type symbol block)
  (setf (symbol-info-type symbol) type)
  (let ((fun-name (symbol-info-name symbol)))
    `(,symbol
      (|.global| ,fun-name)
      (|.type| ,fun-name |%function|)
      (,fun-name \:)
      ,@(gen-block block 
		   (symbol-info-args symbol)))))

(defun to-onp (a op b)
  (append a b (list op)))

(defun swap (a b)
  (append b (list a)))

(defun symbol-concat (a b)
  (intern 
   (concatenate 'string
		(string a)
		(string b))))

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
	    (when instr-else 
	      `((,(symbolicate label2 ":")))))))

(defun gen-while (expression instr)
  (let ((label1 (gensym "L"))
	(label2 (gensym "L")))
    (append `((,(symbolicate label1 ":")))
	    (gen-expression expression)
	    `((cmp r0 \, |#0|)
	      (beq ,label2))
	    instr
	    `((b ,label1))
	    `((,(symbolicate label2 ":"))))))

(defun gen-for (expr1 expr2 expr3 instr)
  (let ((label1 (gensym "L"))
	(label2 (gensym "L")))
    (append (gen-expression expr1)
	    `((,(symbolicate label1 ":")))
	    (gen-expression expr3)
	    (gen-expression expr2)
	    `((cmp r0 \, |#0|)
	      (beq ,label2))
	    instr
	    `((b ,label1))
	    `((,(symbolicate label2 ":"))))))



