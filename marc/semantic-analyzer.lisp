(in-package :marc)

(define-constant +signed-types+ '(char short int long) :test #'equal)
(define-constant +unsigned-types+  '(unsigned-char unsigned-short unsiged unsigned-long) :test #'equal)
(define-constant +integer-types+ (append +signed-types+ +unsigned-types+) :test #'equal)

(define-constant +float-types+ '(float double long-double) :test #'equal)

(define-constant +arithmetic-types+ (append +integer-types+ +float-types+) :test #'equal)

(define-constant +builtin-type-sizes+
    '((char . 1) (unsigned-char . 1)
      (short . 2) (unsigned-short . 2)
      (int . 4) (unsigned-int . 4) (long . 4) (unsigned-long . 4)
      (float . 4) (double . 4) (long-double . 4)) :test #'equal)


(defun type-size (type)
  (if (listp type)
      (case (first type)
	(* 4)
	(t 4)) ;
      (cdr (assoc type +builtin-type-sizes+))))

(defun pack-lvalue (syntax-subtree type line)
  (when (or (and (null (find (value (first syntax-subtree)) '(unary* [] ++ --)))
		 (null (search "LOAD-" (string (value (first syntax-subtree))))))
	    (and (listp type) (eq (first type) '[])))
    (with-simple-restart (continue "Continue.")
      (error 'semantic-condition :line line :description "l-value expected")))
  (let ((variable (second syntax-subtree)))
    (list (symbolicate 'load- (scope variable) '-address) variable)))

(defun convert-types (source-type destination-type expression line)
  (cond
    ((equal source-type destination-type) expression)
    ((or (and (find source-type +arithmetic-types+)
	      (find destination-type +arithmetic-types+)) ; arithmetic types
	 (and (listp source-type)
	      (listp destination-type) ; complex types
	      (or (and (eq (first source-type) '*)
		       (eq (first destination-type) '*)
		       (or (eq (second source-type) 'void)
			   (eq (second destination-type) 'void))) ; pointer types; one is void*
		  (and (eq (second source-type) (second destination-type)) ; table to pointer
		       (eq (first source-type) '[])
		       (eq (first destination-type) '*))
		  (and (eq (second source-type) (first destination-type)) ; pointer to table
		       (eq (first source-type) '*)
		       (eq (first destination-type) '[])))))
     (list 'convert source-type destination-type expression))
    (t (with-simple-restart (continue "Ignore type error")
	 (error 'type-convert-condition :line line :source-type source-type
		:destination-type destination-type)))))

(defun arithmetic-result-type (type1 type2 line operator) ;;TODO
  (cond
    ((or (eq type1 'long-double) (eq type2 'long-double)) 'long-double)
    ((or (eq type1 'double) (eq type2 'double)) 'double)
    ((or (eq type1 'float) (eq type2 'float)) 'float)
    ((or (eq type1 'unsigned-long) (eq type2 'unsigned-long)) 'unsigned-long)
    ((or (and (eq type1 'long) (eq type2 'unsigned-int))
	 (and (eq type1 'unsigned-int) (eq type2 'long))) 'unsigned-long)
    ((or (eq type1 'long) (eq type2 'long)) 'long)
    ((or (eq type1 'unsigned-int) (eq type2 'unsigned-int)) 'unsigned-int)
    ((and (find type1 +arithmetic-types+) (find type2 +arithmetic-types+)) 'int)
    ((and (listp type1) (or (eq (first type1) '*) (eq (first type1) '[])))
     (cond
       ((and (eq operator '+) (find type2 +integer-types+)) type1)
       ((and (eq operator '-) (or (find type2 +integer-types+)
				  (and (listp type2)
				       (eq (first type2) '*)
				       (eq (second type1) (second type2)))))
	type1)))
    ((and (listp type2)
	  (or (eq (first type2) '*)
	      (eq (first type2) '[]))
	  (eq operator '+)
	  (find type1 +integer-types+))
     type2)
    (t
     (with-simple-restart (continue "Ignore type error")
       (error 'semantic-condition :line line
	      :description (format nil "Operator ~A not applicable for types ~A and ~A"
				   operator type1 type2)))
     'int)))

(defun target-type (type)
  (if (listp type)
      (case (first type)
	((* [] |()|) 'word)
	(t type))
      (case type
	((int unsigned-int long unsigned-long) 'word)
	((short unsigned-short) 'short)
	((char unsigned-char) 'byte)
	((float double long-double) 'float))))

(define-guard-function-family analyze-expression
    (syntax-subtree symbol-tables ))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (and (symbolp (first syntax-subtree))
	 (search "LITERAL" (string (first syntax-subtree)))))
  "Literals"
  (let* ((type (get-literal-type (first syntax-subtree)))
	 (internal-type (if (listp type)
			    type
			    (target-type type))))
    (list (list (symbolicate 'literal- internal-type) (value (second syntax-subtree)))
	  symbol-tables type)))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (eq (first syntax-subtree) 'var-name))
  "Variables"
  (let* ((token (second syntax-subtree))
	 (symbol (find-symbol-in-tables (value token) symbol-tables)))
    (unless symbol
	(restart-case
	    (error 'undeclared-identifier :line (line token) :identifier (value token))
	  (treat-as-int ()
	    (add-to-symbol-table symbol-tables
				 (make-variable-info 'int
						     (value token)
						     'local))
	    (setf symbol (find-symbol-in-tables (value token)
						symbol-tables)))))
    (let ((type (variable-type symbol)))
      (list (list (symbolicate 'load-
			       (if (eq (scope symbol) 'global)
				   'global
				   'local)
			       '-
			       (target-type type))
		  symbol)
	    symbol-tables
	    type))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (and (find (value (first syntax-subtree)) '(+ - * / % << >> & ^ \|))
	 (eql (length syntax-subtree) 3)))
  "Arithmetic operators"
  (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
	 (child2 (analyze-expression (third syntax-subtree) symbol-tables))
	 (type (get-base-type (third child1))))
    (if (eq type (get-base-type (third child2)))
	(list (list (first child1) (first child2)
		    (list (symbolicate (value (first syntax-subtree)) '- (target-type type))))
	      symbol-tables
	      type)
	(error "Types must be identical for now"))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (find (value (first syntax-subtree)) '(== != < > <= >= \|\| |&&|)))
  "Logical operators"
  (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
	 (child2 (analyze-expression (third syntax-subtree) symbol-tables))
	 (type (get-base-type (third child1))))
    (if (eq type (get-base-type (third child2)))
	(list (list (first child1) (first child2)
		    (list (symbolicate (value (first syntax-subtree)) '- (target-type type))))
	      symbol-tables
	      'int)
	(error "Types must be identical for now"))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (find (value (first syntax-subtree)) '(=)))
  "Assignment"
  (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
	(child2 (analyze-expression (third syntax-subtree) symbol-tables))
	(type (get-base-type (third child1))))
    (if (eq type (get-base-type (third child2)))
	(list (list (pack-lvalue (first child1) (third child1) (line (first syntax-subtree)))
		    (first child2)
		    (list (symbolicate (value (first syntax-subtree)) '- (target-type type))))
	      symbol-tables
	      type)
	(error "Types must be identical for now"))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    (lambda (syntax-subtree a)
      (declare (ignore a))
      (eq (value (first syntax-subtree)) '|()|))
  "Function call"
  (labels ((args-into-list (syntax-subtree &optional list)
	     (if (and (listp syntax-subtree)
		      (eq (value (first syntax-subtree)) '|,|))
		 (progn
		   (push (third syntax-subtree) list)
		   (args-into-list (second syntax-subtree) list))
		 (progn
		   (push syntax-subtree list)
		   list)))
	   (analyze-args (param-list args-list &optional out-list)
	     (cond
	       ((and (or (null param-list)
			 (eq (variable-type (first param-list)) '|...|))
		     (null args-list))
		(nreverse out-list)) ; done
	       ((null param-list)  ; too much arguments
		(restart-case (error 'semantic-condition :line (line (first syntax-subtree))
				     :description "Too much arguments for function.")
		  (ignore-additional-arguments () (nreverse out-list))))
	       ((null args-list) ; too few arguments
		(restart-case (error 'semantic-condition :line (line (first syntax-subtree))
				     :description "Too few arguments for function.")
		  (ignore-additional-arguments () (nreverse out-list))))
	       (t ; next argument	      
		(let ((analyzed-arg (analyze-expression (first args-list) symbol-tables)))
		  (setf symbol-tables (second analyzed-arg))
		  (unless (or (eq (variable-type (first param-list)) '|...|)
			      (eq (third analyzed-arg) (variable-type (first param-list))))
		    (error "Types must be identical for now."))
		  (push (first analyzed-arg) out-list)
		  (analyze-args (if (eq (variable-type (first param-list)) '|...|)
				    param-list
				    (rest param-list))
				(rest args-list)
				out-list))))))
    (let* ((name (value (second (second syntax-subtree))))
	   (function-info (find-symbol-in-tables name
						 symbol-tables))
	   (arg-list (args-into-list (third syntax-subtree))))
      (unless (and function-info (function-info-p function-info))
	(restart-case
	    (error 'undeclared-identifier :identifier name
		   :line (line (first syntax-subtree)))
	  (add-function ()
	    (add-to-symbol-table symbol-tables
				 (make-variable-info 'int
						     `(|()| ,name
							    ((|...| ((|...|)))))
						     'global)))))
      (list (list (list 'save-registers)
		  (analyze-args (third (variable-type function-info)) arg-list)
		  (list '|()| function-info)
		  (list 'load-registers))
	    symbol-tables (second (variable-type function-info))))))

;;; TODO check if type exists
(define-guard-function analyze-expression (syntax-subtree symbol-tables)
  (lambda (syntax-subtree a)
    (declare (ignore a))
    (eq (value (first syntax-subtree)) 'type-cast))
  "Type cast"
  (let ((child (analyze-expression (third syntax-subtree) symbol-tables)))
    (list (first child) (second child) (value (third syntax-subtree))))) ; TODO convert type to LISP format

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    nil
  "Recursively checks for semantic errors (mainly type errors)
and adds explicit casts etc. Returns modified tree, modified symbol tables and type
of the expression."
  (let (modified-subtree (modified-tables symbol-tables))
    (dolist (element syntax-subtree (list (nreverse modified-subtree) modified-tables 'void))
      (if (listp element)
	(let ((child (analyze-expression element modified-tables )))
	  (push (first child) modified-subtree)
	  (setf modified-tables (second child)))
	(push element modified-subtree)))))

(defun flatten-syntax-tree (syntax-tree)
  (labels ((visit-list (syntax-tree output-list)
	     (cond
	       ((null syntax-tree) output-list)
	       ((and (first syntax-tree) (symbolp (first syntax-tree)))
		(cons syntax-tree output-list))
	       (t (visit-list (rest syntax-tree)
			      (visit-list (first syntax-tree) output-list))))))
    (nreverse (visit-list syntax-tree nil))))