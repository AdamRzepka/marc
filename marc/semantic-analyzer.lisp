(in-package :marc)

(define-constant +integer-types+ '(char short int long unsigned-char unsigned-short
				   unsiged unsigned-long) :test #'equal)
(define-constant +float-types+ '(float double long-double) :test #'equal)

(defun pack-lvalue (syntax-subtree type line)
  (when (or (null (find (value (first syntax-subtree))
			'(var-name unary* [] ++ --)))
	    (eq (first type) '[]))
    (with-simple-restart (continue "Continue.")
      (error 'semantic-condition :line line :description "lvalue expected")))
  (list 'address syntax-subtree))

(clear-guard-function analyze-expression)

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (if (symbolp (first syntax-subtree))
	 (search "literal" (string (first syntax-subtree)))
	 nil))
  "Literals"
  (declare (ignore context))
  (list syntax-subtree symbol-tables (get-literal-type (first syntax-subtree))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (eq (first syntax-subtree) 'var-name))
  "Variables"
  (declare (ignore context))
  (let* ((token (second syntax-subtree))
	 (symbol (find-symbol-in-tables (value token) symbol-tables)))
    (if symbol
	(list syntax-subtree symbol-tables (symbolicate 'l- (variable-type symbol)))
	(error 'undeclared-identifier :line (line token) :identifier (value token)))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (and (find (value (first syntax-subtree)) '(+ - * / % << >> & ^ \|))
	 (eql (length syntax-subtree) 3)))
  "Arithmetic operators"
  (let ((child1 (analyze-expression (second syntax-subtree) symbol-tables context))
	(child2 (analyze-expression (third syntax-subtree) symbol-tables context)))
    (if (eq (deconst-type (third child1)) (deconst-type (third child2)))
	(list (list (first syntax-subtree) (first child1) (first child2))
	      symbol-tables
	      (deconst-type (third child1)))
	(error "Types must be identical for now"))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (find (value (first syntax-subtree)) '(== != < > <= >= \|\| |&&|)))
  "Logical operators"
  (let ((child1 (analyze-expression (second syntax-subtree) symbol-tables context))
	(child2 (analyze-expression (third syntax-subtree) symbol-tables context)))
    (if (eq (deconst-type (third child1)) (deconst-type (third child2)))
	(list (list (first syntax-subtree) (first child1) (first child2))
	      symbol-tables
	      'int)
	(error "Types must be identical for now"))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (find (value (first syntax-subtree)) '(=)))
  "Assignment"
  (let ((child1 (analyze-expression (second syntax-subtree) symbol-tables context))
	(child2 (analyze-expression (third syntax-subtree) symbol-tables context)))
    (if (eq (deconst-type (third child1)) (deconst-type (third child2)))
	(list (list (first syntax-subtree)
		    (pack-lvalue (first child1) (third child1) (line (first syntax-subtree)))
		    (first child2))
	      symbol-tables
	      (deconst-type (third child1)))
	(error "Types must be identical for now"))))

;;; TODO check if type exists
(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
  (lambda (syntax-subtree a b)
    (declare (ignore a b))
    (eq (value (first syntax-subtree)) 'type-cast))
  "Type cast"
  (let ((child (analyze-expression (third syntax-subtree) symbol-tables context)))
    (list (first child) (second child) (value (third syntax-subtree))))) ; TODO convert type to LISP format

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
    (lambda (syntax-subtree a b)
      (declare (ignore a b))
      (eq (first syntax-subtree) 'declaration-line))
  "Declaration line"
  )

(define-guard-function analyze-expression (syntax-subtree symbol-tables context)
    nil
  "Recursively checks for semantic errors (mainly type errors)
and adds explicit casts etc. Returns modified tree, modified symbol tables and type
of the expression."
  (let (modified-subtree (modified-tables symbol-tables))
    (dolist (element syntax-subtree (list (nreverse modified-subtree) modified-tables 'void))
      (if (listp element)
	(let ((child (analyze-expression element modified-tables context)))
	  (push (first child) modified-subtree)
	  (setf modified-tables (second child)))
	(push element modified-subtree)))))

