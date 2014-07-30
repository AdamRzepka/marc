(in-package :marc)

(define-constant +logical-operators+ '(== != < > <= >= \|\| |&&|) :test #'equal)


(define-guard-function-family analyze-expression
    (syntax-subtree symbol-tables ))


(define-guard-function analyze-expression (syntax-subtree symbol-tables )
  ((and (symbolp (value (first syntax-subtree)))
	(search "LITERAL" (string (value (first syntax-subtree))))))
  "Literals"
  (let* ((type (get-literal-type (value (first syntax-subtree))))
	 (target-type (literal-target-type type)))
    (list (list (symbolicate 'literal- target-type) (value (second syntax-subtree)))
	  symbol-tables (make-const type))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
    ((eq (value (first syntax-subtree)) 'var-name))
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
						     'local 'unknown))
	    (setf symbol (find-symbol-in-tables (value token)
						symbol-tables)))))
    (let ((type (variable-type symbol))
	  (scope (if (eq (scope symbol) 'global)
		     'global
		     'local)))
      (list (list (if (and (listp type) (eq (first type) '[]))
		      (symbolicate 'load- (scope symbol) '-address)
		      (symbolicate 'load- scope '- (target-type type)))
		  symbol)
	    symbol-tables
	    type))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables )
    ((and (find (value (first syntax-subtree)) '(+ - * / % << >> & ^ \|
						 == != < > <= >= \|\| |&&|))
	  (eql (length syntax-subtree) 3)))
  "Binary operators"
  (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
	 (child2 (analyze-expression (third syntax-subtree) symbol-tables))
	 (type1 (third child1))
	 (type2 (third child2))
	 (line (line (first syntax-subtree)))
	 (operator (value (first syntax-subtree)))
	 (destination-type (arithmetic-result-type type1 type2 line operator))
	 (result-type (if (find operator +logical-operators+)
			  (if (constp destination-type)
			      (make-const 'int)
			      'int)
			  destination-type)))
    (list (if (constp result-type)
	      (list (symbolicate 'literal- (literal-target-type (get-base-type result-type)))
		    (calculate-const-expression (list operator
						      (second (first child1))
						      (second (first child2)))
						(get-base-type destination-type)))
	      (if (find operator '(\|\| |&&|))
		  (analyze-logical operator
				   (auto-convert-types (first child1) type1 destination-type line)
				   (auto-convert-types (first child2) type2 destination-type line))
		  (list (auto-convert-types (first child1) type1 destination-type line operator)
			(auto-convert-types (first child2) type2 destination-type line operator)
			(list (symbolicate (value (first syntax-subtree)) '-
					   (internal-type destination-type))))))
	  symbol-tables
	  (if (find operator +logical-operators+)
	      result-type
	      destination-type))))

(defun analyze-logical (operator child1 child2)
  (if (eq operator '\|\|)
      (analyze-or child1 child2)
      (analyze-and child1 child2)))

(defun analyze-or (child1 child2)
  (let ((true-label (genlabel))
	(end-label (genlabel)))
    (list child1
	  (list 'test)
	  (list 'jump-if-ne true-label)
	  child2
	  (list 'test)
	  (list 'jump-if-ne true-label)
	  (list 'literal-word 0)
	  (list 'jump end-label)
	  (list 'insert-label true-label)
	  (list 'literal-word 1)
	  (list 'insert-label end-label))))

(defun analyze-and (child1 child2)
  (let ((false-label (genlabel))
	(end-label (genlabel)))
    (list child1
	  (list 'test)
	  (list 'jump-if-eq false-label)
	  child2
	  (list 'test)
	  (list 'jump-if-eq false-label)
	  (list 'literal-word 1)
	  (list 'jump end-label)
	  (list 'insert-label false-label)
	  (list 'literal-word 0)
	  (list 'insert-label end-label))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((find (value (first syntax-subtree)) '(unary-- ~)))
  (let* ((child (analyze-expression (second syntax-subtree) symbol-tables))
	 (type (third child))
	 (operator (value (first syntax-subtree))))
    (when (or (not (find type +arithmetic-types+))
	      (and (eq operator '~)
		   (find (get-base-type type) +float-types+)))
      (with-simple-restart (continue "Ignore type error")
	(error 'semantic-condition :line (line (first syntax-subtree))
	       :description (format nil "Operator ~A not applicable for type ~A"
				    operator (get-base-type type)))))
    (list (if (constp type)
	      (list (symbolicate 'literal- (literal-target-type (get-base-type type)))
		    (calculate-const-expression (list operator (second (first child)))
						(get-base-type type)))
	      (list (first child)
		    (list (symbolicate operator '- (internal-type (third child))))))
	  (second child)
	  type)))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((eq (first (value syntax-subtree)) '!))
  (let ((child (analyze-expression (second syntax-subtree) symbol-tables)))
    (if (constp (third child))
	(list (list (symbolicate 'literal- (literal-target-type (get-base-type (third child))))
		    (calculate-const-expression (list '! (second (first child)))
						(get-base-type (third child))))
	      (second child)
	      (make-const 'int))
	(list (list (first child)
		    (list (symbolicate '!- (internal-type (get-base-type (third child))))))
	      (second child)
	      'int))))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((eq (value (first syntax-subtree)) 'unary-&))
  (let ((child (analyze-expression (second syntax-subtree) symbol-tables)))
    (list (pack-lvalue (value (first (second syntax-subtree))) (first child)
		       (third child) (line (first syntax-subtree)))
	  (second child)
	  (list '* (third child)))))
		
(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((eq (value (first syntax-subtree)) 'unary-*))
  (let* ((child (analyze-expression (second syntax-subtree) symbol-tables))
	 (type (get-base-type (third child))))
    (unless (and (listp type)
		 (find (first type) '(* [])))
      (with-simple-restart (continue "Ignore error")
	(error 'semantic-condition :line (line (first syntax-subtree))
	       :description (format nil "Type ~A not dereferencable."
				    type)))
      (setf type (list '* type)))
    (list (list (first child)
		(list (symbolicate 'load- (target-type (second type)))))
	  (second child)
	  (second type))))			; type loses const (TODO?)

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((find (value (first syntax-subtree)) '(++ -- post-++ post---)))
  (let* ((child (analyze-expression (second syntax-subtree) symbol-tables))
	 (type (third child)))
    (unless (or (find type +integer-types+)
		(and (listp type)
		     (eq (first type) '*)))
      (with-simple-restart (continue "Ignore error")
	(error 'semantic-condition :line (line (first syntax-subtree))
	       :description (format nil
				    "Only integer types allowed with operator ~A."
				    (value (first syntax-subtree))))))
    (list (list (pack-lvalue (value (first (second syntax-subtree))) (first child) type
			     (line (first syntax-subtree)))
		(list 'copy)
		(list 'copy)
		(list (symbolicate 'load- (target-type type)))
		(list 'literal-word (if (and (listp type)
					     (eq (first type) '*))
					(type-size (second type))
					1))
		(if (eq (value (first syntax-subtree)) '++)
		    (list '+-word)
		    (list '--word))
		(list '=-word)
		(list 'pop)
		(list (symbolicate 'load- (target-type type))))
	  (second child)
	  type)))


(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((find (value (first syntax-subtree)) '(=)))
  "Assignment"
  (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
	 (child2 (analyze-expression (third syntax-subtree) symbol-tables))
	 (type (get-base-type (third child1))))
    (list (list (pack-lvalue (first (second syntax-subtree))
			     (first child1) (third child1) (line (first syntax-subtree)))
		(auto-convert-types (first child2) (third child2) (third child1)
				    (line (first syntax-subtree)))
		(list (symbolicate (value (first syntax-subtree)) '- (target-type type))))
	  symbol-tables
	  type)))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((eq (value (first syntax-subtree)) '|()|))
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
		  (continue () (nreverse out-list))))
	       ((null args-list) ; too few arguments
		(restart-case (error 'semantic-condition :line (line (first syntax-subtree))
				     :description "Too few arguments for function.")
		  (continue () (nreverse out-list))))
	       (t ; next argument	      
		(let ((analyzed-arg (analyze-expression (first args-list) symbol-tables)))
		  (setf symbol-tables (second analyzed-arg))
		  ;; (unless (or (eq (variable-type (first param-list)) '|...|)
		  ;; 	      (types-equal (get-base-type (third analyzed-arg))
		  ;; 			   (get-base-type (variable-type (first param-list)))))
		  ;;   (error "Types must be identical for now."))
		  (push (auto-convert-types (first analyzed-arg)
					    (third analyzed-arg)
					    (if (eq (variable-type (first param-list)) '|...|)
						(third analyzed-arg)
						(variable-type (first param-list)))
					    (line (caar args-list)))
			out-list)
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
	  (treat-as-int ()
	    (add-to-symbol-table symbol-tables
				 (make-variable-info 'int
						     `(|()| ,name
							    ((|...| ((|...|)))))
						     'global 'unknown)))))
      (list (list (list 'save-registers)
		  (analyze-args (third (variable-type function-info)) arg-list)
		  (list '|()| function-info)
		  (list 'load-registers))
	    symbol-tables (second (variable-type function-info))))))


(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    ((eq (value (first syntax-subtree)) 'type-cast))
  "Type cast"
  (labels ((extract-type (syntax-subtree)
	     (if (listp syntax-subtree)
		 (mapcar #'extract-type syntax-subtree)
		 (value syntax-subtree))))
    (let* ((child (analyze-expression (second syntax-subtree) symbol-tables))
	   (source-type (third child))
	   (target-type (extract-type (third syntax-subtree))))
      (check-if-type-exists target-type symbol-tables)
      (list (pack-with-type-conversion (first child) source-type target-type)
	    (second child)
	    target-type)
      ;; (if (types-compatible-p source-type target-type)
      ;; 	  (list (first child) (second child) target-type)
      ;; 	  (list `((,(symbolicate 'convert-to- (internal-type target-type)))
      ;; 		  ,(first child))
      ;; 		(second child)
      ;; 		target-type))
      )))

(define-guard-function analyze-expression (syntax-subtree symbol-tables)
    (t)
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

(defun calculate-const-expression (syntax-subtree type)
  (let* ((a (value (second syntax-subtree)))
	 (b (value (third syntax-subtree)))
	 (clear-expr (list (value (first syntax-subtree)) a b)))
    (case (value (first syntax-subtree))
      (/ (if (find type +integer-types+)
	     (floor (eval clear-expr))
	     (eval clear-expr)))
      (% (mod a b))
      (<< (ash a b))
      (>> (ash a (- b)))
      (t (eval clear-expr)))))				;TODO all operators