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

(define-constant +logical-operators+ '(== != < > <= >= \|\| |&&|) :test #'equal)


(defun type-size (type)
  (if (listp type)
      (case (first type)
	(|[]| (* (third type) (type-size (second type))))
	(t 4))
      (cdr (assoc type +builtin-type-sizes+))))

(defun remove-last (list)
	   (cond 
	     ((null list) nil)
	     ((null (cdr list))
	      (setf list nil)
	      list)
	     ((null (cddr list))
	      (setf (cdr list) nil)
	      list)
	     (t
	      (remove-last (cdr list))
	      list)))

(defun pack-lvalue (operator analyzed-subtree type line)
  (cond 
    ((find (value operator) '(unary-* [] ++ --))
     (list (remove-last analyzed-subtree)))
    ((and (eq (value operator) 'var-name)
	  (not (and (listp type)
		    (or (eq (first type) '[])
			(eq (first type) '|()|)))))
     (let ((variable (second analyzed-subtree)))
       (list (symbolicate 'load- (scope variable) '-address) variable)))
    (t
     (with-simple-restart (continue "Continue.")
       (error 'semantic-condition :line line :description "l-value expected"))
     analyzed-subtree)))

(defun auto-convert-types (syntax-subtree source-type destination-type line &optional operator)
  (let ((base-source (get-base-type source-type))
	(base-destination (get-base-type destination-type)))
    (cond
      ((types-equal base-source base-destination) syntax-subtree)
      ((or (and (find base-source +arithmetic-types+)
		(find base-destination +arithmetic-types+)) ; arithmetic types
	   (and (listp base-source)
		(listp base-destination) ; complex types
		(or (and (eq (first base-source) '*)
			 (eq (first base-destination) '*)
			 (or (eq (second base-source) 'void)
			     (eq (second base-destination) 'void))) ; pointer types; one is void*
		    (and (eq (second base-source) (second base-destination)) ; table to pointer
			 (eq (first base-source) '[])
			 (eq (first base-destination) '*))
		    (and (eq (second base-source) (second base-destination)) ; pointer to table
			 (eq (first base-source) '*)
			 (eq (first base-destination) '[])))))
       (pack-with-type-conversion syntax-subtree base-source base-destination))
      ((and (listp base-destination)
	    (find (first base-destination) '(* []))
	    (find base-source +integer-types+)
	    (find operator '(+ -))) ; pointer's artithmetic
       (list syntax-subtree
	     (list 'literal-word (type-size (second base-destination)))
	     (list '*-word)))
      (t (with-simple-restart (continue "Ignore type error")
	   (error 'type-convert-condition :line line :source-type source-type
		  :destination-type destination-type))))))

(defun arithmetic-result-type (type1 type2 line operator) ;;TODO
  (let ((base1 (get-base-type type1))
	(base2 (get-base-type type2))
	(both-const (and (constp type1) (constp type2))))
    (when (and (find operator '(% << >> & ^ \|))
	       (or (find base1 +float-types+)
		   (find base2 +float-types+)))
      (with-simple-restart (continue "Ignore type error")
	(error 'semantic-condition :line line
	       :description (format nil "Operator ~A not applicable for types ~A and ~A"
				    operator base1 base2))))
    (let ((base-result
	   (cond
	     ((or (eq base1 'long-double) (eq base2 'long-double)) 'long-double)
	     ((or (eq base1 'double) (eq base2 'double)) 'double)
	     ((or (eq base1 'float) (eq base2 'float)) 'float)
	     ((or (eq base1 'unsigned-long) (eq base2 'unsigned-long)) 'unsigned-long)
	     ((or (and (eq base1 'long) (eq base2 'unsigned-int))
		  (and (eq base1 'unsigned-int) (eq base2 'long))) 'unsigned-long)
	     ((or (eq base1 'long) (eq base2 'long)) 'long)
	     ((or (eq base1 'unsigned-int) (eq base2 'unsigned-int)) 'unsigned-int)
	     ((and (find base1 +arithmetic-types+) (find base2 +arithmetic-types+)) 'int)
	     ((and (listp base1) (or (eq (first base1) '*) (eq (first base1) '[])))
	      (cond
		((and (eq operator '+) (find base2 +integer-types+)) base1)
		((and (eq operator '-) (or (find base2 +integer-types+)
					   (and (listp base2)
						(eq (first base2) '*)
						(eq (second base1) (second base2)))))
		 base1))) ;TODO error
	     ((and (listp base2)
		   (or (eq (first base2) '*)
		       (eq (first base2) '[]))
		   (eq operator '+)
		   (find base1 +integer-types+))
	      base2)
	     (t
	      (with-simple-restart (continue "Ignore type error")
		(error 'semantic-condition :line line
		       :description (format nil "Operator ~A not applicable for types ~A and ~A"
					    operator base1 base2)))
	      'int))))
      (if both-const
	  (make-const base-result)
	  base-result))))

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

(defun internal-type (type)
  (if (find type +float-types+) 'float
    'word))

(defun signed-internal-type (type)
  (cond
    ((find type +float-types+) 'float)
    ((find type +signed-types+) 'signed)
    (t 'word)))

(defun literal-target-type (type)
  (if (listp type)
      (symbolicate (second type) '*)
      (target-type type)))

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
    (list (pack-lvalue (value (first syntax-subtree)) (first child)
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
    ((find (value (first syntax-subtree)) '(++ -- post++ post--)))
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
    (list (list (pack-lvalue (value (first syntax-subtree)) (first child) type
			     (line (first syntax-subtree)))
		(list 'copy)
		(list 'copy)
		(list 'load)
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


;; (define-guard-function analyze-expression (syntax-subtree symbol-tables )
;;   (lambda (syntax-subtree a)
;;     (declare (ignore a))
;;     (find (value (first syntax-subtree)) '(== != < > <= >= \|\| |&&|)))
;;   "Logical operators"
;;   (let* ((child1 (analyze-expression (second syntax-subtree) symbol-tables))
;; 	 (child2 (analyze-expression (third syntax-subtree) symbol-tables))
;; 	 (type (get-base-type (third child1))))
;;     (if (eq type (get-base-type (third child2)))
;; 	(list (list (first child1) (first child2)
;; 		    (list (symbolicate (value (first syntax-subtree)) '-
;; 				        (signed-internal-type type))))
;; 	      symbol-tables
;; 	      'int)
;; 	(error "Types must be identical for now"))))

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
		  (ignore-additional-arguments () (nreverse out-list))))
	       ((null args-list) ; too few arguments
		(restart-case (error 'semantic-condition :line (line (first syntax-subtree))
				     :description "Too few arguments for function.")
		  (ignore-additional-arguments () (nreverse out-list))))
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
	  (add-function ()
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

(defun flatten-syntax-tree (syntax-tree)
  (labels ((visit-list (syntax-tree output-list)
	     (cond
	       ((null syntax-tree) output-list)
	       ((and (first syntax-tree) (symbolp (first syntax-tree)))
		(cons syntax-tree output-list))
	       (t (visit-list (rest syntax-tree)
			      (visit-list (first syntax-tree) output-list))))))
    (nreverse (visit-list syntax-tree nil))))

(defun check-if-type-exists (syntax-subtree symbol-tables)
  (if (listp syntax-subtree)
      (check-if-type-exists (second syntax-subtree) symbol-tables)
      (unless (find (value syntax-subtree) +arithmetic-types+)
	(with-simple-restart (continue "Ignore error")
	  (error 'semantic-condition :line (line syntax-subtree)
		 :description (format nil "Unknown type ~S" (value syntax-subtree)))))))

(defun types-equal (type1 type2)
  (cond
    ((eq (value type1) (value type2)) t)
    ((and (listp type1) (listp type2)
	  (eql (length type1) (length type2))
	  (reduce (lambda (a b) (and a b)) (mapcar #'types-equal type1 type2)))
     t)
    (t nil)))

(defun pack-with-type-conversion (syntax-subtree source-type target-type)
  (flet ((types-compatible-p (source-type target-type)
	   (or (eq (internal-type (value source-type)) (internal-type (value target-type)))
	       (and (listp source-type) (listp target-type)
		    (eq (internal-type (value (first source-type)))
			(internal-type (value (first target-type))))))))
    (if (types-compatible-p source-type target-type)
	syntax-subtree
	(list syntax-subtree
	      (list (symbolicate 'convert-to- (signed-internal-type target-type))
		    (signed-internal-type source-type))))))


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