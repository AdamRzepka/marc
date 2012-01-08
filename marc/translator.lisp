;;;; Semantic analyzer and code generator
(in-package :marc)

;;; A few class for describing symbol table entries

(defclass symbol-info ()
  ((name :type symbol
	 :accessor name
	 :initarg :name))
  (:documentation "Base class for symbol table entry"))

(defclass variable-info (symbol-info)
  ((type :accessor variable-type
	 :initarg :type)
   (scope :type symbol
	  :accessor scope
	  :initarg :scope)
   (address :type integer		; offset from fp - for stack variables only
	    :accessor address
	    :initarg :address
	    :initform nil))
  
  (:documentation "Variable info for symbol table"))

(defclass typedef-info (variable-info)
  ())

(defclass struct-info (symbol-info)
  ((fields :accessor fields
	   :type list
	   :initarg :fields)))

(defclass enum-info (symbol-info)
  ((enums :type list
	  :accessor enums
	  :initarg :enums)))

(defgeneric function-info-p (obj)
  (:documentation "Checks whether variable-info is function info."))

(defmethod function-info-p ((obj symbol-info))
  nil)

(defmethod function-info-p ((obj variable-info))
  (let ((type (variable-type obj)))
    (and (listp type)
	 (eq (first type) '|()|))))

(defmethod print-object ((object variable-info) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~S" (name object) (variable-type object))))

(defclass context ()
  ((enclosing-function :type variable-info
		       :accessor enclosing-function
		       :initarg :enclosing-function
		       :initform nil)
   (enclosing-loop :accessor enclosing-loop
					; :type - TODO
		   :initarg :enclosing-loop
		   :initform nil)
   (enclosing-switch :accessor enclosing-switch
		     :initarg :enclosing-switch
		     :initform nil)
   (enclosing-blocks-count :type integer
			   :accessor enclosing-blocks-count
			   :initform 0))
  (:documentation "For the return, break and continue purposes."))

(defgeneric inc-blocks-count (context))

(defmethod inc-blocks-count ((context context))
  (incf (enclosing-blocks-count context)))

(defgeneric dec-blocks-count (context))

(defmethod dec-blocks-count ((context context))
  (decf (enclosing-blocks-count context)))

(define-condition internal-error (error)
  ((description :type string
		:reader description
		:initarg :description))
  (:report (lambda (c stream)
	     (format stream "Internal compiler error: ~A" (description c)))))

(define-condition semantic-condition (error)
  ((line :type integer
	 :reader line
	 :initarg :line)
   (description :type string
		:reader description
		:initarg :description
		:initform "Semantic error")
   (severity :type integer
	     :reader severity
	     :initarg :severity
	     :initform 'error))
  (:report (lambda (c stream)
	     (format stream "Line ~D: ~A" (line c) (description c)))))

(define-condition undeclared-identifier (semantic-condition)
  ((identifier :type symbol
	       :reader identifier
	       :initarg :identifier))
  (:report (lambda (c stream)
	     (format stream "Line ~D: Undeclared identifier '~A'" (line c) (identifier c)))))

(define-condition type-convert-condition (semantic-condition)
  ((source-type :reader source-type
		:initarg :source-type)
   (destination-type :reader destination-type
		     :initarg :destination-type))
  (:report (lambda (c stream)
	     (format stream "Line ~D: Can not convert from ~A to ~A" (line c) (source-type c)
		     (destination-type c)))))


(defun add-to-symbol-table (symbol-tables symbols)
  "SYMBOL-TABLES is a stack of symbol tables from all accessible scopes.
First table in this list is current scope table. SYMBOLS may be single symbol,
or list of symbols."
  (declare (type list symbol-tables))
  (cond ((listp symbols)
	 (mapc (lambda (symbol) 
		 (setf (gethash (name symbol) (first symbol-tables)) symbol)) symbols))
	((typep symbols 'symbol-info)
	 (setf (gethash (name symbols) (first symbol-tables)) symbols))
	(t
	 (error 
	  'internal-error :description
	  (format nil "Something strange passed to add-to-symbol-table: ~S." symbols))))
  symbol-tables)

(defun add-start-symbol (instructions)
  (cons `(start-symbol) instructions))

(defun analyze-file (file)
  "FILE is a syntax tree created with build-syntax-tree"
  (let ((symbol-tables (list (make-hash-table)))
	result)
    (dolist (entry file (add-start-symbol (flatten-syntax-tree (nreverse result))))
      (case (value (first entry))
	(declaration-line
	 (let ((declaration-list
		(analyze-declaration-line entry symbol-tables 'global)))
	   ;; (mapc (lambda (declaration) (when declaration (push declaration result)))
	   ;; 	 (first declaration-list))
	   (push (first declaration-list) result)
	   (add-to-symbol-table symbol-tables (second declaration-list))))
	(function-definition
	 (let ((function (analyze-function entry symbol-tables)))
	   (push (first function) result)
;	   (mapc (lambda (instruction) (push instruction result)) (second function))
	   (add-to-symbol-table symbol-tables (second function))))
	(t (unsupported (first entry)))))))

(defun analyze-table-size (size-expression symbol-tables line)
  (let ((analyzed-expression (analyze-expression size-expression symbol-tables)))
    (cond
      ((not (constp (third analyzed-expression)))
       (with-simple-restart (continue "Ignore error")
	 (error 'semantic-condition :line line
		:description "Table size initializer must be constant expression."))
       1)
      ((not (find (second (third analyzed-expression)) +integer-types+))
       (with-simple-restart (continue "Ignore error")
	 (error 'semantic-condition :line line
		:description "Table size initializer must be integer."))
       1)
      ((<= (second (first analyzed-expression)) 0)
       (with-simple-restart (continue "Ignore error")
	 (error 'semantic-condition :line line
		:description "Table size initializer must be greater than 0."))
       1)
      (t
       (second (first analyzed-expression))))))

(defun make-variable-info (base-type variable scope line &optional symbol-tables)
  "Recursively adds *, [] or () to BASE-TYPE and creates variable-info instance"
  (if (listp variable)
      (case (value (first variable))
	(* (make-variable-info (list '* base-type) (second variable) scope line symbol-tables))
	([] (make-variable-info (list '[] base-type (analyze-table-size (third variable)
									symbol-tables line))
				(second variable) scope line symbol-tables))
	(|()| (make-variable-info (list '|()| base-type
					(mapcan (lambda (arg)
						  (second
						   (analyze-declaration-line
						    (cons 'declaration-line arg) nil 'argument)))
						(third variable)))
				  (second variable) scope line symbol-tables)))
      (progn
	(when (types-equal (value base-type) 'void)
	  (with-simple-restart (continue "Ignore error")
	    (error 'semantic-condition :line line
			     :description "Declaring void variable is forbidden."))
	  (setf base-type 'int))
	(make-instance 'variable-info :name (value variable) :type (value base-type) :scope scope))))

(defun analyze-initializer (initializer variable symbol-tables scope) ;TODO
  (when initializer
    (let ((expression (analyze-expression initializer symbol-tables))
	  (type (variable-type variable)))
      (case scope
	(global (if (constp (third expression))
		    (second (auto-convert-types (first expression) (third expression)
						type (line (first initializer))))
		    (with-simple-restart (continue "Ignore error")
		      (error 'semantic-condition :line (line (first initializer))
			     :description "Global variable initializer must be constant."))))
	(local	 
	 (list (list 'load-local-address variable)
	       (auto-convert-types (first expression) (third expression)
				   type (line (first initializer)))
	       (if (and (listp type) (eq (first type) '[]) (eq (second type) 'char))
		   (list (list 'literal-word (type-size type))
			 (list 'copy-object))
		   (list (symbolicate '=- (target-type type))))
	       (list 'clear-registers-counter)))))))

(defun allocate-stack-variables (symbol-tables scope)
  (let ((next-address 0))
    (flet ((increment-address (size)
	     (setf next-address
		   (cond ((eq scope 'local)
			  (- next-address size))
			 ((eq scope 'argument)
			  (if (= next-address 16)
			      48		; skipping saved registers
			      (+ next-address 4)))
			 (t (error 'internal-error :description
				   "Strange scope passed to alocate-stack-variable."))))))
      (with-hash-table-iterator (next (first symbol-tables))
	(loop
	   (multiple-value-bind (more? key value) (next)
	     (declare (ignore key))
	     (unless more? (return))
	     (increment-address (type-size (variable-type value)))
	     (setf (address value) next-address))))))
  symbol-tables)

(defun analyze-single-declaration (single-declaration base-type symbol-tables scope)
  "Analyzes single declaration like a=3. SINGLE-DECLARATION would be ('a 3) for example."
  (let ((variable-info (make-variable-info (value base-type) (first single-declaration) scope
					   (line (first single-declaration)))))
    (when (find-symbol-in-local-table (name variable-info) symbol-tables)
      (with-simple-restart (continue "Ignore error")
		      (error 'semantic-condition :line (line (first single-declaration))
			     :description (format nil "Identifier ~A already defined."
						  (name variable-info)))))
    (list (unless (function-info-p variable-info)
	    (if (eq scope 'local)
		(list (list (symbolicate scope '-declaration)
			    variable-info)
		      (analyze-initializer (second single-declaration)
					   variable-info
					   symbol-tables
					   scope))
		(list (symbolicate scope '-declaration)
		      variable-info
		      (analyze-initializer (second single-declaration)
					   variable-info
					   symbol-tables
					   scope))))
	  variable-info)))

(defun transpose-pairs-list (pairs)
  "Transforms list of pairs into two list of single elements."
  (declare (type list pairs))
  (let ((result (list () ())))
    (dolist (element pairs (mapcar #'nreverse result))
      (push (first element) (first result))
      (push (second element) (second result)))))

(defun analyze-declaration-line (declaration symbol-tables scope)
  (transpose-pairs-list
   (mapcar (lambda (single-declaration)
	     (analyze-single-declaration single-declaration
					 (second declaration)
					 symbol-tables
					 scope))
	   (third declaration))))

(defun analyze-function (function symbol-tables)
  (flet ((add-params-to-symbol-tables (symbol-tables params)
	   (add-to-symbol-table (cons (make-hash-table) symbol-tables)
				params)))
    (let ((function-info (make-variable-info (value (second function)) (third function)
					     'global (line (first function)))))
      (list (list (list 'function-definition
			function-info)
		  (first
		   (analyze-block (fourth function)
				  (allocate-stack-variables
				   (add-params-to-symbol-tables
				    symbol-tables
				    (third (variable-type function-info)))
				   'argument)
				  (make-instance 'context
						 :enclosing-function function-info)))
		  (list 'function-end function-info))
	    function-info))))

(defun analyze-block (block symbol-tables context)
  (declare (type context context))
  (inc-blocks-count context)
  (let* ((block-symbol-tables (cons (make-hash-table) symbol-tables))
	 (result (list
		  (list '(begin)
			(mapcar (lambda (declaration-line)
				  (let ((analyzed-line (analyze-declaration-line declaration-line
										 block-symbol-tables
										 'local)))
				    (add-to-symbol-table block-symbol-tables (second analyzed-line))
				    (first analyzed-line)))
				(second block))
			(mapcar (lambda (instruction-line)
				  (let ((analyzed-line (analyze-instruction instruction-line
									    (allocate-stack-variables
									     block-symbol-tables
									     'local)
									    context)))
				    (setf block-symbol-tables (second analyzed-line))
				    (first analyzed-line)))
				(third block))
			'(end))
		  symbol-tables)))
    (dec-blocks-count context)
    result))

(defun analyze-instruction (instruction symbol-tables context)
  (case (value (first instruction))
    (new-block (analyze-block instruction symbol-tables context))
    (expression (let ((expression (analyze-expression (second instruction)
						      symbol-tables)))
		  (list (list (first expression)			      
			      '(clear-registers-counter))
			symbol-tables)))
    (if-else (analyze-if-else instruction symbol-tables context))
    (for-loop instruction)
    (while-loop instruction)
    (do-loop instruction)
    (return (analyze-return instruction symbol-tables context))
    (otherwise nil)))

(defun analyze-if-else (syntax-subtree symbol-tables context)
  (let ((else-label (genlabel))
	(end-label (genlabel)))
    (let* ((test-expression (analyze-expression (second syntax-subtree)
						symbol-tables))
	   (modified-symbol-tables (second test-expression))
	   (if-instruction (analyze-instruction (third syntax-subtree)
						modified-symbol-tables
						context))
	   (else-instruction (analyze-instruction (fourth syntax-subtree)
						  modified-symbol-tables
						  context)))
      (list (list (first test-expression)
		  '(test)
		  '(clear-registers-counter)
		  `(jump-if-eq ,else-label)
		  (first if-instruction)
		  `(jump ,end-label)
		  `(insert-label ,else-label)
		  (first else-instruction)
		  `(insert-label ,end-label))
	    modified-symbol-tables))))

(defun analyze-return (syntax-subtree symbol-tables context)
  (let ((expression (analyze-expression (second syntax-subtree)
					symbol-tables))
	(function-type (second (variable-type (enclosing-function context)))))
    (list (list (auto-convert-types (first expression) (third expression) function-type
				    (line (first syntax-subtree)))
		`(return ,(enclosing-function context) ,(enclosing-blocks-count context)))
	  symbol-tables)))

(defun unsupported (&rest a)
    (error "Unsupported construction ~S~%" a))

(defun genlabel ()
  (gensym ".L"))

(defun add-element (list a &optional (b nil b-supplied-p))
  (if b-supplied-p
      (append list (list b))
      (append list (list a))))

(defun skip-and-append (a b c)
  (declare (ignore b))
  (append a c))

(defun append-line (a b)
  (append a b))

(defun rcons (a b)
  (cons b a))

(defun skip-and-rcons (a b c)
  (declare (ignore b))
  (cons c a))

(defun to-pn (a op b)
  "Converts to polish notation"
  (list op a b))

(defun insert-after (new-element old-element list &key (test 'eql))
  "Return a list like list, but with new-element appearing after the
first occurence of old-element. If old-element does not appear in
list, then a list returning just new-element is returned."
  (if (endp list) (list new-element)
    (do ((head (list (first list)) (cons (first tail) head))
         (tail (rest list) (rest tail)))
        ((or (endp tail) (funcall test old-element (first head)))
         (nreconc head (cons new-element tail))))))
