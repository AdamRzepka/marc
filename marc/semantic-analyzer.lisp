;;;; Semantic analyzer
(in-package :marc)

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
			   :initarg :enclosing-blocks-count
			   :initform 0))
  (:documentation "For the return, break and continue purposes."))

(defgeneric inc-blocks-count (context))

(defmethod inc-blocks-count ((context context))
  (incf (enclosing-blocks-count context)))

(defgeneric dec-blocks-count (context))

(defmethod dec-blocks-count ((context context))
  (decf (enclosing-blocks-count context)))

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
    (expression (let ((expression (analyze-expression (second instruction) symbol-tables)))
		  (list (list (first expression)			      
			      '(clear-registers-counter))
			symbol-tables)))
    (if-else (analyze-if-else instruction symbol-tables context))
    (for-loop (analyze-for-loop instruction symbol-tables context))
    (while-loop (analyze-while-loop instruction symbol-tables context))
    (do-loop instruction (analyze-do-loop instruction symbol-tables context))
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

(defun analyze-while-loop (syntax-subtree symbol-tables context)
  (let ((start-label (genlabel))
	(end-label (genlabel)))
    (list
     (list (list 'insert-label start-label)
	   (first (analyze-expression (second syntax-subtree) symbol-tables))
	   (list 'test)
	   (list 'jump-if-eq end-label)
	   (first (analyze-instruction (third syntax-subtree)
				       symbol-tables
				       (make-instance 'context
						      :enclosing-function
						      (enclosing-function context)
						      :enclosing-blocks-count
						      (enclosing-blocks-count context))))
	   (list 'jump start-label)
	   (list 'insert-label end-label))
     symbol-tables)))

(defun analyze-do-loop (syntax-subtree symbol-tables context)
  (let ((start-label (genlabel)))
    (list
     (list (list 'insert-label start-label)
	   (first (analyze-instruction (third syntax-subtree)
				       symbol-tables
				       (make-instance 'context
						      :enclosing-function
						      (enclosing-function context)
						      :enclosing-blocks-count
						      (enclosing-blocks-count context))))
	   (first (analyze-expression (second syntax-subtree) symbol-tables))
	   (list 'test)
	   (list 'jump-if-ne start-label))
     symbol-tables)))

(defun analyze-for-loop (syntax-subtree symbol-tables context)
  (let ((start-label (genlabel))
	(end-label (genlabel)))
    (unless (third syntax-subtree)
      (setf (third syntax-subtree)
	    (list 'int-literal 1)))
    (list
     (list (first (analyze-expression (second syntax-subtree) symbol-tables))
	   (list 'insert-label start-label)
	   (first (analyze-expression (third syntax-subtree) symbol-tables))
	   (list 'test)
	   (list 'jump-if-eq end-label)
	   (first (analyze-instruction (fifth syntax-subtree)
				       symbol-tables
				       (make-instance 'context
						      :enclosing-function
						      (enclosing-function context)
						      :enclosing-blocks-count
						      (enclosing-blocks-count context))))
	   (first (analyze-expression (fourth syntax-subtree) symbol-tables))
	   (list 'insert-label end-label))
     symbol-tables)))

(defun analyze-return (syntax-subtree symbol-tables context)
  (let ((expression (analyze-expression (second syntax-subtree)
					symbol-tables))
	(function-type (second (variable-type (enclosing-function context)))))
    (list (list (auto-convert-types (first expression) (third expression) function-type
				    (line (first syntax-subtree)))
		`(return ,(enclosing-function context) ,(enclosing-blocks-count context)))
	  symbol-tables)))


