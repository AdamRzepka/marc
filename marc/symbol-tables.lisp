(in-package :marc)

;;; A few classes for describing symbol table entries

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

(defun find-symbol-in-tables (symbol symbol-tables)
  (cond
    ((null symbol-tables) nil)
    ((gethash symbol (first symbol-tables)))
    (t (find-symbol-in-tables symbol (rest symbol-tables)))))

(defun find-symbol-in-local-table (symbol symbol-tables)
  (if (null symbol-tables)
      nil
      (gethash symbol (first symbol-tables))))

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