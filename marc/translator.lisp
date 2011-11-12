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
	 :initarg :type))
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

(defmethod print-object ((object variable-info) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~S" (name object) (variable-type object))))

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
		:initarg :description)
   (severity :type integer
	     :reader severity
	     :initarg :severity))
  (:report (lambda (c stream)
	     (format stream "Line ~D: ~A" (line c) (description c)))))

(defun add-to-symbol-table (symbol-table symbols)
  (declare (type hash-table symbol-table))
  (cond ((listp symbols)
	 (mapc (lambda (symbol) 
		 (setf (gethash (name symbol) symbol-table) symbol)) symbols))
	((typep symbols 'symbol-info)
	 (setf (gethash (name symbols) symbol-table) symbols))
	(t
	 (error 
	  'internal-error :description
	  (format nil "Something strange passed to add-to-symbol-table: ~S." symbols))))
  symbol-table)

(defun analyze-file (file)
  "FILE is a syntax tree created with build-syntax-tree"
  (let ((symbol-table (make-hash-table))
	result)
    (dolist (entry file (nreverse result))
      (multiple-value-bind (code symbols)
	  (case (first entry)
	    (declaration-line (analyze-declaration (rest entry) symbol-table))
	    (fun-definition (analyze-function (rest entry) symbol-table))
	    (t (unsupported (first entry))))
	(push code result)
	(add-to-symbol-table symbol-table symbols)))))

(defun make-variable-info (base-type variable)
  "Recursively adds *, [] or () to BASE-TYPE and creates variable-info instance"
  (if (listp variable)
      (case (first variable)
	(* (make-variable-info (list '* base-type) (second variable)))
	([] (make-variable-info (list '[] base-type (third variable))
				(second variable)))
	(|()| (make-variable-info (list '|()| base-type); TODO: add parameter info
				  (second variable))))
      (make-instance 'variable-info :name variable :type base-type)))

(defun generate-global-declaration (variable-info) ;TODO
  )

(defun analyze-single-declaration (single-declaration base-type)
  ;"Analyzes single declaration like a=3. SINGLE-DECLARATION would be ('a 3) for example."
  (let ((variable-info (make-variable-info base-type (first single-declaration))))
    (values (generate-global-declaration variable-info) variable-info)))

(defun multiple-value-mapcar (function list)
  (let (result1 result2)
    (dolist (element list (values (nreverse result1) (nreverse result2)))
      (multiple-value-bind (a b) (funcall function element)
	(push a result1)
	(push b result2)))))

(defun analyze-declaration (declaration symbol-table &optional (local nil))
  (multiple-value-mapcar (lambda (single-declaration)
			   (analyze-single-declaration single-declaration
						       (first declaration)))
	  (second declaration)))

(defun analyze-function (function symbol-table)
  'function)

(defun unsupported (&rest a)
    (error "Unsupported construction ~S~%" a))

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
