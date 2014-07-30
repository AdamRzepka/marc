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
	(|[]| (* (third type) (type-size (second type))))
	(t 4))
      (cdr (assoc type +builtin-type-sizes+))))

(defun get-literal-type (literal-description)
  (let* ((s (string literal-description))
	 (type (subseq s 0 (position #\- s :from-end t))))
    (if (find #\* type)
	(list '* (intern (subseq type 0 (1- (length type))) :marc))
	(intern type :marc))))

(defun deconst-type (type)
  (let* ((str-type (string type))
	 (const-position (search "const-" str-type))
	 (l-position (search "l-" str-type)))
    (cond ((eql const-position 0) (intern (subseq str-type (length "const-")) :marc))
	  ((eql l-position 0) (intern (subseq str-type (length "l-")) :marc))
	  (t type))))

(defun get-base-type (type)
  (if (constp type)
      (second type)
      type))

(defun make-const (type)
  (if (constp type)
      type
      (list 'const type)))

(defun constp (type)
  (and (listp type)
       (eq (first type) 'const)))

(defun types-equal (type1 type2)
  (cond
    ((eq (value type1) (value type2)) t)
    ((and (listp type1) (listp type2)
	  (eql (length type1) (length type2))
	  (reduce (lambda (a b) (and a b)) (mapcar #'types-equal type1 type2)))
     t)
    (t nil)))

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

(defun check-if-type-exists (syntax-subtree symbol-tables)
  (if (listp syntax-subtree)
      (check-if-type-exists (second syntax-subtree) symbol-tables)
      (unless (find (value syntax-subtree) +arithmetic-types+)
	(with-simple-restart (continue "Ignore error")
	  (error 'semantic-condition :line (line syntax-subtree)
		 :description (format nil "Unknown type ~S" (value syntax-subtree)))))))

