(in-package :marc)

(defun line-to-string (line)
  (if line
      (let ((str-line (reduce (lambda (s1 s2) 
				(concatenate 'string 
					     s1 " " s2)) 
			      (mapcar #'string line))))
	(if (not (find #\: str-line))
	    (concatenate 'string "~4t" str-line)
	    str-line))
      "~%"))

(defun result-to-string (instructions)
  (concatenate 'string
	       (reduce (lambda (s1 s2) 
			 (concatenate 'string s1 "~%" s2)) 
		       (mapcar #'line-to-string instructions))
	       "~%"))

(defun transpose-pairs-list (pairs)
  "Transforms list of pairs into two list of single elements."
  (declare (type list pairs))
  (let ((result (list () ())))
    (dolist (element pairs (mapcar #'nreverse result))
      (push (first element) (first result))
      (push (second element) (second result)))))

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

(defun flatten-syntax-tree (syntax-tree)
  (labels ((visit-list (syntax-tree output-list)
	     (cond
	       ((null syntax-tree) output-list)
	       ((and (first syntax-tree) (symbolp (first syntax-tree)))
		(cons syntax-tree output-list))
	       (t (visit-list (rest syntax-tree)
			      (visit-list (first syntax-tree) output-list))))))
    (nreverse (visit-list syntax-tree nil))))

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
list, then a list containing just new-element is returned."
  (if (endp list) (list new-element)
    (do ((head (list (first list)) (cons (first tail) head))
         (tail (rest list) (rest tail)))
        ((or (endp tail) (funcall test old-element (first head)))
         (nreconc head (cons new-element tail))))))


(defmacro define-guard-function-family (name (&rest arguments))
  (let ((function-list (symbolicate '* name '-guard-function-implementations*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,function-list nil)
       (defun ,name ,arguments
	 (let (result)
	   (loop for element in ,function-list
	      while (null result)
	      do (when (funcall (first element) ,@arguments)			       
		   (setf result (funcall (second element) ,@arguments))))
	   (if result
	       result
	       (error "No function with appropriate guards found.")))))))

(defmacro define-guard-function (name (&rest arguments) (&rest guard) &body body)
"Defines function with guard. When the function is called, dispatcher seeks for function
implementation with appriopriate guard, in order they were defined. If no function
with appropriate guard was defined, error is raised."
;; TODO: &rest, &key etc. Make it more elegant :)
  (let ((function-list (symbolicate '* name '-guard-function-implementations*)))
    `(setf ,function-list
	   (nconc ,function-list (list (list (lambda ,arguments
					       (declare (ignorable ,@arguments))
					       ,@guard)
					     (lambda ,arguments
					       ,@body)))))))

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
	     (format stream "Line ~D: Cannot convert from ~A to ~A" (line c) (source-type c)
		     (destination-type c)))))

(define-condition unsupported-construction (error)
  ((construction :reader construction
		 :initarg :construction))
  (:report (lambda (c stream)
	     (format stream "~S not supported yet" (construction c)))))

(defun unsupported (&rest a)
  (with-simple-restart (continue "Ignore error")
    (error 'unsupported-construction :construction a)))

