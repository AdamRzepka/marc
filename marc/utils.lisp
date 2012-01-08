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

(defun find-symbol-in-tables (symbol symbol-tables)
  (cond
    ((null symbol-tables) nil)
    ((gethash symbol (first symbol-tables)))
    (t (find-symbol-in-tables symbol (rest symbol-tables)))))

(defun find-symbol-in-local-table (symbol symbol-tables)
  (if (null symbol-tables)
      nil
      (gethash symbol (first symbol-tables))))

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
