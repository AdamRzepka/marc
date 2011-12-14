(in-package :marc)

(defun line-to-string (line)
  (if line
      (let ((str-line (reduce (lambda (s1 s2) 
				(concatenate 'string 
					     s1 " " s2)) 
			      (mapcar #'string line))))
	(if (not (find #\: str-line))
	    (concatenate 'string "~t" str-line)
	    str-line))
      "~%"))

(defun result-to-string (instructions)
  (reduce (lambda (s1 s2) 
	    (concatenate 'string s1 "~%" s2)) 
	  (mapcar #'line-to-string instructions)))

(defun add-start-symbol (instructions)
  (append '((|.global| |_start|)
	    (|_start:| b |main|))
	instructions))

(defun find-symbol-in-tables (symbol symbol-tables)
  (cond
    ((null symbol-tables) nil)
    ((gethash symbol (first symbol-tables)))
    (t (find-symbol-in-tables (rest symbol-tables)))))


(defmacro define-guard-function (name (&rest arguments) guard &body body)
"Defines function with guard. When the function is called, dispatcher seeks for function
implementation with appriopriate guard, in order they were defined. If no function
with appropriate guard was defined, error is raised."
;; TODO: &rest, &key etc. Make it more elegant :)
  (let ((function-list (symbolicate '* name '-guard-function-implementations*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(unless (boundp function-list)
		`(defvar ,function-list nil))
       ,(unless (fboundp name)
		`(defun ,name ,arguments
		   (let (found)
		     (loop for element in ,function-list
			while (null found)
			do (when (funcall (first element) ,@arguments)			       
			     (setf found t)
			     (funcall (second element) ,@arguments)))
		     (unless found
		       (error "No function with appropriate guards found.")))))
       (setf ,function-list
	     (nconc ,function-list (list (list (if ,guard 
						   ,guard 
						   ,(lambda () t))
					       (lambda ,arguments
						 ,@body))))))))