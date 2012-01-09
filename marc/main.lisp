(in-package :marc)

(defun read-file (stream)
  (with-output-to-string (os)
    (let (c)
      (loop do (setf c (read-char stream nil))
	 while c
	 do (write-char c os)))))

(defun parse-file (stream)
  (create-c-lexer c-lexer)
  (handler-bind ((yacc-parse-error #'handle-parse-error
		  ;(lambda (c) (invoke-restart 'try-to-recover))
		  ))
    (parse-with-lexer (c-lexer (read-file stream)) *c-parser*) nil nil))

(defun compile-c-file (in out)
  (setf *errors-count* 0)
  (with-open-file (ostream out :direction :output :if-exists :supersede)
    (handler-bind ((lexer-error #'handle-lexer-error)
		   (yacc-parse-error #'handle-parse-error)
		   (undeclared-identifier #'handle-undeclared-identifier)
		   (type-convert-condition #'handle-type-convert-condition)
		   (semantic-condition #'handle-semantic-condition)
		   (unsupported-construction #'handle-unsupported-construction))
      (let ((syntax-tree (build-syntax-tree (read-file-into-string in)))
	    intermediate)
	(when (= *errors-count* 0)
	  (setf intermediate (analyze-file syntax-tree))
	  (when (= *errors-count* 0)
	       (format ostream 
		       (result-to-string
			(generate-code
			 intermediate)))))))))

;; (defparameter *args* (or 
;; 		      #+SBCL (cdr sb-ext:*posix-argv*)  
;; 		      #+LISPWORKS system:*line-arguments-list*
;; 		      #+CMU extensions:*command-line-words*
;; 		      #+CLISP cl-user::*args*
;; 		      nil))

(defun main ()
  (let ((args (or 
	       #+SBCL (cdr sb-ext:*posix-argv*)  
	       #+LISPWORKS system:*line-arguments-list*
	       #+CMU extensions:*command-line-words*
	       #+CLISP cl-user::*args*
	       nil)))
    (case (length args)
      (0 (format t "Usage:~%carm SOURCE-FILE [OUTPUT-FILE]~%"))
      (1 (compile-c-file (car args) 
			 (replace (car args) ".s" 
				  :start1 (position #\. (car args) 
						    :from-end t))))
      (2 (compile-c-file (car args) (cadr args))))))

(main)

