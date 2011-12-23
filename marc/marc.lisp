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
  (with-open-file 
   (istream in :direction :input)
    (with-open-file (ostream out :direction :output :if-exists :supersede)
		   (format ostream 
			   (result-to-string 
			    (add-start-symbol
			     (cadr (parse-file istream))))))))

(defparameter *args* (or 
		      #+SBCL (cdr sb-ext:*posix-argv*)  
		      #+LISPWORKS system:*line-arguments-list*
		      #+CMU extensions:*command-line-words*
		      #+CLISP cl-user::*args*
		      nil))

(defun main ()
  (case (length *args*)
    (0 (format t "Usage:~%carm SOURCE-FILE [OUTPUT-FILE]~%"))
    (1 (compile-c-file (car *args*) 
		       (replace (car *args*) ".s" 
				:start1 (position #\. (car *args*) 
						  :from-end t))))
    (2 (compile-c-file (car *args*) (cadr *args*)))))

(main)

