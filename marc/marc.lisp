;; (setf *load-verbose* nil)
;; (setf *load-print* nil)

;; (with-open-file (*standard-output* "/dev/null" :direction :output
;;                                    :if-exists :supersede)
;; 		(load "init.fas")
;; 		(load "translator.fas")
;; 		(load "parser.fas")
;; 		(load "utils.fas"))

(in-package :marc)

(defun compile-c-file (in out)
  (with-open-file 
   (istream in :direction :input)
   (with-open-file (ostream out :direction :output)
		   (format ostream 
			   (result-to-string 
			    (add-start-symbol
			     (cadr (parse-file in))))))))

(defparameter *args* (or 
		      #+SBCL (cdr sb-ext:*posix-argv*)  
		      #+LISPWORKS system:*line-arguments-list*
		      #+CMU extensions:*command-line-words*
		      #+CLISP cl-user::*args*
		      nil))

(case (length *args*)
  (0 (format t "Usage:~%carm SOURCE-FILE [OUTPUT-FILE]~%"))
  (1 (compile-c-file (car *args*) 
		     (replace (car *args*) ".s" 
			      :start1 (position #\. (car *args*) 
						:from-end t))))
  (2 (compile-c-file (car *args*) (cadr *args*))))

#|
(format t (result-to-string 
	   (cadr 
	    (with-open-file 
		  (stream "sample/sample.c" 
			  :direction :input)
		  (parse-file stream)))))|#

