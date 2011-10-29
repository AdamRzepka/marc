(in-package :marc)

(defun try-to-recover ()
  (invoke-restart 'try-to-recover))

(defun insert-token (symbol value)
  (invoke-restart 'insert-token symbol value))

(defun handle-parse-error (c)
  (declare (type yacc-parse-error c))
  (cond ((and (find (yacc-parse-error-terminal c) '(\; \{))
	      (find '\) (yacc-parse-error-expected-terminals c)))
	 (format t "Syntax error: expected )~%")
	 (insert-token '\) '\) ))
	(t (format t "Unexpected terminal ~S (value ~S) after terminal ~S~
                      (value ~S). ~@:_ Expected one of: ~S"
		   (yacc-parse-error-terminal c)
		   (yacc-parse-error-value c)
		   (yacc-parse-error-preceding-terminal c)
		   (yacc-parse-error-preceding-value c)
		   (yacc-parse-error-expected-terminals c)) 
	   (try-to-recover))))


