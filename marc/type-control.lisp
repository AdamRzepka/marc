(in-package :marc)

(defun get-literal-type (literal-description)
  (let ((s (string literal-description)))
    (intern (subseq s 0 (position #\- s :from-end t)))))

(defun get-variable-type (variable-name symbol-table)
  (let ((info (gethash (value variable-name) symbol-table)))
    ))

(defun get-type (expr symbol-table)
  (case (first expr)
    ((float-literal double-literal long-double-literal char-literal wchar-literal
		    int-literal unsigned-literal long-literal unsigned-long-literal
		    char*-literal wchar*-literal) (get-literal-type (first-expr)))
    (var-name )))