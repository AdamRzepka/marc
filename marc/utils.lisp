(in-package :marc)

(defun build-syntax-tree (source)
  (declare (type string source))
  (create-c-lexer c-lexer)
  (parse-with-lexer (c-lexer source) *c-parser*))

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

(defun rcons (a b)
  (cons b a))