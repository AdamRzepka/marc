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