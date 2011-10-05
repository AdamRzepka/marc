(in-package :c-compiler)

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


(format t (result-to-string 
	   (cadr 
	    (with-open-file 
		  (stream "sample/sample.c" 
			  :direction :input)
		  (parse-file stream)))))