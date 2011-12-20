(in-package :marc)


(defun get-literal-type (literal-description)
  (let ((s (string literal-description)))
    (intern (concatenate 'string "const-" (subseq s 0 (position #\- s :from-end t))))))

(defun deconst-type (type)
  (let* ((str-type (string type))
	 (const-position (search "const-" str-type))
	 (l-position (search "l-" str-type)))
    (cond ((eql const-position 0) (intern (subseq str-type (length "const-"))))
	  ((eql l-position 0) (intern (subseq str-type (length "l-"))))
	  (t type))))
