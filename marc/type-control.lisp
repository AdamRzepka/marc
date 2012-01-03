(in-package :marc)


(defun get-literal-type (literal-description)
  (let* ((s (string literal-description))
	 (type (subseq s 0 (position #\- s :from-end t))))
    (if (find #\* type)
	(list '* (intern (subseq type 0 (1- (length type)))))
	(intern type))))

(defun deconst-type (type)
  (let* ((str-type (string type))
	 (const-position (search "const-" str-type))
	 (l-position (search "l-" str-type)))
    (cond ((eql const-position 0) (intern (subseq str-type (length "const-"))))
	  ((eql l-position 0) (intern (subseq str-type (length "l-"))))
	  (t type))))

(defun get-base-type (type)
  (if (and (listp type) (eq (first type) 'const))
      (second type)
      type))

(defgeneric convert-type (source-type destination-type ))