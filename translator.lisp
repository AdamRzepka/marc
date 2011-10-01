(in-package :c-compiler)

(defstruct symbol-info
  name type function-p init-value args)

(defun add-element (list a &optional (b nil b-supplied-p))
  (if b-supplied-p
      (append list (list b))
      (append list (list a))))

(defun skip-and-append (a b c)
  (append a c))

(defun set-type (type symbol-list)
  (mapcar (lambda (symbol) 
	    (setf (symbol-info-type symbol) type)
	    symbol) 
	  symbol-list))
(defun set-function (symbol a b &optional 
		     (c nil c-supplied-p))
  (setf (symbol-info-function-p symbol) t)
  (setf (symbol-info-args symbol) 
	(if c-supplied-p
	    b
	    nil))
  symbol)

(defun gen-param-list ())
(defun gen-function-header (type pointer-declarator)
  )

(defun gen-stack-variables (block)
  )
(defun gen-function (type pointer-declarator block)
  `((|.type| |nazwa,| |%function|) ))
