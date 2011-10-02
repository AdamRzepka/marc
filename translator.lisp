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

(defun gen-stack-variables (block &optional fun-args)
  block)
(defun gen-function (type symbol block)
  (setf (symbol-info-type symbol) type)
  (let ((fun-name (symbol-info-name symbol)))
    `(,symbol
      (|.global| ,fun-name)
      (|.type| ,fun-name |%function|)
      (,fun-name \:)
      ,(gen-stack-variables block))))

(defun to-onp (a op b)
  (append a b (list op)))

(defun swap (a b)
  (append b (list a)))

(defun gen-expression (onp-expr level)
  )