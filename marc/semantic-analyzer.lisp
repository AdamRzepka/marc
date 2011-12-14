(in-package :marc)


(define-guard-function analyze-semantic (syntax-subtree symbol-tables context)
    (lambda (syntax-subtree a b)
      (declare (ignore a b))
      (search "literal" (string (first syntax-subtree))))
  (syntax-subtree symbol-tables (get-literal-type (first syntax-subtree))))

(define-guard-function analyze-semantic (syntax-subtree symbol-tables context)
    (lambda (syntax-subtree a b)
      (declare (ignore a b))
      (eq (first syntax-subtree) 'var-name)))



(define-guard-function analyze-semantic (syntax-subtree symbol-tables context)
    nil
  "Recursively checks for semantic errors (mainly type errors)
and adds explicit casts etc. Returns modified tree, modified symbol tables and type
of the expression.")

(defmethod semantic-control ((element (eql 'var-name)) children symbol-table context)
  )