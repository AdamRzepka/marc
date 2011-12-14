(in-package :marc)


(define-guard-function analyze-semantic (syntax-subtree symbol-table context)
    (lambda (syntax-subtree a b)
      (declare (ignore a b))
      (search "literal" (string (first syntax-subtree)))))


(define-guard-function analyze-semantic (syntax-subtree symbol-table context)
    nil
  "Recursively checks for semantic errors (mainly type errors)
and adds explicit casts etc. Returns modified tree, modified symbol tables and type
of the expression.")

(defmethod semantic-control ((element (eql 'var-name)) children symbol-table context)
  )