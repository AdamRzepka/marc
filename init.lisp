(in-package "COMMON-LISP-USER")

;(ql:quickload 'cl-lex)
;(ql:quickload 'cl-ppcre)
;(ql:quickload 'yacc)
;(ql:quickload 'alexandria)

(defpackage :marc 
	    (:use :common-lisp :yacc :cl-lex :cl-ppcre :alexandria)
	    (:export :parse-file))