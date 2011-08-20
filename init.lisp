(in-package "COMMON-LISP-USER")

(ql:quickload 'cl-lex)
(ql:quickload 'cl-ppcre)
(ql:quickload 'yacc)

(defpackage :c-compiler 
	    (:use :common-lisp :yacc :cl-lex :cl-ppcre)
	    (:export :parse-file))