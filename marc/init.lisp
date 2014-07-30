(in-package "COMMON-LISP-USER")

(defpackage :marc 
	    (:use :common-lisp :yacc :cl-lex :cl-ppcre :alexandria)
	    (:export :main :compile-c-file :generate-code :analyze-file :build-syntax-tree))