(in-package "COMMON-LISP-USER")

(defpackage :marc 
	    (:use :common-lisp :yacc :cl-lex :cl-ppcre :alexandria)
	    (:export :parse-file :compile-c-file :main))