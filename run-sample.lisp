(use-package :c-compiler)

(defun reload-and-run ()
  (load "translator.lisp")
  (load "parser.lisp")
  (load "run-sample.lisp"))

(print (with-open-file (stream "sample/sample.c" :direction :input)
	 (parse-file stream)))