(use-package :c-compiler)

(defun reload-and-run ()
  (load "translator.lisp")
  (load "parser.lisp")
  (in-package :c-compiler)
  (load "run-sample.lisp")
  (in-package :common-lisp-user))

(print (with-open-file (stream "sample/sample.c" :direction :input)
	 (parse-file stream)))