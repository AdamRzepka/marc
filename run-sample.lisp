(use-package :c-compiler)

(print (with-open-file (stream "sample/sample.c" :direction :input)
	 (parse-file stream)))