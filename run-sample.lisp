(use-package :c-compiler)

(with-open-file (stream "sample/sample.c" :direction :input)
  (parse-file stream))