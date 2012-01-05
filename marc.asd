(in-package :asdf)

(defsystem "marc"
  :description "marc: Micro ARm Compiler"
  :version "0.01"
  :author "Adam Rzepka <adrzepka@gmail.com>"
  :license "GNU GPL"
  :depends-on (cl-ppcre cl-lex yacc alexandria)
  :components ((:module "marc"
			:serial t
			:components
			((:file "init")
			 (:file "translator")
			 (:file "parser")
			 (:file "utils")
			 (:file "type-control")
			 (:file "semantic-analyzer")
			 (:file "error-handler")
			 (:file "code-generator")
			 (:file "marc")))))