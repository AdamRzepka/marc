#This file is only stub, since all work is done by asdf

mkdir bin 2> /dev/null

bin/marc: marc/init.lisp marc/utils.lisp marc/symbol-tables.lisp marc/types.lisp marc/parser.lisp marc/expression-analyzer.lisp marc/semantic-analyzer.lisp marc/code-generator.lisp marc/main.lisp build/sbcl.lisp marc.asd
		sbcl --script build/sbcl.lisp