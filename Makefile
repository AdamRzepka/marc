#This file is only stub, since all work is done by asdf

bin/marc: marc/init.lisp marc/utils.lisp marc/symbol-tables.lisp marc/types.lisp marc/parser.lisp marc/expression-analyzer.lisp marc/semantic-analyzer.lisp marc/code-generator.lisp marc/main.lisp build/sbcl.lisp marc.asd
		mkdir -p bin 2> /dev/null;
		sbcl --script build/sbcl.lisp || clisp build/clisp.lisp || { echo "Not sbcl nor clisp found. Aborting."; exit 1; }