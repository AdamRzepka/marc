(in-package :marc)

(defun generate-code (intermediate-code)
  (let ((stack-index 0))
    (mapcar (lambda (instruction)
	      (let ((result (generate-single-instruction instruction stack-index)))
		(setf stack-index (second result))
		(first result)))
	    intermediate-code)))

(defmacro intermediate-instruction-case (intermediate-instruction stack-index &rest all-instructions)
  `(case (first ,intermediate-instruction)
     ,@(loop for instruction in all-instructions
	    collecting `(,instruction (,(symbolicate 'generate- instruction)
					,intermediate-instruction
					,stack-index)))
     (otherwise (list '((|todo|)) ,stack-index))))

(defun generate-single-instruction (intermediate-instruction stack-index)
  (intermediate-instruction-case intermediate-instruction stack-index
				 global-declaration
				 function-definition
				 local-declaration
				 function-end
				 begin
				 end
				 load-local-word
				 load-global-word
				 load-local-address
				 load-argument-address
				 load-global-address
				 literal-word 
				 save-registers
				 load-registers
				 |()|
				 +-word
				 --word
				 =-word
				 ==-word
				 test
				 clear-registers-counter
				 insert-label
				 jump-if-ne
				 jump
				 return))

(defun generate-global-declaration (instruction stack-index)
  (let* ((name (name (second instruction)))
	 (type (variable-type (second instruction)))
	 (size (type-size type))
	 (target-type (symbol-downcase (target-type type))))
    `(,(remove-if #'null `((|.data|)
			   (|.global| ,name)
			   (|.type| ,name \, |%object|)
			   ,(if (> size 1)
				 `(|.align| ,(floor (/ size 2)))
				 `(|@|))
			   (|.size| ,name \, ,size)
			   (,(symbolicate name '\:))
			   (,(symbolicate '\. target-type) 0)))
      ,stack-index)))

(defun generate-function-definition (instruction stack-index)
  (let ((name (name (second instruction))))
    `(((|.text|)
       (|.global| ,name)
       (|.type| ,name \, |%function|)
       (|.align| 2)
       (,(symbolicate name '\:))
       (|stmfd| |sp!| \, |{r0-r10},lr|))
      ,stack-index)))

(defun generate-function-end (instruction stack-index)
  `(((,(symbolicate '|.L_| (name (second instruction)) '_end '\:))
     (|add| |sp| \, |sp| \, |#16|)
     (|ldmfd| |sp!| \, |{r4-r10},lr|))
    ,stack-index))

(defun generate-begin (instruction stack-index)
  (declare (ignore instruction))
  `(((|stmfd| |sp!| \, {|fp|})
     (|mov| |fp| \, |sp|))
    ,stack-index))

(defun generate-end (instruction stack-index)
  (declare (ignore instruction))
  `(((|mov| |sp| \, |fp|)
     (|ldmfd| |sp!| \, {|fp|}))
    ,stack-index))

(defun generate-local-declaration (instruction stack-index)
  `(((|sub| |sp| \, |sp| \, ,(symbolicate '\#
					  (write-to-string
					   (type-size
					    (variable-type (second
							    instruction)))))))
    ,stack-index))

(defun generate-load-global-word (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(name (second instruction))))
    ,(1+ stack-index)))

(defun generate-load-local-word (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(stack-address (address (second instruction)))))
    ,(1+ stack-index)))

(defun generate-load-local-address (instruction stack-index)
  `(((|sub| ,(register stack-index) \, |fp| \, ,(symbolicate '\#
							    (write-to-string
							     (- (address
								 (second instruction)))))))
    ,(1+ stack-index)))

(defun generate-load-argument-address (instruction stack-index)
  `(((|add| ,(register stack-index) \, |fp| \, ,(symbolicate '\#
							    (write-to-string
							     (address
							      (second instruction))))))
    ,(1+ stack-index)))

(defun generate-load-global-address (instruction stack-index)
  `(((|adr| ,(register stack-index) \, ,(name (second instruction))))
    ,(1+ stack-index)))

(defun generate-literal-word (instruction stack-index)
  `(((|mov| ,(register stack-index) \, ,(symbolicate '\# (second instruction))))
    ,(1+ stack-index)))

(defun generate-+-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|add| ,(register (- stack-index 2)) \, ,(register (- stack-index 2)) \,
	    ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate---word (instruction stack-index)
  (declare (ignore instruction))
  `(((|sub| ,(register (- stack-index 2)) \, ,(register (- stack-index 2)) \,
	    ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate-*-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|mul| ,(register stack-index) \, ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
     (|mov| ,(register (- stack-index 2)) \,  (register stack-index)))
    ,(1- stack-index)))

(defvar *saved-stack-index* nil)

(defun generate-save-registers (instruction stack-index)
  (declare (ignore instruction))
  (push stack-index *saved-stack-index*)
  `(((|stmfd| |sp!| \, |{r0-r4}|))
    ,0))

(defun generate-load-registers (instruction stack-index)
  (declare (ignore instruction))
  (setf stack-index (first *saved-stack-index*))
  (pop *saved-stack-index*)
  `(((|mov| ,(register stack-index) \, ,(register 0))
     ,(if (> stack-index 0)
	  `(|ldmdf| |sp!| \, |{r0-r4}|)
	  `(\@)))
    ,(1+ stack-index)))

(defun generate-|()| (instruction stack-index)
  `(((|bl| ,(name (second instruction))))
    ,stack-index))

(defun generate-=-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|str| ,(register (1- stack-index)) \, ,(symbolicate '\[ (register (- stack-index 2)) '\]))
     (|mov| ,(register (- stack-index 2)) \,  ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate-==-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
     (|moveq| ,(register (- stack-index 2)) \, |#1|)
     (|movne| ,(register (- stack-index 2)) \, |#0|))
    ,(1- stack-index)))

(defun generate-test (instruction stack-index)
  (declare (ignore instruction))
  `(((|cmp| ,(register (1- stack-index)) \, |#0|))
    ,stack-index))

(defun generate-clear-registers-counter (instruction stack-index)
  (declare (ignore instruction stack-index))
  `(((\@))
    0))

(defun generate-jump-if-ne (instruction stack-index)
  `(((|bne| ,(second instruction)))
    ,stack-index))

(defun generate-jump (instruction stack-index)
  `(((|b| ,(second instruction)))
    ,stack-index))

(defun generate-insert-label (instruction stack-index)
  `((,(symbolicate (second instruction) '\:))
    ,stack-index))

(defun generate-return (instruction stack-index)
  (let* ((leave-frames-code (loop repeat (third instruction)
			       appending (first (generate-end nil 0)))))
    `(,(append leave-frames-code `((|b| ,(symbolicate '|.L_| (name (second instruction))
						      '_end))))
       ,stack-index)))

(defun stack-address (offset)
  (symbolicate '|[fp, #| (write-to-string offset) '|]|))

(defun register (number)
  (symbolicate '|r| (write-to-string number)))

(defun symbol-downcase (symbol)
  (intern (string-downcase (string symbol))))