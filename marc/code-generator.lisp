(in-package :marc)

(defun generate-code (intermediate-code)
  (let ((stack-index 0))
    ;; mapcan should be used here (or nconc instead of append), but there is a bug in
    ;; sbcl, which causes freeze
    (reduce #'append (mapcar (lambda (instruction)
			       (let ((result (generate-single-instruction instruction stack-index)))
				 (setf stack-index (second result))
				 (first result)))
			     intermediate-code))))

(defmacro intermediate-instruction-case (intermediate-instruction stack-index &rest all-instructions)
  `(case (first ,intermediate-instruction)
     ,@(loop for instruction in all-instructions
	    collecting `(,instruction (,(symbolicate 'generate- instruction)
					,intermediate-instruction
					,stack-index)))
     (otherwise (list '((|todo|)) ,stack-index))))

(defun generate-single-instruction (intermediate-instruction stack-index)
  (intermediate-instruction-case intermediate-instruction stack-index
				 start-symbol
				 global-declaration
				 function-definition
				 local-declaration
				 function-end
				 begin
				 end
				 load-word
				 load-float
				 load-short
				 load-byte
				 load-local-word
				 load-local-float
				 load-local-short
				 load-local-byte
				 load-global-word
				 load-global-float
				 load-global-short
				 load-global-byte
				 load-local-address
				 load-argument-address
				 load-global-address
				 literal-word
				 literal-float
				 literal-short
				 literal-byte
				 save-registers
				 load-registers
				 |()|
				 +-word
				 +-float
				 --word
				 --float
				 *-word
				 *-float
				 /-word
				 /-float
				 %-word
				 unary---word
				 unary---float
				 !-word
				 !-float
				 <<-word
				 >>-word
				 \&-word
				 \|-word
				 ^-word
				 ~-word
				 =-word
				 =-float
				 =-short
				 =-byte
				 convert-to-word
				 convert-to-signed
				 convert-to-float
				 ==-word
				 <-word
				 >-word
				 <=-word
				 >=-word
				 !=-word
				 ==-signed
				 <-signed
				 >-signed
				 <=-signed
				 >=-signed
				 !=-signed
				 ==-float
				 <-float
				 >-float
				 <=-float
				 >=-float
				 !=-float
				 test
				 clear-registers-counter
				 pop
				 copy
				 insert-label
				 jump-if-ne
				 jump-if-eq
				 jump
				 return))

(defun generate-start-symbol (instruction stack-index)
  (declare (ignore instruction))
  `(((|.text|)
     (|.global| |_start|)
     (|_start:| |b| |main|))
    ,stack-index))

(defun generate-global-declaration (instruction stack-index)
  (let* ((name (name (second instruction)))
	 (type (variable-type (second instruction)))
	 (size (type-size type))
	 (target-type (cond
			((and (listp type) (eq (second type) 'char)) '|asciz|)
			((> size 4)
			 (symbolicate '|space | (princ-to-string size) '\,))
			(t (symbol-downcase (target-type type))))))
    `(,(remove-if #'null `((|.data|)
			   (|.global| ,name)
			   (|.type| ,name \, |%object|)
			   ,(if (> size 1)
				 `(|.align| ,(princ-to-string (if (= size 2) 1 2)))
				 `(|@|))
			   (|.size| ,name \, ,(princ-to-string size))
			   (,(symbolicate name '\:))
			   (,(symbolicate '\. target-type)
			     ,(princ-to-string (if (third instruction)
						   (third instruction)
						   0)))))
      ,stack-index)))

(defun generate-function-definition (instruction stack-index)
  (declare (ignore stack-index))
  (let ((name (name (second instruction))))
    `(((|.text|)
       (|.global| ,name)
       (|.type| ,name \, |%function|)
       (|.align| |2|)
       (,(symbolicate name '\:))
       (|stmfd| |sp!| \, |{r0-r10,lr}|))
      ,0)))

(defun generate-function-end (instruction stack-index)
  `(((,(symbolicate '|.L_| (name (second instruction)) '_end '\:))
     (|add| |sp| \, |sp| \, |#16|)
     (|ldmfd| |sp!| \, |{r4-r10,pc}|))
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
					  (princ-to-string
					   (type-size
					    (variable-type (second
							    instruction)))))))
    ,stack-index))

(defun generate-load-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|ldr| ,(register (1- stack-index)) \,
	    ,(symbolicate '\[ (register (1- stack-index)) '\])))
    ,stack-index))

(defun generate-load-float (instruction stack-index)
  (generate-load-word instruction stack-index))

(defun generate-load-short (instruction stack-index)
  (declare (ignore instruction))
  `(((|ldrh| ,(register (1- stack-index)) \,
	    ,(symbolicate '\[ (register (1- stack-index)) '\])))
    ,stack-index))

(defun generate-load-byte (instruction stack-index)
  (declare (ignore instruction))
  `(((|ldrb| ,(register (1- stack-index)) \,
	    ,(symbolicate '\[ (register (1- stack-index)) '\])))
    ,stack-index))

(defun generate-load-global-word (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(symbolicate '= (name (second instruction))))
     (|ldr| ,(register stack-index) \, ,(symbolicate '\[ (register stack-index) '\])))
    ,(1+ stack-index)))

(defun generate-load-global-float (instruction stack-index)
  (generate-load-global-word instruction stack-index))

(defun generate-load-global-short (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(symbolicate '= (name (second instruction))))
     (|ldrh| ,(register stack-index) \, ,(symbolicate '\[ (register stack-index) '\])))
    ,(1+ stack-index)))

(defun generate-load-global-byte (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(symbolicate '= (name (second instruction))))
     (|ldrb| ,(register stack-index) \, ,(symbolicate '\[ (register stack-index) '\])))
    ,(1+ stack-index)))

(defun generate-load-local-word (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(stack-address (address (second instruction)))))
    ,(1+ stack-index)))

(defun generate-load-local-float (instruction stack-index)
  (generate-load-local-word instruction stack-index))

(defun generate-load-local-short (instruction stack-index)
  `(((|ldrh| ,(register stack-index) \, ,(stack-address (address (second instruction)))))
    ,(1+ stack-index)))

(defun generate-load-local-byte (instruction stack-index)
  `(((|ldrb| ,(register stack-index) \, ,(stack-address (address (second instruction)))))
    ,(1+ stack-index)))

(defun generate-load-local-address (instruction stack-index)
  `(((|sub| ,(register stack-index) \, |fp| \, ,(symbolicate '\#
							    (princ-to-string
							     (- (address
								 (second instruction)))))))
    ,(1+ stack-index)))

(defun generate-load-argument-address (instruction stack-index)
  `(((|add| ,(register stack-index) \, |fp| \, ,(symbolicate '\#
							    (princ-to-string
							     (address
							      (second instruction))))))
    ,(1+ stack-index)))

(defun generate-load-global-address (instruction stack-index)
  `(((|ldr| ,(register stack-index) \, ,(symbolicate '= (name (second instruction)))))
    ,(1+ stack-index)))

(defun generate-literal-word (instruction stack-index)
  `(((|mov| ,(register stack-index) \, ,(symbolicate '\# (princ-to-string (second instruction)))))
    ,(1+ stack-index)))

(defun generate-literal-float (instruction stack-index)
  (generate-literal-word instruction stack-index))

(defun generate-literal-short (instruction stack-index)
  (generate-literal-word instruction stack-index))

(defun generate-literal-byte (instruction stack-index)
  (generate-literal-word instruction stack-index))

(defun generate-+-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|add| ,(register (- stack-index 2)) \, ,(register (- stack-index 2)) \,
	    ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate-+-float (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__addsf3| stack-index 2))

(defun generate---word (instruction stack-index)
  (declare (ignore instruction))
  `(((|sub| ,(register (- stack-index 2)) \, ,(register (- stack-index 2)) \,
	    ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate---float (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__subsf3| stack-index 2))

(defun generate-*-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|mul| ,(register stack-index) \, ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
     (|mov| ,(register (- stack-index 2)) \,  ,(register stack-index)))
    ,(1- stack-index)))

(defun generate-*-float (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__mulsf3| stack-index 2))

(defun generate-/-word (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__divsi3| stack-index 2))

(defun generate-/-float (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__divsf3| stack-index 2))

(defun generate-%-word (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__modsi3| stack-index 2))

(defun generate-unary---word (instruction stack-index)
  (declare (ignore instruction))
  `(((|rsb| ,(register (1- stack-index)) \, ,(register (1- stack-index)) \, |#0|))
    ,stack-index))

(defun generate-unary---float (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__negsf2| stack-index 1))

(defun generate-!-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|cmp| ,(register (1- stack-index)) \, |#0|)
     (|moveq| ,(register (1- stack-index)) \, |#1|)
     (|movne| ,(register (1- stack-index)) \, |#0|))
    ,stack-index))

(defun generate-!-float (instruction stack-index)
  (declare (ignore instruction))
  `(,(append `((|mov| ,(register stack-index) \, |#0|))
	     (first (gas-function '|__eqsf2| (1+ stack-index) 2)))
     ,stack-index))

(defun generate-<<-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|mov| ,(register (- stack-index 2)) \, ,(register (- stack-index 2))
	    \, |lsl| ,(register (1- stack-index))))
    (1- stack-index)))

(defun generate->>-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|mov| ,(register (- stack-index 2)) \, ,(register (- stack-index 2))
	    \, |lsr| ,(register (1- stack-index))))
    (1- stack-index)))

(defun generate-&-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|and| ,(register (- stack-index 2)) \, ,(register (- stack-index 2))
	    \, ,(register (1- stack-index))))
    (1- stack-index)))

(defun generate-\|-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|orr| ,(register (- stack-index 2)) \, ,(register (- stack-index 2))
	    \, ,(register (1- stack-index))))
    (1- stack-index)))

(defun generate-^-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|eor| ,(register (- stack-index 2)) \, ,(register (- stack-index 2))
	    \, ,(register (1- stack-index))))
    (1- stack-index)))

(defun generate-~-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|mvn| ,(register (1- stack-index)) '\, ,(register (1- stack-index))))
    ,stack-index))

(defvar *saved-stack-index* nil)

(defun generate-save-registers (instruction stack-index)
  (declare (ignore instruction))
  (push stack-index *saved-stack-index*)
  `((,(cond
       ((> stack-index 1)
	`(|stmfd| |sp!| \, ,(symbolicate '|{r0-r| (princ-to-string (1- stack-index)) '|}|)))
       ((= stack-index 1)
	`(|stmfd| |sp!| \, |{r0}|))
       (t `(\@))))
    ,0))

(defun generate-load-registers (instruction stack-index)
  (declare (ignore instruction))
  (setf stack-index (first *saved-stack-index*))
  (pop *saved-stack-index*)
  `(((|mov| ,(register stack-index) \, ,(register 0))
     ,(cond
       ((> stack-index 1)
	`(|ldmfd| |sp!| \, ,(symbolicate '|{r0-r| (princ-to-string (1- stack-index)) '|}|)))
       ((= stack-index 1)
	`(|ldmfd| |sp!| \, |{r0}|))
       (t `(\@))))
    ,(1+ stack-index)))

(defun generate-|()| (instruction stack-index)
  `(((|bl| ,(name (second instruction))))
    ,stack-index))

(defun generate-=-word (instruction stack-index)
  (declare (ignore instruction))
  `(((|str| ,(register (1- stack-index)) \, ,(symbolicate '\[ (register (- stack-index 2)) '\]))
     (|mov| ,(register (- stack-index 2)) \,  ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate-=-float (instruction stack-index)
  (generate-=-word instruction stack-index))

(defun generate-=-short (instruction stack-index)
  (declare (ignore instruction))
  `(((|strh| ,(register (1- stack-index)) \, ,(symbolicate '\[ (register (- stack-index 2)) '\]))
     (|mov| ,(register (- stack-index 2)) \,  ,(register (1- stack-index))))
    ,(1- stack-index)))

(defun generate-=-byte (instruction stack-index)
  (declare (ignore instruction))
  `(((|strb| ,(register (1- stack-index)) \, ,(symbolicate '\[ (register (- stack-index 2)) '\]))
     (|mov| ,(register (- stack-index 2)) \,  ,(register (1- stack-index))))
    ,(1- stack-index)))

(defmacro integer-comparison-functions (&rest functions-desc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for (comparator type positive-cond negative-cond) in functions-desc
	    collecting `(defun ,(symbolicate 'generate- comparator '- type)
			    (instruction stack-index)
			  (declare (ignore instruction))
			  (list (list (list '|cmp| (register (- stack-index 2)) '\,
					    (register (1- stack-index)))
				      (list ',(symbolicate '|mov| positive-cond)
					    (register (- stack-index 2)) '\, '|#1|)
				      (list ',(symbolicate '|mov| negative-cond)
					    (register (- stack-index 2)) '\, '|#0|))
				(1- stack-index))))))

(integer-comparison-functions (== word |eq| |ne|) (< word |lo| |hs|) (> word |hi| |ls|)
			      (<= word |ls| |hi|) (>= word |hs| |lo|) (!= word |ne| |eq|)
			      (== signed |eq| |ne|) (< signed |lt| |ge|) (> signed |gt| |le|)
			      (<= signed |le| |gt|) (>= signed |ge| |lt|) (!= signed |ne| |eq|))

(defmacro float-comparison-functions (&rest functions-desc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for (comparator positive-cond negative-cond) in functions-desc
	    collecting `(defun ,(symbolicate 'generate- comparator '-float)
			    (instruction stack-index)
			  (declare (ignore instruction))
			  (list (append (first (gas-function '|__cmpsf2| stack-index 2))
					(list (list '|cmp| (register (- stack-index 2)) '\,
						    '|#0|)
					      (list ',(symbolicate '|mov| positive-cond)
						    (register (- stack-index 2)) '\, '|#1|)
					      (list ',(symbolicate '|mov| negative-cond)
						    (register (- stack-index 2)) '\, '|#0|)))
				(1- stack-index))))))

(float-comparison-functions (== |eq| |ne|) (< |lt| |ge|) (> |gt| |le|)
			    (<= |le| |gt|) (>= |ge| |lt|) (!= |ne| |eq|))

(defun generate-convert-to-float (instruction stack-index)
  (gas-function (if (eq (second instruction) 'signed)
		    '|__floatsisf| '|__floatunsisf|)
		stack-index 1))

(defun generate-convert-to-word (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__fixunssfsi| stack-index 1))

(defun generate-convert-to-signed (instruction stack-index)
  (declare (ignore instruction))
  (gas-function '|__fixsfsi| stack-index 1))

;; (defun generate-==-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|moveq| ,(register (- stack-index 2)) \, |#1|)
;;      (|movne| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate-<-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|movlo| ,(register (- stack-index 2)) \, |#1|)
;;      (|movhs| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate->-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|movhi| ,(register (- stack-index 2)) \, |#1|)
;;      (|movls| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate-<=-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|movls| ,(register (- stack-index 2)) \, |#1|)
;;      (|movhi| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate->=-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|movhs| ,(register (- stack-index 2)) \, |#1|)
;;      (|movlo| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate-!=-word (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(((|cmp| ,(register (- stack-index 2)) \, ,(register (1- stack-index)))
;;      (|movne| ,(register (- stack-index 2)) \, |#1|)
;;      (|moveq| ,(register (- stack-index 2)) \, |#0|))
;;     ,(1- stack-index)))

;; (defun generate-==-float (instruction stack-index)
;;   (declare (ignore instruction))
;;   `(,(append (first (gas-function |__cmpsf2| stack-index) 2)
;; 	     `((|cmp| ,(register (- stack-index 2)) \, |#0|)
;; 	      (|moveq| ,(register (- stack-index 2)) \, |#1|)
;; 	      (|movne| ,(register (- stack-index 2)) \, |#0|)))
;;     ,(1- stack-index)))

(defun generate-test (instruction stack-index)
  (declare (ignore instruction))
  `(((|cmp| ,(register (1- stack-index)) \, |#0|))
    ,stack-index))

(defun generate-clear-registers-counter (instruction stack-index)
  (declare (ignore instruction stack-index))
  `(((\@))
    0))

(defun generate-pop (instruction stack-index)
  (declare (ignore instruction))
  `(((\@))
    ,(1- stack-index)))

(defun generate-copy (instruction stack-index)
  (declare (ignore instruction))
  `(((|mov| ,(register (1- stack-index)) \, ,(register stack-index)))
    ,(1+ stack-index)))

(defun generate-jump-if-ne (instruction stack-index)
  `(((|bne| ,(second instruction)))
    ,stack-index))

(defun generate-jump-if-eq (instruction stack-index)
  `(((|beq| ,(second instruction)))
    ,stack-index))

(defun generate-jump (instruction stack-index)
  `(((|b| ,(second instruction)))
    ,stack-index))

(defun generate-insert-label (instruction stack-index)
  `(((,(symbolicate (second instruction) '\:)))
    ,stack-index))

(defun generate-return (instruction stack-index)
  (let* ((leave-frames-code (loop repeat (third instruction)
			       appending (first (generate-end nil 0)))))
    `(,(append leave-frames-code `((|b| ,(symbolicate '|.L_| (name (second instruction))
						      '_end))))
       ,stack-index)))

(defun stack-address (offset)
  (symbolicate '|[fp, #| (princ-to-string offset) '|]|))

(defun register (number)
  (symbolicate '|r| (princ-to-string number)))

(defun symbol-downcase (symbol)
  (intern (string-downcase (string symbol))))

(defun gas-function (function stack-index args-count)
  (list
   (append
    (first (generate-save-registers nil (if (eq args-count 2)
					    (- stack-index 2)
					    (1- stack-index))))
    (if (eq args-count 2)
	`((|mov| |r0| \, ,(register (- stack-index 2)))
	  (|mov| |r1| \, ,(register (1- stack-index)))
	  (|bl| ,function))
	`((|mov| |r0| \, ,(register (1- stack-index)))
	  (|bl| ,function)))
    (first (generate-load-registers nil 0)))
   (if (eq args-count 2)
       (1- stack-index)
       stack-index)))