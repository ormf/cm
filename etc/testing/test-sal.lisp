;;; **********************************************************************
;;; Copyright (C) 2006 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision$
;;; $Date$

(in-package cm)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro saltest (pat str &optional res &rest args)
    `(let ((r (quote ,res))
	   a b c x)
       (multiple-value-setq (a b c)
	 (parse *sal-grammer* ,pat  ,str ,@args))
       c
       (cond ((not r)
	      (let ((*print-case* ':downcase))
		(pprint b))
	      (setq x t)
	      )
	     ((not a)
	      (if (typep b 'sal-error)
		  (if (eql r ':error)
		      (setq x t)
		      (setq x nil))
		  (setq x nil)))
	     ((equal b r)
	      (setq x t))
	     (t
	      (setq x nil)))
       (if (not x)
	   (let ((*print-case* ':downcase))
	     (format t "~&~&>>>>>>>> Test ~S ~S failed,~&         value:  ~S~&         result: ~S" 
		     ,pat ,str r b)
	     nil)
	   t))))

(saltest :list "{}" (list))
(saltest :list "{{}}" '(nil))
(saltest :list "{{}" :error)
(saltest :list "{}}" :error)
(saltest :list "{{{{{{}}}}}}" '(((((nil))))))
(saltest :list "{{{}{}}{{}{}}}" '((nil nil) (nil nil)))
(saltest :list "{1}" '(1))
(saltest :list "{1 {2 3}}" '(1 (2 3)))

(saltest :aref "a[1]" (elt a 1))
(saltest :aref "a[1,2]" (aref a 1 2))
(saltest :aref "a[random(10)]" (elt a (random 10)))

(saltest :sexpr "a" a)
(saltest :sexpr "a." a.)
(saltest :sexpr ".a" .a)
(saltest :sexpr "a.b" (slot-value a 'b))
(saltest :sexpr "a.b.c" a.b.c)
(saltest :sexpr "e+w" e+w)
(saltest :sexpr "->" ->)

(saltest :sexpr "foo" foo)
(saltest :funcall "foo()" (foo))
(saltest :funcall "foo ()" (foo))
(saltest :funcall "foo(1)" (foo 1))
(saltest :funcall "foo(1, 2)" (foo 1 2))
(saltest :funcall "foo(hi: 1)" (foo :hi 1))
(saltest :funcall "foo(hi: 1, ho: 33)" (foo :hi 1 :ho 33))
(saltest :funcall "foo(1,2,hi: 1, ho: 33)" (foo 1 2 :hi 1 :ho 33))
(saltest :funcall "foo(bar())" (foo (bar)))
(saltest :funcall "foo(bar(1))" (foo (bar 1)))

; (parse sexpr :ifexpr "?( #t, #t , #f)")
; (parse sexpr :ifexpr "?( #t, :foo)")
; (parse sexpr :ifexpr "?( 10 < a, :foo, 44 + random(100))")
; (parse sexpr :ifexpr "?( fii(a), :true)")



(saltest :funcall "foo(1, hi: bar(2), ho: 3)" (foo 1 :hi (bar 2) :ho 3))
(saltest :sexpr "foo(bar(baz(1),2),3)" (foo (bar (baz 1) 2) 3))
(saltest :funcall "foo(1,1/2)" (foo 1 1/2))
(saltest :funcall "foo(1, {1 2 3})" (foo 1 '(1 2 3)))
(saltest :funcall "foo(1,1/2, .1)" (foo 1 1/2 0.1))
(saltest :funcall "foo(1,.1,\"hiho\")" (foo 1 0.1 "hiho"))
(saltest :funcall "foo(hiho: 2)" (foo :hiho 2))
(saltest :funcall "foo(hi: 2, ho: .3)" (foo :hi 2 :ho 0.3))
(saltest :funcall "foo(hi: 2, ho: .3, hee: \"zuz\")" 
	 (foo :hi 2 :ho 0.3 :hee "zuz"))
(saltest :funcall "foo(1,2,hi: 2, ho: .3, hee: -1/3)" 
	 (foo 1 2 :hi 2 :ho 0.3 :hee -1/3))
(saltest :funcall "foo(1, 2.2, hi: 2/3, ho: \"zuz\")"
	 (foo 1 2.2 :hi 2/3 :ho "zuz"))
(saltest :funcall "foo(1,2,hi: 2, ho: .3, hee: -1/3)"
	 (foo 1 2 :hi 2 :ho 0.3 :hee -1/3))
(saltest :funcall "foo(1, bar(-99,88), 3)" (foo 1 (bar -99 88) 3))
(saltest :funcall "foo(1, bar(-99,baz(1000)), 3)" 
	 (foo 1 (bar -99 (baz 1000)) 3))
(saltest :sexpr "1 + 3 + 4" (+ (+ 1 3) 4))
(saltest :sexpr "- 1" (- 1))
(saltest :sexpr "1 + - 3 + 4" (+ (+ 1 (- 3)) 4))
(saltest :sexpr "1" 1)
(saltest :sexpr "1 + 2" (+ 1 2))
(saltest :sexpr "1 + foo(100)" (+ 1 (foo 100)))
(saltest :sexpr "foo(100) + 1" (+ (foo 100) 1))
(saltest :sexpr "foo(1 + 2)" (foo (+ 1 2)))
(saltest :sexpr "100 * interp(i, { 0 0 1 100})" 
	 (* 100 (interp i '(0 0 1 100))))
(saltest :sexpr "1 + (100 * interp(i, { 0 0 1 100}))"
	 (+ 1 (* 100 (interp i '(0 0 1 100)))))
(saltest :sexpr "foo(1 + 2, 3)" (foo (+ 1 2) 3))
(saltest :sexpr "foo(1, 2 + 3)" (foo 1 (+ 2 3)))
(saltest :sexpr "foo(1 * 2, 2 + 3, - 4)" (foo (* 1 2) (+ 2 3) (- 4)))
(saltest :sexpr "foo()" (foo))
(saltest :sexpr "foo(1) + bar(33)" (+ (foo 1) (bar 33)))
(saltest :sexpr "foo(1) + bar(33) * zuz(88)"
	 (+ (foo 1) (* (bar 33) (zuz 88))))
(saltest :sexpr "foo(1, 2 + bar(33))"
	 (foo 1 (+ 2 (bar 33))))
(saltest :sexpr "foo(1, 2 + bar(33 - xxx))" 
	 (foo 1 (+ 2 (bar (- 33 xxx)))))
(saltest :sexpr "bar(33 - xxx)" (bar (- 33 xxx)))
(saltest :sexpr "33 - xxx" (- 33 xxx))
(saltest :sexpr "- 33" (- 33))
(saltest :sexpr "1 - 33" (- 1 33))
(saltest :sexpr "1 - - 33" (- 1 (- 33)))
(saltest :sexpr "220 * expt(2, 1/12)" (* 220 (expt 2 1/12)))
(saltest :sexpr "times(2,3,4) / 3" (/ (times 2 3 4) 3))
(saltest :sexpr "(2 * 3 * 4) / 3" (/ (* (* 2 3) 4) 3))
(saltest :sexpr "foo / bar" (/ foo bar))
(saltest :sexpr "foo / bar(ziz * 5)" (/ foo (bar (* ziz 5))))
(saltest :sexpr "foo / bar(33)" (/ foo (bar 33)))
(saltest :sexpr "bar(33) / foo" (/ (bar 33) foo))
(saltest :sexpr "1 + 2 * 3" (+ 1 (* 2 3)))
(saltest :sexpr "(1 + 2) * 3" (* (+ 1 2) 3))
(saltest :sexpr "1 + foo(-2) * 3" (+ 1 (* (foo -2) 3)))
(saltest :sexpr "(1 + foo(-2)) * 3" (* (+ 1 (foo -2)) 3))
(saltest :sexpr "foo(-2) " (foo -2))
(saltest :sexpr "cl-user:zyz" common-lisp-user::zyz)
(saltest :sexpr "cl-zz:zyz" :error)
(saltest :sexpr "<midi>" #.<midi>)
(saltest :bindings "x" ((x nil)))
(saltest :bindings "x, y" ((x nil) (y nil)))
(saltest :bindings "x = 3" ((x 3)))
(saltest :bindings "x = 3, c, vv = x" ((x 3) (c nil) (vv x)))

;;;
;;; loop tests
;;;

(saltest :loop-statement "loop for i from 1 to 10 print i end"
	 (loop for i from 1 to 10 do (sal-print i)))

(saltest :loop-statement "loop for i from 1 print i end" 
	 (loop for i from 1 do (sal-print i)))

(saltest :loop-statement "loop for i to 2 print i end"
	 (loop for i to 2 do (sal-print i)))

(saltest :loop-statement "loop for i below 10 print i end" 
	 (loop for i below 10 do (sal-print i)))

(saltest :loop-statement "loop for i from 1 to 10 by 10 print i end" 
	 (loop for i from 1 to 10 by 10 do (sal-print i)))

(saltest :loop-statement 
	 "loop repeat 10 for x = 1 then 2 for i from 3 print i, x end" 
	 (loop repeat 10 for x = 1 then 2 for i from 3
	    do (sal-print i x)))

(saltest :loop-statement "loop for i from 0 + 1 to 10 + 1 print i end" 
	 (loop for i from (+ 0 1) to (+ 10 1) do (sal-print i)))

(saltest :loop-statement "loop for i below 10 + 1 ^ 3 print i end" 
	 (loop for i below (+ 10 (expt 1 3)) do (sal-print i)))


(saltest :loop-statement "loop with keys = {}, even = {}, sum = 0, lo = 128, hi = -1 
  repeat 10
  for k = random(128)
  set keys &= k, sum += k, lo <= k, hi >= k
  when even?(k) set even &= k
  finally
  begin
    with avr = sum / 10.0
    print list(keys,even,avr,lo,hi)
  end
end" 
	 (LOOP WITH KEYS = (LIST) WITH EVEN = (LIST) WITH SUM = 0 WITH LO = 128 WITH HI = -1 REPEAT 10 FOR K = (RANDOM 128) DO (PROGN (SETF KEYS (NCONC KEYS (LIST K))) (SETF SUM (+ SUM K)) (SETF LO (MIN LO K)) (SETF HI (MAX HI K))) (WHEN (EVEN? K) (SETF EVEN (NCONC EVEN (LIST K)))) FINALLY (LET* ((AVR (/ SUM 10.0))) (SAL-PRINT (LIST KEYS EVEN AVR LO HI))))
)

(saltest :loop-statement "loop
  with a, b = 0, c = 1, d = {}, e = {}, f = -1, g = 0
  for i below 5
    set a = i, b += 1, c *= 2, d &= i, e @= i, f <= i, g >= i
  finally print list(a,b,c,d,e,f,g)
end"
	 (LOOP WITH A = NIL WITH B = 0 WITH C = 1 WITH D = (LIST) WITH E = (LIST) WITH F = -1 WITH G = 0 FOR I BELOW 5 DO (PROGN (SETF A I) (SETF B (+ B 1)) (SETF C (* C 2)) (SETF D (NCONC D (LIST I))) (SETF E (CONS I E)) (SETF F (MIN F I)) (SETF G (MAX G I))) FINALLY (SAL-PRINT (LIST A B C D E F G)))
)

;;;
;;; operator tests
;;;

(saltest :sexpr "x % y" (mod x y))
(saltest :sexpr "y ^ z" (expt y z))
(saltest :sexpr "a = b | c < d | e >= f"
	 (or (or (= a b) (< c d)) (>= e f)))
(saltest :sexpr "x + y * z" (+ x (* y z)))
(saltest :sexpr "x - - y ^ z" (- x (expt (- y) z)))


; (saltest-pattern-constructor "cycle 1 2 3 4 5")
; (saltest-pattern-constructor "cycle {1 2} 3 4 5")
; (saltest-pattern-constructor "heap 1 2 3 for 4")
; (saltest-pattern-constructor "heap of notes c4 d e for 4")
; (saltest-pattern-constructor "weighting 1 2 3 for x")
; (saltest-pattern-constructor "weighting {1 weight 3} 2 for x")
; (saltest-pattern-constructor "markov {1 2 -> 3 4}")
; (saltest-pattern-constructor "markov {1 2 -> {3 .1} 4}")

(saltest :procdecl
	 "process foo ()
  run repeat 10
    print now()
  end"
	 (defprocess foo nil (process repeat 10 do (sal-print (now))))
	 :info '((:definition . :process)))

(saltest :fundecl "function foo () begin print 123 end"
	 (defun foo nil (progn (sal-print 123)) (values)))

(saltest :vardecl "variable foo = 1" (defparameter foo 1))

(saltest :procdecl "process foo () run repeat 10 print 123 end"  
	 (defprocess foo nil (process repeat 10 do (sal-print 123)))
	 :info '((:definition . :process)))

(saltest :fundecl "function row->tp (row)
  loop with a = first(row), new = {}
    for b in rest(row)
    set new &= tp(a,b), a = b
    finally
    begin
      set new &= tp(a,first(row))
      return new
    end
  end"
	 (DEFUN ROW->TP (ROW) (LOOP WITH A = (FIRST ROW) WITH NEW = (LIST) FOR B IN (REST ROW) DO (PROGN (SETF NEW (NCONC NEW (LIST (TP A B)))) (SETF A B)) FINALLY (PROGN (SETF NEW (NCONC NEW (LIST (TP A (FIRST ROW))))) (RETURN-FROM ROW->TP (VALUES NEW)))) (VALUES))
	 :info '((:definition . :function)))

;;;
;;; top level statements
;;;

(saltest :statement "begin
  open \"test.mid\"
  define process simp ()
    run repeat 10 output make(<midi>) wait .2
    end
  sprout simp()
end"
	 (PROGN (SAL-OPEN "test.mid") (SAL-DEFINE ':PROCESS '(DEFPROCESS SIMP NIL (PROCESS REPEAT 10 DO (OUTPUT (MAKE-INSTANCE 'MIDI)) (WAIT 0.2)))) (SAL-SPROUT (SIMP)))
	 )

(saltest :statement "sprout make(<midi>)"
	 (SAL-SPROUT (MAKE-INSTANCE 'MIDI)))

(saltest :statement "sprout list(make(<midi>), foo()), {10 20}"
	 (SAL-SPROUT (LIST (MAKE-INSTANCE 'MIDI) (FOO)) :AT '(10 20)))

(saltest :statement "begin with x = {1 2 3} print x, interpl(x[0], {0 0 100 1}) end"
	 (let* ((x (copy-list '(1 2 3)))) (sal-print x (interpl (elt x 0) '(0 0 100 1)))))
