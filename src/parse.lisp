;;; **********************************************************************
;;; Copyright (C) 2006 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision$
;;; $Date$

(in-package cm)

;; scanning delimiters.

(defconstant +quote+ #\")		; "..." string 
(defconstant +kwote+ #\')		; '...' kwoted expr
(defconstant +comma+ #\,)                ; positional arg delimiter
(defconstant +pound+ #\#)                ; for bools etc
(defconstant +semic+ #\;)		; comment char
(defconstant +lbrace+ #\{)               ; {} list notation 
(defconstant +rbrace+ #\})
(defconstant +lbrack+ #\[)               ; unused for now
(defconstant +rbrack+ #\])
(defconstant +lparen+ #\()               ; () expr and arg grouping
(defconstant +rparen+ #\))

(defparameter +whites+ '(#\space #\tab #\return #\linefeed))

(defparameter +delims+
  (list* +quote+ +kwote+ +pound+ +comma+ +semic+ +lbrace+ +rbrace+
	 +lbrack+ +rbrack+
	 +lparen+ +rparen+ 
	 +whites+))

;; some delimiters are also tokens

(defparameter +tokdels+
  (list +kwote+ +comma+
	+lbrace+ +rbrace+ 
	+lbrack+ +rbrack+
	+lparen+ +rparen+))

;; keyword style is declared : ({:prefix | :suffix} . chars) 

(defparameter +kwstyle+ (list :suffix #\:)) ; let's try dylan

;; operator tokens. see also emit method for assignment-unit
;; (sal.lisp) and *op-weight* later in this file (which should be
;; combined with this at some point...)

(defparameter +operators+
  ;; each op is: (<token-class> <sal-name> <lisp-form>)
  '((:+ "+" +)
    (:- "-" -)
    (:* "*" *)
    (:/ "/" /)
    (:% "%" mod)
    (:^ "^" expt)
    (:= "=" =)   ; equality and assigment
    (:!= "!=" /=)
    (:< "<" <)
    (:> ">" >)
    (:<= "<=" <=) ; leq and assignment minimization
    (:>= ">=" >=) ; geq and assignment maximization
    (:~= "~=" equal) ; general equality
    (:+= "+=" +=) ; assignment increment-and-store
    (:*= "*=" *=) ; assignment multiply-and-store
    (:&= "&=" &=) ; assigment list collecting
    (:@= "@=" @=) ; assigment list prepending
    (:^= "^=" ^=) ; assigment list appending
    (:! "!" not)
    (:& "&" and)
    (:\| "|" or)
    ))

(defparameter +delimiters+
  '((:lp #\()
    (:rp #\))
    (:lc #\{)				; left curly
    (:rc #\})
    (:lb #\[)
    (:rb #\])
    (:co #\,)
    (:kw #\')				; kwote
    (nil #\")				; not token
    (nil #\#)
    (nil #\;)
    ))

;;; 
;;; parsing errors
;;;

(defclass sal-error ()
  ((type :accessor sal-error-type :initarg :type)
   (text :accessor sal-error-text :initarg :text)
   (line :accessor sal-error-line :initarg :line :initform nil)
   (start :accessor sal-error-start :initarg :start)))

(defmacro errexit (func input message pos)
  `(funcall ,func (make-instance 'sal-error :type ':read
		 :line ,input :text ,message :start ,pos)))

(defun pperror (x &optional (stream t))
  (let* ((source (sal-error-line x))
	 (llen (length source))
	 (beg nil)
	 (end nil))
    ;; isolate line containing error
    (do ((i (sal-error-start x) (- i 1))
	 (n nil))
	((or (< i 0) n)
	 (setq beg (or n 0)))
      (if (char= (elt source i) #\newline)
	  (setq n (+ i 1))))
    (do ((i (sal-error-start x) (+ i 1))
	 (n nil))
	((or (not (< i llen)) n)
	 (setq end (or n llen)))
      (if (char= (elt source i) #\newline)
	  (setq n i)))
    ;; print the error. include the specfic line of input containing
    ;; the error as well as aline below it marking the error position
    ;; with an arrow: ^
    (let* ((pos (- (sal-error-start x) beg))
	   (line (if (and (= beg 0) (= end llen)) 
		     source
		     (subseq source beg end)))
	   (mark (make-string (+ pos 1) :initial-element #\space)))
      (setf (elt mark pos) #\^)
      (format stream "~%>>> Sal ~(~A~) error: ~A.~%~%~A~%~A~%" 
	      (sal-error-type x)
	      (sal-error-text x)
	      line
	      mark)
      )))

;;;
;;; tokeinizing
;;;

(defun advance-white (str white start end)
  ;; skip "white" chars, where white can be a char, list of chars
  ;; or predicate test
  (do ((i start )
       (p nil))
      ((or p (if (< start end)
		 (not (< -1 i end))
		 (not (> i end -1))))
       (or p end))
    (cond ((consp white)
	   (unless (member (elt str i) white :test #'char=)
	     (setq p i)))
	  ((characterp white)
	   (unless (char= (elt str i) white)
	     (setq p i)))
	  ((functionp white)
	   (unless (funcall white (elt str i))
	     (setq p i))))
    (if (< start end)
	(incf i)
	(decf i))))

;(let ((str  "       1")) (advance-white str +whites+ 0 (length str)))
;(let ((str  "1         ")) (advance-white str +whites+ (1- (length str)) 0))
;(let ((str ""))  (advance-white str +whites+ 0 (length str)))
;(let ((str ""))  (advance-white str +whites+ (1- (length str)) 0 ))
;(let ((str "   "))  (advance-white str +whites+ 0 (length str)))
;(let ((str "   "))  (advance-white str +whites+ (1- (length str)) 0 ))

(defun search-delim (str delim start end)
  ;; find position of "delim" chars, where delim can be
  ;; a char, list of chars or predicate test
  (do ((i start (+ i 1))
       (p nil))
      ((or (not (< i end)) p)
       (or p end))
    (cond ((consp delim)
	   (if (member (elt str i) delim :test #'char=)
	       (setq p i)))
	  ((characterp delim)
	   (if (char= (elt str i) delim)
	       (setq p i)))
	  ((functionp delim)
	   (if (funcall delim (elt str i))
	       (setq p i))))))

(defun read-delimited (input &key (start 0) end (null-ok t)
		       (delimit +delims+) ; includes whites...
		       (white +whites+)
		       (skip-initial-white t)
		       (errorf #'pperror))
  ;; read a substrubg from input, optionally skipping any white chars
  ;; first. reading a comment delim equals end-of-line, input delim
  ;; reads whole input, pound reads next token. call errf if error
  ;;(FORMAT T "~%READ-DELIMITED: ~S :START ~S :END ~S" input start end)
  (let ((len (or end (length input))))
    (when skip-initial-white
      (setq start (advance-white input white start len)))
    (if (< start len)
	(let ((char (elt input start)))
	  (setq end (search-delim input delimit start len))
	  (if (equal start end)		; have a delimeter
	      (cond ((char= char +semic+)
		     ;; comment skips to next line and trys again...
		     (setq start (position-if (lambda (c)
						(char= c #\newline))
					      input :start (+ start 1)))
		     (if start
			 (read-delimited input :start (+ start 1) 
					 :end len
					 :delimit delimit :white white
					 :skip-initial-white t 
					 :errorf errorf)
			 (if null-ok (values ':eof end)
			     (errexit errorf input
				      "Unexpected end of input"
				      start))))
		    ((char= char +pound+)
		     ;; read # dispatch
		     (read-hash input delimit start len errorf))
		    ((char= char +quote+)
		     ;; input delim reads whole input
		     (read-string input delimit start len errorf))
		    ((char= char +kwote+)
		     (errexit errorf input "Illegal delimiter" start))
		    (t
		     ;; else delimiter is token
		     (values char (+ start 1))))
	      (values (subseq input start end) end)))
	(if null-ok 
	    (values ':eof end)
	    (errexit errorf input "Unexpected end of input"
		     start)))))

(defparameter hash-readers 
  '(( #\t read-bool)
    ( #\f read-bool)
;    ( #\a read-constructor)
    ( #\? read-iftok)
    ))

(defun read-hash (str delims pos len errf)
  (let ((e (+ pos 1)))
    (if (< e len)
	(let ((a (assoc (elt str e) hash-readers)))
	  (if (not a)
	      (errexit errf str "Illegal # character" e)
	      (funcall (cadr a) str delims e len errf)))
	(errexit errf str "Missing # character" pos))))

(defun read-iftok (str delims pos len errf)
  str delims len errf
  (values (make-instance 'token :type ':? :string "#?" :lisp 'if
			 :start (- pos 1))
	  (+ pos 1)))

; (read-string str start len)

(defun read-bool (str delims pos len errf)
  delims len errf
  (let ((end (search-delim str delims pos len)))
    (unless (= end (+ pos 1))
      (errexit errf str "Illegal # expression" (- pos 1)))
    (values (let ((t? (char= (elt str pos) #\t) ))
	      (make-instance 'token :type ':bool 
			   :string (if t? "#t" "#f")
			   :lisp t?
			   :start (- pos 1)))
	    (+ pos 1))))

(defun read-string (str delims pos len errf)
  delims
  (let* ((l nil)
	 (e nil))
    ;; find next non-escaped "
    (setq e (search-delim str
			  (lambda (c )
			    (if (char= c +quote+)
				(if (and l (char= l #\\))
				    (progn (setq l c) nil)
				    t)
				(progn (setq l c) nil)))
			  (+ pos 1)
			  len))
    (if (< e len)
	(values (make-instance 'token :type ':string
			       :start pos
			       ;; dont include actual #\" in string
			       :string (subseq str (+ pos 1) e))
		(+ e 1))
	(errexit errf str "Unmatched \"" pos))))
	   
; (read-delimited "123" )
; (read-delimited " 123")
; (read-delimited " ;foo")
; (read-delimited (format nil " ; foo ~% ; bar ~% 123"))
; (read-delimited "")
; (read-delimited "   ")
; (read-delimited " \"hi\" 123")
; (read-delimited "\"hi\"")
; (read-delimited "\"hi") ; => :err pos
; (read-delimited "#f")
; (read-delimited "#t")
; (read-delimited "#x")
; (read-delimited "  #")
; (read-delimited "#foo") ; => error

;;;
;;; #a[...] constructor methods
;;;

;; the constructors are defined in sal.lisp

(defparameter special-constructors
  '(("loop" parse-sequence-constructor )
    ;;("vector" parse-sequence-constructor)
    ("cycle" parse-pattern-constructor )
    ("heap" parse-pattern-constructor) 
    ("weighting" parse-pattern-constructor)
    ("rotation" parse-pattern-constructor)
    ("line" parse-pattern-constructor)
    ("palindrome" parse-pattern-constructor)
    ("markov" parse-pattern-constructor)
    ("graph" parse-pattern-constructor)
    ))

; (parse-instance-constructor (find-class 'midi) nil)

(defun get-constructor-method (s)
  ;; return a specialized construcutor or class
  ;; removed class support
  (do ((tail special-constructors (cdr tail))
       (func nil))
      ((or func (null tail))
       ;;(or func (find-class (read-from-string s) nil))
       func)
    (if (equal s (car (car tail))) 
	(setq func (cadr (car tail))))))

(defun read-constructor (input delims pos len errf)
  (let ((old (- pos 1)))		; start of #a[...]
    (incf pos)				; increment 1 past #\a
    (setq pos (advance-white input +whites+ pos len))
    (unless (and (< pos len) (char= (elt input pos) +lbrack+))
      (errexit errf input "#a missing ["  pos))
    (let* ((l 0)
	   (b nil)
	   (s nil)
	   (m nil)
	   (x nil)
	   (e (search-delim input
			    (lambda (c )
			      (cond ((char= c +lbrack+)
				     (setq l (+ l 1))
				     nil)
				    ((char= c +rbrack+)
				     (setq l (- l 1))
				     (if (= l 0) t nil))
				    (t nil)))
			    pos len)))
      (unless (< e len) (errexit errf input "#a missing ]" pos))
      (incf pos)			; pos now past [
      (setq s (subseq input pos e))	; s is constructor expr
      (setq l (length s))
      (multiple-value-setq (x b)
	(read-delimited s :end l :null-ok nil
			:delimit delims :skip-initial-white t
			:errorf errf))
      (setq m (get-constructor-method x))
      (unless m (errexit errf input "not a constructor type" pos))
      (let ((tok (if nil ;(typep m 'standard-class)
		     nil ;(parse-instance-constructor m (subseq s b) errf)
		     (funcall m s b errf))))
	(setf (token-start tok) old)
	(values tok (+ e 1))))))

; (let ((x "a[midi time 0]")) (read-constructor x +delims+ 0 (length x) #'pperror))
; (let ((x "a[kaka time 0]")) (read-constructor x +delims+ 0 (length x) #'pperror))
; (let ((x "a[list for i below 10 collect i]")) (read-constructor x +delims+ 0 (length x) #'pperror))
; (let ((x "a[cycle a b")) (read-constructor x +delims+ 0 (length x) #'pperror))
; (let ((x "a   [   cycle a b]")) (read-constructor x +delims+ 0 (length x) #'pperror))

(defun unbalanced-input (errf line toks par bra brk kwo)
  ;; search input for the starting position of some unbalanced
  ;; delimiter. toks is reversed list of tokens with something
  ;; unbalanced
  (let (char text targ  othr levl pos)
    (cond ((> par 0) (setq char #\( targ ':lp othr ':rp levl par))
	  ((< par 0) (setq char #\) targ ':rp othr ':lp levl 0))
	  ((> bra 0) (setq char #\{ targ ':lc othr ':rc levl bra))
	  ((< bra 0) (setq char #\} targ ':rc othr ':lc levl 0))
	  ((> brk 0) (setq char #\[ targ ':ls othr ':rs levl brk))
	  ((< brk 0) (setq char #\] targ ':rs othr ':ls levl 0))
	  ((> kwo 0) (setq char #\' targ ':kw othr ':kw levl kwo))
	  )
    (setq text (format nil "Unmatched '~c'" char))
    ;; search for start of error in token list
    (do ((n levl)
	 (tail toks (cdr tail)))
	((or (null tail) pos)
	 (or pos (error "Shouldn't! cant find op ~c in ~s."
			targ (reverse toks))))
      (if (eql (token-type (car tail)) targ)
	  (if (= n levl)
	      (setq pos (token-start (car tail)))
	      (decf n))
	  (if (eql (token-type (car tail)) othr)
	      (incf n))))    
    (errexit errf line text pos)))

;;;
;;; the lexer. right now it assumes input string is complete and ready
;;; to be processed as a valid expression.
;;;

(defun tokenize (str &key (start 0) (end (length str)) 
		 (white-space +whites+) (delimiters +delimiters+)
		 (operators +operators+) (null-ok t)
		 (keyword-style +kwstyle+) (reserved nil) 
		 (error-fn nil)
		 &allow-other-keys)
  ;; return zero or more tokens or a sal-error
  (let ((toks (list t))
	(errf (or error-fn 
		  (lambda (x) (pperror x) (return-from tokenize x)))))
    (do ((beg start)
	 (pos nil)
	 (all (append (loop for x in delimiters collect (cadr x))
		      white-space))
	 (par 0)
	 (bra 0)
	 (brk 0)
	 (kwo 0)
	 (tok nil)
	 (tail toks))
	((not (< beg end))
	 ;; since input is complete check parens levels.
	 (if (= 0 par bra brk kwo)
	     (if (null (cdr toks))
		 (if null-ok (list) (errexit errf str "Empty input" 0))
		 (cdr toks))
	     (unbalanced-input errf str (reverse (cdr toks)) 
			       par bra brk kwo)))
      (setq beg (advance-white str white-space beg end))
      (multiple-value-setq (tok pos)
	(read-delimited str :start beg :end end 
			:white white-space :delimit all
			:skip-initial-white nil :errorf errf))
      ;; tok now string, char (delimiter), :eof or token since input
      ;; is complete keep track of balancing delims
      (cond ((eql tok +lbrace+) (incf bra))
	    ((eql tok +rbrace+) (decf bra))
	    ((eql tok +lparen+) (incf par))
	    ((eql tok +rparen+) (decf par))
	    ((eql tok +lbrack+) (incf brk))
	    ((eql tok +rbrack+) (decf brk))
	    ((eql tok +kwote+) (setq kwo (mod (+ kwo 1) 2))))
      (cond ((eql tok ':eof)
	     (setq beg end))
	    
	    (t
	     (setq tok (classify-token tok beg str errf
				       delimiters operators
				       keyword-style reserved))
	     (setf (cdr tail) (list tok ))
	     (setf tail (cdr tail))
	     (setq beg pos))))))

;;;
;;; tokens
;;;

(defclass token ()
  ((type :accessor token-type :initarg :type :initform nil)
   (string :accessor token-string :initarg :string :initform "")
   (start :accessor token-start :initarg :start)
   ;; cache extra info here
   (info :accessor token-info :initarg :info :initform nil)
   ;; lisp representation of token if boundp
   (lisp :accessor token-lisp :initarg :lisp ) 
   ))

(defun token=? (tok op)
  (if (typep tok 'token)
      (equal (token-type tok) op)
      (eql tok op)))

(defmethod print-object ((obj token) stream)
  (let ((*print-case* ':downcase))
    (format stream "#<~s ~s>" 
	    (token-type obj) 
	    (token-string obj))))

;;;
;;; token classification. types not disjoint!
;;;

(defun classify-token (str pos input errf delims ops kstyle res)
  (let ((tok nil))
    (cond ((characterp str)
	   ;; normalize char delimiter tokens
	   (setq tok (delimiter-token? str pos input errf delims)))
	  ((stringp str)
	   (setq tok (or (number-token? str pos input errf)
			 (operator-token? str pos input errf ops)
			 (keyword-token? str pos input errf kstyle)
			 (class-token? str pos input errf res)
			 (reserved-token? str pos input errf res)
			 (symbol-token? str pos input errf)
			 ))
	   (unless tok
	     (errexit errf input "Not an expression or symbol"
		      pos)))
	  (t (setq tok str)))
    tok))

(defun delimiter-token? (str pos input errf delims)
  (let ((typ (find str delims :test #'char= :key #'cadr)))
    (if (and typ (car typ))
	(make-instance 'token :type (car typ) :string str
		       :start pos)
	(errexit errf input "Shouldn't: non-token delimiter" pos))))

(defun number-token? (str pos input errf)
  errf input
  (do ((i 0 (+ i 1))
       (len (length str))
       (c nil)
       (dot 0)
       (typ ':int)
       (sig 0)
       (sla 0)
       (dig 0)
       (non nil))
      ((or (not (< i len)) non)
       (if non nil
	   (if (> dig 0) 
	       (make-instance 'token :type typ :string str
			      :start pos)
	       nil)))
    (setq c (elt str i))
    (cond ((member c '(#\+ #\-))
	   (if (> i 0) (setq non t)
	       (incf sig)))
	  ((char= c #\.)
	   (if (> dot 0) (setq non t)
	       (if (> sla 0) (setq non t)
		   (incf dot))))
	  ((char= c #\/)
	   (setq typ ':ratio)
	   (if (> sla 0) (setq non t)
	       (if (= dig 0) (setq non t)
		   (if (> dot 0) (setq non t)
		       (if (= i (1- len)) (setq non t)
			   (incf sla))))))
	  ((digit-char-p c)
	   (incf dig)
	   (if (> dot 0) (setq typ ':float)))
	  (t (setq non t)))))

#||
(number-token? "" 0 "" #'pperror)
(number-token? " " 0 "" #'pperror)
(number-token? "a"  0 "" #'pperror)
(number-token? "1" 0 "" #'pperror)
(number-token? "+" 0 "" #'pperror)
(number-token? "-1/2" 0 "" #'pperror)
(number-token? "1." 0 "" #'pperror)
(number-token? "1.." 0 "" #'pperror)
(number-token? ".1." 0 "" #'pperror)
(number-token? ".1" 0 "" #'pperror)
(number-token? "-0.1" 0 "" #'pperror)
(number-token? "1/2" 0 "" #'pperror)
(number-token? "1//2" 0 "" #'pperror)
(number-token? "/12" 0 "" #'pperror)
(number-token? "12/" 0 "" #'pperror)
(number-token? "12/1" 0 "" #'pperror)
(number-token? "12./1" 0 "" #'pperror)
(number-token? "12/.1" 0 "" #'pperror)
||#

(defun operator-token? (str pos input errf ops)
  ;; tok can be string or char
  errf input
  (let ((typ (find str ops :test #'equal :key #'cadr)))
    (if typ 
	(make-instance 'token :type (car typ) :string str
		       :start pos :lisp (or (third typ)
					    (read-from-string str)))
	nil)))

(defun keyword-token? (tok pos input errf style)
  (let* ((tlen (length tok))
	 (keys (cdr style))
	 (klen (length keys)))
    (cond ((not (< klen tlen)) nil)
	  ((eql (car style) ':prefix)
	   (do ((i 0 (+ i 1))
		(x nil))
	       ((or (not (< i klen)) x)
		(if (not x)
		    (let ((sym (symbol-token? (subseq tok i)
					      pos input errf )))
		      (if sym (progn (setf (token-type sym) ':key) sym)
			  nil))
		    nil))
	     (unless (char= (elt tok i) (elt keys i))
	       (setq x t))))
	  ((eql (car style) ':suffix)
	   (do ((j (- tlen klen) (+ j 1))
		(i 0 (+ i 1))
		(x nil))
	       ((or (not (< i klen)) x)
		(if (not x)
		    (let ((sym (symbol-token? (subseq tok 0 (- tlen klen))
					      pos input errf )))
		      (if sym (progn (setf (token-type sym) ':key) sym)
			  nil))
		    nil))
	     (unless (char= (elt tok j) (elt keys i))
	       (setq x t)))))))

(defun class-token? (str pos input errf res)
  res
  (let ((a (elt str 0)))
    (if (char= a #\<)
	(let* ((l (length str))
	       (b (elt str (- l 1))))
	  (if (char= b #\>)
	      (let ((tok (symbol-token? (subseq str 1 (- l 1))
					pos input errf)))
		;; class token has <> removed!
		(if tok (progn (setf (token-type tok) ':class)
			       tok)
		    (errexit errf input "Not a class identifer"
			     pos)))
	      (errexit errf input "Not a class identifer"
		       pos)))
	nil)))

; (keyword-token? ":asd" '(:prefix #\:))
; (keyword-token? "asd" KSTYLE)
; (keyword-token? "asd:"  KSTYLE)
; (keyword-token? "123:"  KSTYLE)
; (keyword-token? ":foo" '(:prefix #\:))
; (keyword-token? "foo=" '(:suffix #\=))
; (keyword-token? "--foo" '(:prefix #\- #\-))
; (keyword-token? ":123" '(:suffix #\:))
; (keyword-token? "--asd" '(:prefix #\-)) ; ok since -asd is legal symbol

(defun reserved-token? (str pos input errf reserved)
  errf input
  (let ((typ (find str reserved :test #'equal :key #'cadr)))
    (if typ 
	(make-instance 'token :type (car typ) :string str
		       :start pos)
	nil)))

(defun symbol-token? (str pos input errf)
  ;; REMOVED: let's insist on at least one letter for sanity's sake
  ;; this needs to be cleaned up!!
  (do ((i 0 (+ i 1))
       (bad "Not an expression or symbol")
       (chr nil)
       (ltr 0)
       (ses nil)
       (dot nil)
       (pkg nil)
       (len (length str))
       (err nil))
      ((or (not (< i len)) err)
       (if (or (and ses (> len 1)) T ; (> ltr 0)
	       )
	   (let ((info ()))
	     (if pkg (push (cons ':pkg pkg) info))
	     ;; if a single dot with alphchars after its a slot.ref
	     (if (and dot (< 0 dot (1- len))
		      (= (count-if #'alpha-char-p str :start (+ dot 1))
			 (- len dot 1))
		      )
		 (push (cons ':slot dot) info))	     
	     (make-instance 'token :type ':id :string str
			    :info info :start pos))
	   nil))
    (setq chr (elt str i))
    (cond ((not (null ses))		; a self evaluating symbol
	   (if (char= chr #\:)
	       (errexit errf input
			(format nil "Too many colons in ~s" str)
			pos)))
	  ((alpha-char-p chr) (incf ltr))
	  ((member chr '(#\* #\+))
	   (if (< 0 i (- len 2))
	       (errexit errf input bad pos )))
	  ((char= chr #\/)
	   (errexit errf input bad pos ))
	  ((char= chr #\:)
	   (if (= i 0)			; keywords are ok!
	       (setq ses t)
	       (cond ((= ltr 0)
		      (errexit errf input bad pos ))
		     ((not pkg)
		      (setq pkg i))
		     (t (errexit errf input
				 (format nil "Too many colons in ~s" str)
				 pos))))
	   (setq ltr 0))
	  ((char= chr #\.)
	   (if (not dot) (setq dot i))
	   (setq ltr 0) ))))

; (let ((i "foo")) (symbol-token? i 0 i #'pperror))
; (let ((i "foo..bar")) (symbol-token? i 0 i #'pperror))
; (let ((i ".bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo.bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "bar.")) (symbol-token?  i 0 i #'pperror))
; (let ((i "1...")) (symbol-token?  i 0 i #'pperror))
; (let ((i "a1..." )) (symbol-token? i 0 i #'pperror))
; (let ((i  "a{b")) (symbol-token? i 0 i #'pperror))
; (let ((i "foo-bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "123-a")) (symbol-token?  i 0 i #'pperror))
; (let ((i "1a23-a")) (symbol-token?  i 0 i #'pperror))
; (let ((i "*foo*")) (symbol-token?  i 0 i #'pperror))
; (let ((i "+foo+")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo+bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo/bar")) (symbol-token?  i 0 i #'pperror))

; (let ((i ":bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "::bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo:bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "cl-user:bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "cl-user::bar")) (symbol-token?  i 0 i #'pperror))
; (tokenize "aaa + bbb \"asdasdd\" aaa(1,2,3)")
; (tokenize "aaa+bbb \"asdasdd\" aaa(1,2,3)")

;;;
;;; parse-unit
;;; 

(defclass parse-unit ()
  ((parsed :initform  nil :initarg :parsed :accessor parse-unit-parsed)
   (code :initform nil :initarg :code :accessor parse-unit-code )))

(defmethod print-object ((obj parse-unit) stream)
  (format stream "#<~(~A~): ~S>" (class-name (class-of obj))
	  (parse-unit-parsed obj)))

;;;
;;; grammers
;;;

(defmacro defgrammer (sym rules &rest args)
  `(defparameter ,sym
     (make-instance 'grammer :rules ,rules ,@args)))

(defclass grammer ()
  ((rules :initform nil :initarg :rules :accessor grammer-rules)
   (literals :initarg :literals :accessor grammer-literals)
   ))

(defmethod initialize-instance :after ((obj grammer) &rest inits)
  (declare (ignore inits))
  ;; each literal is (:name "name")
  (cond ((slot-boundp obj 'literals)
	 (setf (grammer-literals obj)
	       (loop for x in (grammer-literals obj)
		  if (consp x) collect x
		  else 
		  collect (list (string->keyword
				 (string-upcase (string x)))
				(string-downcase (string x))))))
	(t
	 (setf (grammer-literals obj)
	       (loop for x in (grammer-rules obj)
		  when (terminal-rule? x)
		  collect (list (car x)
				(string-downcase
				 (string (car x)))))))))

;;;
;;; parsing
;;;

(defvar *maxtok* 0)

(defun parse (grammer pat input &key junk-allowed trace null-ok
	      (error-fn nil)  (info ()))
  (let* ((rules (list))
	 (reserved (list))
	 (tokens (list))
	 (bool nil)
	 (expr nil)
	 (rest nil)
	 ;; error function is passed to tokenize and emit so
	 ;; subroutines can return from here as soon as an error is
	 ;; discovered
	 (errf (or error-fn
		   (lambda (e)
		     ;; e is a sal error emit doesnt have input line...
		     (unless (sal-error-line e)
		       (setf (sal-error-line e) input))
		     ;; will be a continuation in scheme
		     (RETURN-FROM parse (values nil e rest))))))
    (cond ((consp grammer)
	   ;; combine all the grammers into 1 big one
	   (dolist (g grammer)
	     (setq rules (append rules (grammer-rules g)))
	     (setq reserved (append reserved (grammer-literals g)))))
	  (t (setq rules (grammer-rules grammer))
	     (setq reserved (grammer-literals grammer))))
    (if (stringp input)
	(setq tokens (tokenize input :reserved reserved 
			       :error-fn errf :null-ok null-ok))
	(setq tokens input))
    (setq *maxtok* 0)
    (cond
      ((null tokens)  
       (if null-ok (setq bool t)))
      ((consp tokens)
       ;; call parser on tokens
       (when trace (format t "~&tokens: ~s" tokens))
       (multiple-value-setq (bool expr rest)
	 (parser rules pat tokens 0 trace errf))
       (if (and bool (or (not rest) junk-allowed)) ;success
	   (setq expr (emit expr
			    ;; top-level info is syntax to emit unless
			    ;; info passed in
			    (add-emit-info ':syntax ':cltl info)
			    ;; the error function to call
			    errf))
	   (errexit errf input "Invalid expression"
		    (or *maxtok* (token-start (first rest))))))
      (t (setq expr tokens)))
    (values bool expr rest)))

;; emit turns parsed tokens into lisp form.  the info parameter is
;; used by some subcalls to emit to pass information downto deeper
;; subcalls for deparsing.  errf a function to call if a parse error
;; is encountered. in ClTl it amounts to a throw back to the main
;; parse function. currently info can be:
;; (:syntax . {:cltl|:scheme} )
;; ({:function|:process} . name)
;; (:definition . {:function|:process})
;; ({:loop|:run} . t)

(defgeneric emit (x &optional info errf))

(defun add-emit-info (name data info)
  (cons (cons name data) info))

(defun get-emit-info (name info)
  (let ((x (assoc name info)))
    (and x (cdr x))))

(defmethod emit ((x t) &optional info errf)
  info errf
  x)

(defmethod emit ((x list) &optional info errf)
  (if (null x) (list)
      (cons (emit (car x) info errf)
	    (emit (cdr x) info errf))))

;;;
;;; parser

;;; the parser takes a rule set, a starting rule and a list of tokens
;;; and returns three values: a bool indicating sucess or failure, the
;;; parsed data and the remaining (unparsed) tokens. by default the
;;; starting rule must consume all tokens unless :junk-allowed is t.
;;; each rule is a list: ((<lh> . <rh> <parser>) ... ) where <lh> is a
;;; keyword rule name, <rh> is the rule's pattern to match against the
;;; tokens, and <parser> is an optional rule parser, a function that
;;; is passed the matching tokens and returns the "value" of the match
;;; (ususally the tokens organized into some sort of canoncical form.)
;;; if <rh> is NIL then the rule is a terminal, ie it only matches if
;;; the next token's type is eq to it.  otherwise <rh> is a keyword
;;; rule name or list of rule names where each name in the list must
;;; match for the rule to be true.  the <rh> pattern can also contain
;;; four "metarules":

;;;   (or {pattern}+) -- true if any pattern is true
;;;   (+  {pattern}+) -- true if one or more of pattern is true
;;;   (@  {pattern}+)  -- optional pattern, always true
;;;   (*  {pattern}+)  -- zero or more pattern, always true

(defun terminal-rule? (rule)
  (or (null (cdr rule)) (not (cadr rule))))

(defgeneric parser-match? (tok pat)) ; allow differnet representations

(defmethod parser-match? ((tok t) (pat t))
  (equal tok pat ))

(defmethod parser-match? ((tok token) (pat t))
  ;; a method for true tokens
  (equal (token-type tok) pat))

(defun parser (rules pat tokens level trace errf)
  ;; return true or false with remaining tokens
  (when trace
    (format t "~%parse[~d]: <~s> tokens=~s" level pat tokens))
  ;; keep track of maximum extent into tokens for error reporting
  (unless (null tokens)
    (let ((pos (token-start (car tokens))))
      (when (or (not *maxtok*) (> pos *maxtok*)) (setq *maxtok* pos))))
  (cond ((null pat) 
	 (error "Shouldn't! Called parser with no pattern."))
	((keywordp pat)
	 (let* ((rule (or (find pat rules :key #'car)
			  (error "Shouldn't! No rule for pattern ~s."
				 pat)))
		(term (terminal-rule? rule))
		(test (if term pat (cadr rule)))
		(func (if (and (consp (cdr rule)) (consp (cddr rule)))
			  (caddr rule))))

	   (if term			; terminal rule
	       (if (parser-match? (car tokens)  test)
		   ;; append match to sofar list
		   (values t (if func (funcall func (car tokens)
					       errf)
				 (car tokens))
			   (cdr tokens))
		   (values nil nil tokens))
	       ;; else recurse on right hand side of rule
	       (multiple-value-bind (a b c)
		   (parser rules test tokens (+ level 1) trace errf)
		 (if a (values a (if func (funcall func b errf) b) c)
		     (values nil nil c))))))
	((consp pat)
	 (cond ;((eql (car pat) 'E)
	       ;;(if (member (car tokens) (cdr pat) :test #'parser-match?)
               ;;  (values t ? (cdr tokens))
               ;;  (values nil ? tokens)))
	       ((eql (car pat) 'or)
		(let ((p nil)
		      (l nil)
		      (s nil))
		  ;; 'or' finds first true match. terms are
		  ;; independant and get tokens passed in
		  (loop for x in (cdr pat)
		     do (multiple-value-setq (p s l) 
			  (parser rules x tokens (+ level 1) trace errf))
		     until p)
		  (if p
		      (values p s l)
		      (values nil nil tokens))))
	       ((or (eql (car pat) '*)
		    (eql (car pat) '+))
		;; '*' and '+' iterate (cdr pat) over tokens while
		;; true. entire cdr must match to be true. * is zero or
		;; more, + is 1 or more
		(let ((p t)
		      (n 0)
		      (l tokens)
		      (r (list))
		      (s nil))
		  (loop while (and p (not (null l)))
		     do
		     (multiple-value-setq (p s l)
		       (parser rules (cdr pat) l (+ level 1) trace errf))
		       ;;(print (list :s-> s))
		     ;; if we had a match update tokens and save results
		     (if p (setq r (append r s) ;(cons s r)
				 tokens l n (+ n 1))))
		  (if (or (eql (car pat) '*) (> n 0))
		      (setq p t ; r (nreverse r)
			    )
		      (setq p nil r nil))
		  (values p r tokens)))
	       ((eql (car pat) '@)
		;; '@' optional pattern.
		(multiple-value-bind (p s l)
		    (parser rules (cdr pat) tokens (+ level 1) trace
			    errf)
		  (if p (values p s l)
		      (values t nil tokens))))
	       (t
		;; else process pattern left to right as an implicit
		;; 'and': terms consumed by a leftward pattern are
		;; not available for rightwards
		(let ((p nil)
		      (l tokens)
		      (r (list))
		      (s nil))
		  (loop while pat 
		     do (multiple-value-setq (p s l)
			  (parser rules (car pat) l (+ level 1) trace
				  errf))
		     until (not p) do (progn (pop pat) (push s r)))
		  ;; pat should be null else failed
		  (if pat (setq p nil r (list) l tokens)
		      (setq p t r (nreverse r)))
		  ;; if failure return tokens as they were else return
		  ;; only the tokens that remained after the parse
		  (values p r (if p l tokens))))))))

;;;
;;; Infix to prefix conversion.
;;;

(defparameter *op-weights*
  '(
    (:\| 1)
    (:& 2)
    (:! 3)
    (:= 4)
    (:!= 4)
    (:> 4)
    (:>= 4)
    (:< 4)
    (:<= 4)
    (:~= 4) ; general equality
    (:+ 5)
    (:- 5)
    (:% 5)
    (:* 6)
    (:/ 6)
    (:^ 7)))

(defun is-op? (x)
  ;; return op weight if x is operator
  (let ((o (find (if (typep x 'token) (token-type x) x)
		 *op-weights* :key #'car)))
    (and o (cadr o))))

(defun inf->pre (inf)
  ;; this does NOT rewrite subexpressions because parser applies rules
  ;; depth-first so subexprs are already processed
  (let (op lh rh w1)
    (if (consp inf)
	(do ()
	    ((null inf) lh)
	  (setq op (pop inf))		; look at each element of in
	  (setq w1 (is-op? op))
	  (cond ((numberp w1)		; found op
		 (do ((w2 nil)
		      (ok t)
		      (li (list)))
		     ((or (not inf) (not ok))
		      (setq rh (inf->pre (nreverse li)))
		      (setq lh (if lh (list op lh rh)
				   (list op rh nil))))
		   (setq w2 (is-op? (first inf)))
		   (if (and w2 (<= w2 w1))
		       (setq ok nil)
		       (push (pop inf) li))))
		(t
		 (setq lh op))))
	inf)))

; (inf->pre '(1 :+ 2 :+ 3 :* 4))
; (inf->pre '(1 :+ 2 :- 3 :- 4))
; (inf->pre '(1 :+ 2 :* 3))
; (inf->pre '(1 :+ 2 :+ 3 :/ 4 :/ 5 :* 4))
; (inf->pre '(1 :* 3 :+ 10))
; (inf->pre '(1 :< 3 :+ 10))
; (inf->pre '(1 :+ 2 :+ 3 :=  4 :/ 5 :* 4 ))
; (inf->pre '( 1 := 2 :& 3 := 4))
; (inf->pre '( 1 := 2 :& 3 := 4 :& 1 :+ 2 :< 7))
; (inf->pre '( a :\| :! b))
; (inf->pre '( a :\| b :& c :\| :! d :+ f))

;;; eof


