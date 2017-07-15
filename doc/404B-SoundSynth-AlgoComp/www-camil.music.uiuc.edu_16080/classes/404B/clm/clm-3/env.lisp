;;; -*- syntax: common-lisp; package: clm; base: 10; mode: lisp -*-
;;;
;;; Envelope handlers
;;;
;;; I'm using "envelope" to mean a list of breakpoints, and "env" to mean the result of make-env -- the argument to env
;;;
;;;   envelope-length e                        number of break points in e
;;;   envelope-reverse e                       turn e backwards
;;;   envelope-concatenate es...               concatenate envelopes into one envelope
;;;   merge-breakpoints brkpts                 sort breakpoints and merge into envelope
;;;   envelope-funcall, envelope-apply, envelope-map apply or map some function to or across envelope(s)
;;;   envelope+ es...                          add all envelopes together
;;;   envelope* es...                          multiply envelopes together 
;;;   envelope-simplify e &optional ygrid xgrid simplify e
;;;   fft-envelope-simplify e &optional cutoff same but use fft filtering
;;;   envelope-repeat                          repeat/reflect envelope
;;;   envelope-exp                             exp segments
;;;   power-env, make-power-env                generator for extended envelopes (each segment has its own base)
;;;   exp-envelope, dB-envelope, make-dB-env, semitones-envelope, make-semitones-env, octaves-envelope, make-octaves-env
;;;   windowed-envelope                        return windowed portion of envelope
;;;   stretch-envelope                         attack and decay portions

(in-package :clm)

;;; List Interpolation -- assume a list of x y pairs (i.e. envelope breakpoints or synth tables)

(defun envelope-interp (x fn &optional (base 0)) ;order of args is like NTH
  (cond ((null fn) 0.0)			;no data -- return 0.0
	((or (<= x (first fn))		;we're sitting on x val (or if < we blew it)
	     (null (third fn)))		;or we're at the end of the list
	 (second fn))			;so return current y value
	((> (third fn) x)		;x <= next fn x axis value
	 (if (= (second fn) (fourth fn))
	     (second fn)		;y1=y0, so just return y0 (avoid endless calculations below)
	   (if (or (= 0 base) 
		   (= 1 base))		;linear envelope
	       (+ (second fn)		;y0+(x-x0)*(y1-y0)/(x1-x0)
		  (* (- x (first fn))
		     (/ (- (fourth fn) (second fn))
			(- (third fn) (first fn)))))
	     (+ (second fn)		;y0+[[[y1-y0] [-1.0 + [base ^ [x-x0]/[x1-x0]]] / [base-1]
		(* (/ (- (fourth fn) (second fn))
		      (- base 1.0))	;scaled y1-y0
		   (- (expt base (/ (- x (first fn)) 
				    (- (third fn) (first fn))))
		      1.0))))))
	(t (envelope-interp x (cddr fn) base))))	;go on looking for x segment

(defun envelope-last-x (env) (if (null env) 0.0 (nth (- (length env) 2) env)))

(defun envelope-length (e) (floor (length e) 2))

(defun max-envelope (env &optional (cur-max 0.0))
  (max cur-max (if env (loop for y in (cdr env) by #'cddr maximize y))))

(defun min-envelope (env &optional (cur-max 0.0))
  (max cur-max (if env (loop for y in (cdr env) by #'cddr minimize y))))

(defun scale-envelope (env scale &optional (offset 0.0))
  (if (endp env) nil
      (append (list (car env) (+ offset (* scale (cadr env))))
	      (scale-envelope (cddr env) scale offset))))
;;; version using nth and a loop was a little slower than this despite all the appends

(defun normalize-envelope (env &optional (new-max 1.0))
  (scale-envelope env (/ new-max (max-envelope env))))

(defun envelope-reverse (e)
  (let ((len (length e)))
    (if (or (= len 0) (= len 2))
	e
      (let ((xmax (nth (- len 2) e))
	    (ne nil))
	(loop for x in e by #'cddr and y in (cdr e) by #'cddr do
	  (push y ne)
	  (push (- xmax x) ne))
	ne))))

(defun merge-breakpoints (&rest breakpoints)
  (if breakpoints
      (apply #'append 
	     (sort (remove-duplicates breakpoints 
				      :test #'= 
				      :key #'car) 
		   #'< :key #'car))))

(defun envelope-concatenate (&rest envs) 
  ;; previous version buggy, fixed by Anders Vinjar 6-Jan-97
  (if (= (length envs) 1)
      (copy-list (first envs))
    (let ((xoff 0)
	  (ne nil))
      (loop for env in envs do
	(let ((firstx (car env)))
	  (loop for x in env by #'cddr and y in (cdr env) by #'cddr do
	    (push (+ xoff (- x firstx)) ne)
	    (push y ne)))
	(setf xoff (+ .01 (second ne))))
      (nreverse ne))))

#|
(envelope-concatenate
 '(0.0 0.0 1.0 1.0)
 '(1.0 1.0 4.0 0.5)
 '(40.0 0.5 47.0 0.0)
 '(7.0 0.5 10.0 0.0)
 '(0.0 0.5 13.0 0.0))
|#

(defun envelope-funcall (function envelope)
  (loop for x in envelope by #'cddr and y in (cdr envelope) by #'cddr
    collect (funcall function x y)))

(defun envelope-apply (function envelope)
  (loop for x in envelope by #'cddr and y in (cdr envelope) by #'cddr
    collect (apply function (list x y))))

(defun envelope-map (function envelope &rest more-envelopes)
  (let ((first-group (envelope-funcall function envelope))
	(second-group (loop for i in more-envelopes 
		       append (envelope-funcall function i))))
    (apply #'merge-breakpoints (append first-group second-group))))


;;; what we actually want here is to run through all the envelopes in parallel (as per mapcar, I think)
;;; using envelope-interp to get y at the current x and then call the function on that list of y's:

(defun funcall-breakpoint (function x y)
  (list x (funcall function x y)))

(defun map-across-envelopes (function &rest envelopes)
  (let ((new-e nil)
	(current-x (loop for x in envelopes minimize (car x)))
	(last-x (loop for x in envelopes maximize (envelope-last-x x))))
    (setf new-e 
      (funcall-breakpoint function 
			  current-x 
			  (loop for x in envelopes 
			   collect (envelope-interp current-x x))))
    (loop until (>= current-x last-x) do
      (setf current-x (loop for x in envelopes 
		       minimize (or (loop for y in x by #'cddr 
				     if (> y current-x) 
				     return y) 
				    last-x)))
      (setf new-e (append new-e 
			  (funcall-breakpoint function 
					      current-x 
					      (loop for x in envelopes 
					       collect (envelope-interp current-x x))))))
    new-e))
      
(defun envelope+ (&rest envelopes) 
  (apply #'map-across-envelopes 
	 (append (list #'(lambda (x y) 
			   (declare (ignore x)) 
			   (apply #'+ y))) 
		 envelopes)))

(defun envelope* (&rest envelopes) 
  (apply #'map-across-envelopes 
	 (append (list #'(lambda (x y) 
			   (declare (ignore x)) 
			   (apply #'* y))) 
		 envelopes)))

(defun envelope-max (&rest envelopes) 
  (apply #'map-across-envelopes 
	 (append (list #'(lambda (x y) 
			   (declare (ignore x)) 
			   (apply #'max y))) 
		 envelopes)))

#|
;;; and so on -- here are env/ and env- 
(defun envelope/ (&rest envelopes) 
  (apply #'map-across-envelopes 
	 (append (list #'(lambda (x y) 
			   (declare (ignore x)) 
			   (apply #'/ y))) 
		 envelopes)))

(defun envelope- (&rest envelopes) 
  (apply #'map-across-envelopes 
	 (append (list #'(lambda (x y) 
			   (declare (ignore x)) 
			   (apply #'- y))) 
		 envelopes)))
|#


(defun add-or-edit-breakpoint (fn x y)
  (if (null fn)
      (list x y)
    (let ((lim (length fn)))
      (loop for px in fn by #'cddr and
                i from 0 by 2 do
	(if (= px x)
	    (if (= i 0)
		(return-from add-or-edit-breakpoint (append (list x y) (subseq fn 2 lim)))
	      (if (>= (+ i 2) lim)
		  (return-from add-or-edit-breakpoint (append (subseq fn 0 i) (list x y)))
		(return-from add-or-edit-breakpoint (append (subseq fn 0 i) (list x y) (subseq fn (+ i 2) lim)))))
	  (if (> px x)
	      (if (= i 0)
		  (return-from add-or-edit-breakpoint (append (list x y) fn))
		(return-from add-or-edit-breakpoint (append (subseq fn 0 i) (list x y) (subseq fn i lim)))))))
      (append fn (list x y)))))

(defun remove-breakpoint (fn x)
  (if fn
      (let ((lim (length fn)))
	(if (= (first fn) x)
	    (cddr fn)
	  (if (= (envelope-last-x fn) x)
	      (subseq fn 0 (- lim 2))
	    (if (> lim 2)
		(progn
		  (loop for x0 in (cddr fn) by #'cddr and i from 2 by 2 do
		    (if (= x0 x)
			(return-from remove-breakpoint (append (subseq fn 0 i) (subseq fn (+ i 2) lim)))))
		  (error "can't find breakpoint at ~A in ~A" x fn))))))))

(defun point-on-line-p (px py qx qy tx ty)

  ;; is point tx ty on line defined by px py and qx qy --
  ;; nil if no, :before if on ray from p, :after if on ray from q, :within if between p and q
  ;; (these are looking at the "line" as a fat vector drawn on a grid)
  ;; taken from "Graphics Gems" by Glassner, code by A Paeth

  (if (or (= py qy ty) (= px qx tx))
      :within
    (if (< (abs (- (* (- qy py) (- tx px))
		   (* (- ty py) (- qx px))))
	   (max (abs (- qx px))
		(abs (- qy py))))
	(if (or (and (< qx px) (< px tx))
		(and (< qy py) (< py ty)))
	    :before
	  (if (or (and (< tx px) (< px qx))
		  (and (< ty py) (< py qy)))
	      :before
	    (if (or (and (< px qx) (< qx tx))
		    (and (< py qy) (< qy ty)))
		:after
	      (if (or (and (< tx qx) (< qx px))
		      (and (< ty qy) (< qy py)))
		  :after
		:within)))))))

(defun envelope-simplify (env &optional (ygrid 10) (xgrid 100))

  ;; grid = how fine a fluctuation we will allow.
  ;; the smaller the grid, the less likely a given bump will get through
  ;; original x and y values are not changed, just sometimes omitted.

  (if (and env
	   (> (envelope-length env) 2))
      (let* ((new-env (list (second env) (first env)))
	     (ymax (loop for y in (cdr env) by #'cddr maximize y))
	     (ymin (loop for y in (cdr env) by #'cddr minimize y))
	     (xmax (envelope-last-x env))
	     (xmin (first env)))
	(if (= ymin ymax)
	    (list xmin ymin xmax ymax)
	  (let* ((y-scl (/ ygrid (- ymax ymin)))
		 (x-scl (/ (or xgrid ygrid) (- xmax xmin)))
		 (px nil) (py nil)
		 (qx nil) (qy nil) 
		 (tx nil) (ty nil) 
		 (qtx nil) (qty nil))
	    (loop for ttx in env by #'cddr and
	              tty in (cdr env) by #'cddr do
	      (setf tx (round (* ttx x-scl)))
	      (setf ty (round (* tty y-scl)))
	      (if px
		  (if (not (point-on-line-p px py qx qy tx ty))
		      (progn
			(push qtx new-env)
			(push qty new-env)
			(setf px qx
			      py qy)))
		(setf px qx
		      py qy))
	      (setf qx tx
		    qy ty
		    qtx ttx
		    qty tty))
	    (push qtx new-env)
	    (push qty new-env)
	    (nreverse new-env))))
    (copy-seq env)))
	       


(defun reduce-amplitude-quantization-noise (e dur amp &optional (ramp-dur .5) (low-amp .005))
  ;; at very low amplitude (0.0 to .005), a ramp can cause an irritating sweeping buzz.
  (let ((new-e nil))
    (when (and e
	       (> (length e) 2)
	       (> dur ramp-dur)
	       (plusp amp))
      (let* ((last-x (envelope-last-x e))
	     (first-x (car e))
	     ;; for now assume e is 0..1 on y axis
	     (x-dur (* ramp-dur (/ (- last-x first-x) dur)))
	     (y-val (/ low-amp amp))
	     (x0 first-x)
	     (y0 (cadr e)))
	(loop for x1 in (cddr e) by #'cddr and y1 in (cdddr e) by #'cddr do
	  (if (and (> (- x1 x0) x-dur)	;segment is longer than ramp-dur
		   (/= y0 y1))		;only ramp gives this buzz (otherwise "normal" noise)
	      (if (< y0 y-val)		;left value is below low-amp
		  (if (< y0 y1)		;going up => make it go up sooner
		      (setf new-e (add-or-edit-breakpoint (or new-e (copy-seq e))
							  (+ x0 x-dur) (min y1 (max y-val (envelope-interp (+ x0 x-dur) e)))))
					;going down => delay down-slope as long as possible
		    (setf new-e (add-or-edit-breakpoint (or new-e (copy-seq e))
							  (- x1 x-dur) (min y0 (max y-val (envelope-interp (- x1 x-dur) e))))))
		(if (< y1 y-val)	;y0>y-val here, so we are going down and need not check for both y0<y-val and y1<y-val
		    (setf new-e (add-or-edit-breakpoint (or new-e (copy-seq e))
							(- x1 x-dur) (max y-val (envelope-interp (- x1 x-dur) e)))))))
	  (setf x0 x1)
	  (setf y0 y1))))
    (or new-e e)))


(defun meld-envelopes (e0 e1)
  (if (not e0) 
      e1
    (if (not e1)
	e0
      (let* ((e0-last-x (envelope-last-x e0))
	     (x-scl (/ 1.0 e0-last-x)))
	(map-across-envelopes #'(lambda (x y)
				  (if (>= x e0-last-x)
				      (cadr y)
				    (+ (car y) (* (- (cadr y) (car y))
						  (* x x-scl)))))
			      e0 e1)))))


;;; fft-envelope-simplify uses fft filtering to smooth an envelope -- aimed at Sansy envelopes -- there
;;; are lots of variations on this idea depending on the application.  In "Numerical Recipes" they
;;; remove the linear trend of the data, but I have found that causes confusion when the underlying
;;; envelope is really a randomly jittered sine wave without any true linear trend.  Also, here I
;;; believe the min and max values and rescale the result to match the ambitus of the input -- 
;;; cave canem or whatever.

(defun fft-envelope-simplify (e &optional cutoff)
  (let* ((min-step (loop for x0 in e by #'cddr and 
		             x1 in (cddr e) by #'cddr 
		    minimize (- x1 x0)))
	 (x-extent (- (envelope-last-x e) (first e)))
	 (x0 (first e))
	 (pts (floor (/ x-extent min-step)))
	 (n (expt 2 (ceiling (log (1+ pts) 2.0))))
	 (cut-n (floor (* n (or cutoff .5))))
	 (datar (make-double-array n))
	 (datai (make-double-array n)))
    (loop for x from x0 by min-step and i from 0 to pts do
      (setf (aref datar i) (envelope-interp x e)))
    (let* ((min-y0 (loop for i from 0 to pts minimize (aref datar i)))
	   (max-y0 (loop for i from 0 to pts maximize (aref datar i)))
	   (midpt (* .5 (+ min-y0 max-y0))))
      (loop for i from 0 to pts do
	(decf (aref datar i) midpt))
      (fft datar datai n 1)    
      (loop for i from cut-n below n do
	(setf (aref datar i) (double 0.0))
	(setf (aref datai i) (double 0.0)))
      (fft datar datai n -1)
      (let* ((min-y1 (loop for i from 0 to pts minimize (aref datar i)))
	     (max-y1 (loop for i from 0 to pts maximize (aref datar i)))
	     (inv-n (/ (- max-y0 min-y0) (- max-y1 min-y1)))
	     (y-off (- (* inv-n min-y1) min-y0)))
	(loop for i from 0 to pts and x from 0.0 by min-step 
	 collect x 
	 collect (- (* inv-n (aref datar i)) y-off))))))



(defun lastx (env)
  (nth (- (length env) 2) env))

(defun envelope-there-and-back-again (env)
  (let* ((x-max (lastx env))
	 (new-env (reverse env))
	 (rev-env (reverse env)))
    (loop for x in (cdddr rev-env) by #'cddr and y in (cddr rev-env) by #'cddr do
      (push (+ x-max (- x-max x)) new-env)
      (push y new-env))
    (nreverse new-env)))

(defun x-norm (env xmax)
  ;; change x axis values so that they run to xmax
  (let ((scl (/ xmax (lastx env))))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
      collect (* x scl) collect y)))

;;; repeat-envelope will repeat an envelope the number of times specified by its second argument.
;;; Hence if you specify (repeat-envelope '(0 0 100 1) 2) the result will be (0 0 100 1 101 0 201 1).
;;; Because the final y value was different from the first y value, a quick ramp will be 
;;; inserted between repeats.  You can have every other repeat be a reflection of the given 
;;; envelope by setting the optional second argument to t.  In that case,
;;; (repeat-envelope '(0 0 100 1) 2 t) the result will be (0 0 100 1 200 0).  If 
;;; you want the original x axis limits respected by the resultant envelope, set
;;; the third argument ("x-normalized") to t.

(defun envelope-repeat (ur-env ur-num-times &optional reflected x-normalized)
  (let* ((env (if reflected (envelope-there-and-back-again ur-env) ur-env))
	 (num-times (if reflected (floor ur-num-times 2) ur-num-times))
	 (x-max (lastx env))
	 (first-y (second env))
	 (x (first env))
	 (first-y-is-last-y (= first-y (first (last env))))
	 (offset (/ x-max 100.0))
	 (dxs (loop for x1 in (cddr env) by #'cddr and x0 in env by #'cddr collect (- x1 x0)))
	 (new-env (list first-y x))
	 (repeating nil))
    (loop for i from 1 to num-times do
      (when (and repeating (not first-y-is-last-y))
	(push (incf x offset) new-env)
	(push first-y new-env))
      (loop for dx in dxs and y in (cdddr env) by #'cddr do
	(push (incf x dx) new-env)
	(push y new-env))
      (setf repeating t))
    (when (and reflected (oddp ur-num-times))
      (loop for xx in (cddr ur-env) by #'cddr and dx in dxs and y in (cdddr env) by #'cddr do
	(push (incf x dx) new-env)
	(push y new-env)))
    (if x-normalized
	(x-norm (nreverse new-env) (lastx ur-env))
      (nreverse new-env))))





;;; by Anders Vinjar:
;;;
;;; envelope-exp can be used to create exponential segments to include in
;;; envelopes.  Given 2 or more breakpoints, it approximates the
;;; curve between them using 'xgrid linesegments and 'power as the
;;; exponent. 
;;; 
;;; env is a list of x-y-breakpoint-pairs,
;;; power applies to whole envelope,
;;; xgrid is how fine a solution to sample our new envelope with.

(defun envelope-exp (env &optional (power 1.0) (xgrid 100))
  (let* ((min (min-envelope env))
	 (largest-diff (- (max-envelope env) min))
	 (x-min (car env))
	 (x-max #-cltl2 (nth (- (length env) 2) env) #+cltl2 (car (last env 2))))
    (loop for x from x-min to x-max by (/ (- x-max x-min) xgrid)
      for y = (envelope-interp x env)
      collect x
      collect (if (zerop largest-diff)
		  y
		(+ min
		   (* largest-diff
		      (expt (/ (- y min) largest-diff) power)))))))

#|
(envelope-concatenate
 (envelope-exp '(0.0 0.0 1.0 1.0))
 (envelope-exp '(1.0 1.0 4.0 0.5))
 (envelope-exp '(40.0 0.5 47.0 0.0) 1.5 3)
 (envelope-exp '(7.0 0.5 10.0 0.0) 2.5)
 (envelope-exp '(0.0 0.5 13.0 0.0) .5))

(envelope-concatenate (envelope-exp '(0.0 400.0 1.0 0.0) 2.0 10.0)
		 (envelope-exp '(1.0 0.0 4.0 70.0) 2.0 50.0)
		 (envelope-exp '(0.0 70.0 3.0 0.0) 2.0 10.0))
|#


;;; extension of env to provide individual base on each segment (include 1 and 0 => linear and step)
;;; (make-power-env (envelope (scaler 1.0) (offset 0.0) duration)
;;;    returns a penv struct containing an array of envelopes
;;;    where the envelope is a sequence of triples [x y base]

;;; (7-17-04): this code was much prettier in CLM-2, but in CLM-3
;;;   def-struct can't handle generator fields
(defstruct penv envs total-envs current-env current-pass)

(defmacro power-env (envs total-envs current-env current-pass)
  `(let* ((val (env (aref ,envs ,current-env))))
    (decf ,current-pass)
    (when (zerop ,current-pass)
      (when (< ,current-env (1- ,total-envs))
	(incf ,current-env)
	(setf ,current-pass (mus-length (aref ,envs ,current-env)))))
    val))

(defun make-power-env (&key envelope (scaler 1.0) (offset 0.0) duration)
  (let* ((len (1- (floor (length envelope) 3)))
	 (pe (make-penv :envs (make-array len :element-type 'seg)
			:total-envs len
			:current-env 0
			:current-pass 0))
	 (xext (- (nth (- (length envelope) 3) envelope) (first envelope))))
    (loop for i from 0 below len and
	      x0 in envelope by #'cdddr and y0 in (cdr envelope) by #'cdddr and base in (cddr envelope) by #'cdddr and
	      x1 in (cdddr envelope) by #'cdddr and y1 in (cddddr envelope) by #'cdddr do
      (setf (aref (penv-envs pe) i)
	    (make-env :envelope (list 0.0 y0 1.0 y1)
		      :base base
		      :scaler scaler
		      :offset offset
		      :duration (* duration (/ (- x1 x0) xext)))))
    (setf (penv-current-pass pe) (mus-length (aref (penv-envs pe) 0)))
    (list (penv-envs pe) (penv-total-envs pe) (penv-current-env pe) (penv-current-pass pe))))

#|
(definstrument test-power-env (dur env)
  (let* ((pe (make-power-env :envelope env :duration dur :scaler .5))
	 (envs (first pe))
	 (total-envs (second pe))
	 (current-env (third pe))
	 (current-pass (fourth pe))
	 (nd (floor (* dur *srate*))))
    (run
     (loop for i from 0 below nd do
       (outa i (power-env envs total-envs current-env current-pass))))))

;;; (with-sound () (test-power-env 1.0 '(0 0 .325  1 1 32  2 0 0)))
;;; (with-sound () (test-power-env 1.0 '(0 0 .325  1 1 32  2 .5 1  3 1.0 .1234 4 0.0 0.0)))
;;; (with-sound () (test-power-env 1.0 '(0 0 0  1 1 1  2 .5 .123  3 1.0 321 4 0.0 0.0)))
|#


#|
; an example instrument to show what the :base argument to make-env is doing in a very simple case

(definstrument expins (dur frq amp)
  (let* ((nd (floor (* dur *srate*)))
         (o1 (make-oscil frq))
	 (o2 (make-oscil frq))
	 (j nd)
	 (maxdiff 0.0)
         (zv (make-env '(0 1 1 0) :base 2.718 :duration dur)))
    (run* (maxdiff)
     (loop for i from 0 to nd do
       (let* ((vala (* amp (env zv) (oscil o1)))
	      (valb (* (/ amp (- 2.718 1.0)) (- (exp (/ j nd)) 1.0) (oscil o2)))
	      (valc (abs (- valb vala))))
       (outa i vala)
       (outb i valb)
       (if (> valc maxdiff) (setf maxdiff valc))
       (decf j))))
    (print maxdiff)))

;(with-sound (:channels 2 :statistics t) (expins 1.0 440.0 .5))
; 8.189678e-5 
;
; you can get as close as you like to a given exponential (i.e.
; thinking in terms of e^(-kt)) by fiddling with the base:

(definstrument expins (dur k m)
  (let* ((nd (floor (* dur *srate*)))
	 (j 0)
	 (maxdiff 0.0)
         (zv (make-env (list 0 1 1 (exp (- k))) :base m :duration dur)))
    (run* (maxdiff)
     (loop for i from 0 to nd do
       (let* ((vala (env zv))
	      (valb (exp (* (- k) (/ j nd))))
	      (valc (abs (- valb vala))))
       (outa i vala)
       (outb i valb)
       (if (> valc maxdiff) (setf maxdiff valc))
       (incf j))))
    (print maxdiff)))

; (with-sound (:channels 2) (expins 1 .5 1.65))
; 7.2419643e-5
; or
; (with-sound (:channels 2) (expins 1 5 148))
; 1.3118982e-4
;
; that is, let the envelope "base" be (exp k)
|#



;;;=============================================================================
;;; Exponential envelopes
;;;=============================================================================

;;; Approximate an exponential envelope with a given base and error bound
;;; by Fernando Lopez-Lezcano (nando@ccrma.stanford.edu)
;;;
;;; base:
;;;   step size of the exponential envelope
;;; error:
;;;   error band of the approximation
;;; scaler:
;;;   scaling factor for the y coordinates
;;; offset:
;;;   offset for the y coordinates
;;; cutoff:
;;;   lowest value of the exponentially rendered envelope, values lower than
;;;   this cutoff value will be approximated as cero.
;;; out-scaler
;;;   scaler for the converted values

(defun exp-envelope (env &key
		    (base (expt 2 (/ 12)))
		    (error 0.01)
		    (scaler 1)
		    (offset 0)
		    (cutoff nil)
		    (out-scaler 1))
  (let* ((result nil)
	 (ycutoff (if cutoff (expt base (+ offset (* cutoff scaler))) nil)))
    (labels (;; linear interpolation
	     (interpolate (xl yl xh yh xi)
	       (+ yl (* (- xi xl)(/ (- yh yl)(- xh xl)))))
	     ;;
	     ;; recursively render one segment
	     ;;   xl,xh   = x coordinates of segment ends
	     ;;   yl,yh   = y coordinates of segment ends
	     ;;   yle,yhe = exponential values of y coords of segment ends
	     ;;   error   = linear domain error bound for rendering
	     ;;
	     (exp-seg (xl yle xh yhe yl yh error)
	       (let* ((xint (/ (+ xl xh) 2))
		      (yint (interpolate xl yl xh yh xint))
		      (yinte (interpolate xl yle xh yhe xint))
		      (yexp (expt base yint))
		      (yerr (- (expt base (+ yint error)) yexp)))
		 ;; is the linear approximation accurate enough?
		 ;; are we still over the cutoff limit?
		 (if (and (> (abs (- yexp yinte)) yerr)
			  (if ycutoff (> yinte ycutoff) t))
		     ;; no --> add a breakpoint and recurse right and left
		     (multiple-value-bind (xi yi)
			 (exp-seg xl yle xint yexp yl yint error)
		       (multiple-value-bind (xj yj)
			   (exp-seg xint yexp xh yhe yint yh error)
			 (values (nconc xi (list xint) xj)
				 (nconc yi (list yexp) yj))))
		   ;; yes --> don't need to add nu'ting to the envelope
		   (values nil nil)))))
      ;;
      ;; loop for each segment in the envelope
      (loop
	  for x in env by #'cddr
	  for y in (cdr env) by #'cddr
	  for nx in (cddr env) by #'cddr
	  for ny in (cdddr env) by #'cddr
	  for yscl = (+ offset (* y scaler))
	  for nyscl = (+ offset (* ny scaler))
	  for xy = (list x (if (or (not ycutoff)
				   (>= (expt base yscl) ycutoff))
			       (* out-scaler (expt base yscl))
			     0))
	  do
	    (if result (nconc result xy)(setf result xy))
	    (multiple-value-bind (xs ys)
		(exp-seg x (expt base yscl) nx (expt base nyscl) yscl nyscl error)
	      (loop
		  with ys-scaled = (mapcar #'(lambda (y)(* y out-scaler)) ys)
		  for x in xs
		  for y in ys-scaled
		  do (nconc result (list x y))))
	  finally
	    (return (nconc result
			   (list nx (if (or (not ycutoff)
					    (>= (expt base nyscl) ycutoff))
					(* out-scaler (expt base nyscl))
				      0))))))))

;;; Amplitude envelope in dBs
;;;
;;; The db scale is defined as:
;;;    value(db)=(* 20 (log10 (/ vin vref)))
;;;  where:
;;;    vref=1.0 reference value = digital clipping

(def-optkey-fun db-envelope (envelope
			(cutoff -70)
			(error 0.01))
  (exp-envelope envelope
	   :base 10
	   :scaler 1/20
	   :offset 0
	   :cutoff cutoff
	   :error error))

(def-optkey-fun make-db-env (envelope
			     (scaler 1)(offset 0)
			     base duration end
			     (start 0)
			     (cutoff -70)
			     (error 0.01))
  (make-env :envelope (db-envelope envelope cutoff error)
	    :scaler scaler :offset offset
	    :base base :duration duration :end end
	    :start start))

;;; Pitch envelopes (y units are semitone and octave intervals)

(def-optkey-fun semitones-envelope (envelope
			       (around 1.0)
			       (error 0.01))
  (exp-envelope envelope
	   :error error
	   :base (expt 2 (/ 12))
	   :cutoff nil
	   :scaler 1
	   :offset 0
	   :out-scaler around))

(def-optkey-fun make-semitones-env (envelope
				    (around 1.0)
				    (scaler 1.0)(offset 0.0)
				    base duration end
				    (start 0)
				    (error 0.01))
  (make-env :envelope (semitones-envelope envelope around error)
	    :scaler scaler :offset offset
	    :base base :duration duration :end end
	    :start start))

(def-optkey-fun octaves-envelope (envelope
			     (around 1.0)
			     (error 0.01))
  (exp-envelope envelope
	   :error error
	   :base 2
	   :cutoff nil
	   :scaler 1
	   :offset 0
	   :out-scaler around))

(def-optkey-fun make-octaves-env (envelope
				  (around 1.0)
				  (scaler 1.0)(offset 0.0)
				  base duration end
				  (start 0)
				  (error 0.01))
  (make-env :envelope (octaves-envelope envelope around error)
	    :scaler scaler :offset offset
	    :base base :duration duration :end end
	    :start start))



;;; return portion of env between x values beg and end 
;;; (useful when checking portions of a mix or when using global envs)

(defun window-envelope (beg end env)
  (let ((nenv nil)
	(lasty (if env (second env) 0)))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr do
      (setf lasty y)
      (if (not nenv)
	  (if (>= x beg)
	      (progn
		(push beg nenv)
		(push (envelope-interp beg env) nenv)
		(if (/= x beg)
		    (if (>= x end)
			(progn
			  (push end nenv)
			  (push (envelope-interp end env) nenv)
			  (return-from window-envelope (nreverse nenv)))
		      (progn
			(push x nenv)
			(push y nenv))))))
	(if (<= x end)
	    (progn
	      (push x nenv)
	      (push y nenv)
	      (if (= x end)
		  (return-from window-envelope (nreverse nenv))))
	  (if (> x end)
	      (progn
		(push end nenv)
		(push (envelope-interp end env) nenv)
		(return-from window-envelope (nreverse nenv)))))))
    (push end nenv)
    (push lasty nenv)
    (nreverse nenv)))

;;; return a new envelope taking into account the attack and decay times given

(defun stretch-envelope (fn old-att new-att &optional old-dec new-dec)
  (when (and old-dec (not new-dec)) 
    (setf new-dec (clm-cerror "assume new-decay = old-decay" old-dec #'numberp "incorrect number of arguments to stretch-env")))
  (when fn
    (let* ((x0 (car fn))
	   (new-x x0)
	   (last-x (envelope-last-x fn))
	   (y0 (cadr fn))
	   (new-fn (list y0 x0))
	   (scl (/ (- new-att x0) (max .0001 (- old-att x0)))))
      (if (and old-dec (= old-dec old-att)) (incf old-dec (* .000001 last-x)))
      (loop for x1 in (cddr fn) by #'cddr and
	        y1 in (cdddr fn) by #'cddr do
	(when (and (< x0 old-att)
		   (>= x1 old-att))
	  (if (= x1 old-att)
	      (setf y0 y1)
	    (setf y0 (float (+ y0 (* (- y1 y0) (/ (- old-att x0) (- x1 x0)))))))
	  (setf x0 old-att)
	  (setf new-x new-att)
	  (push new-x new-fn)
	  (push y0 new-fn)
	  (setf scl (if old-dec 
			(/ (- new-dec new-att) (- old-dec old-att))
		      (/ (- last-x new-att) (- last-x old-att)))))
	(when (and old-dec
		   (< x0 old-dec)
		   (>= x1 old-dec))
	  (if (= x1 old-dec)
	      (setf y0 y1)
	    (setf y0 (float (+ y0 (* (- y1 y0) (/ (- old-dec x0) (- x1 x0)))))))
	    (setf x0 old-dec)
	    (setf new-x new-dec)
	    (push new-x new-fn)
	    (push y0 new-fn)
	    (setf scl (/ (- last-x new-dec) (- last-x old-dec))))
	(when (/= x0 x1)
	  (incf new-x (float (* scl (- x1 x0))))
	  (push new-x new-fn)
	  (push y1 new-fn)
	  (setf x0 x1)
	  (setf y0 y1)))
      (nreverse new-fn))))
		

(defun envelope->coeffs (&key (order 4)
			      (envelope nil)
			      (dc 0))
  (let* ((x (make-double-array (1+ order)))
	 (a (make-double-array (1+ order)))
	 (n order)
	 (last-x (envelope-last-x envelope))
	 (freq-scl (/ last-x order))
	 (m (floor (1+ n) 2))
	 (am (/ (+ n 1.0) 2.0))
	 (m1 (floor (+ (/ n 2) 1)))
	 (q (/ (* 2 pi) n))
	 (n2 (floor n 2)))
    (loop for j from 1 to m1 and fr from 0 by freq-scl do
      (setf (aref a j) (double (envelope-interp fr envelope))))
    (if (zerop dc)
	(progn
	  (loop for j from 1 to m do
	    (let ((xt (/ (aref a 1) 2)))
	      (loop for i from 2 to m do
		(incf xt (* (aref a i) (cos (* q (- am j) (- i 1))))))
	      (setf (aref x j) (/ (* 2.0 xt) n)))))
      (progn
	(loop for j from 1 to m do
	  (let ((xt 0))
	    (loop for i from 1 to n2 do
	      (incf xt (* (aref a i) (cos (* q (- am j) (- i 0.5))))))
	    (if (/= am m)
		(incf xt (* (aref a m) .5 (cos (* pi (- am j))))))
	    (setf (aref x j) (/ (* 2 xt) n))))))
    (let ((len (floor order 2)))
      (append 
       (loop for i from 1 to len collect (aref x i))
       (if (oddp order) 
	   (loop for i from (1+ len) downto 1 collect (aref x i))
	 (loop for i from len downto 1 collect (aref x i)))))))


