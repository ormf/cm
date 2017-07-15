;;; -*- syntax: common-lisp; base: 10; mode: lisp -*-

;;; bounded brownian noise ("green noise" was Michael McNabb's name for it)
;;; has a low frequency component from the bounce off the bounds
;;; a sort of weak 1/f noise
;;; on each step, the bounds are checked, and if exceeded, the step is sent the other direction.

(def-clm-struct grn (output 0.0) (amp .1) (lo -1.0) (hi 1.0) (phase 0.0) (freq 0.0) (incr 0.0))

(defmacro green-noise-1 (gr)
  `(let ((val (centered-random (grn-amp ,gr))))
     (incf (grn-output ,gr) val)
     (if (not (<= (grn-lo ,gr) (grn-output ,gr) (grn-hi ,gr)))
	 (decf (grn-output ,gr) (* 2 val)))
     (grn-output ,gr)))

;;; to get brownian noise (which is unbounded, so not usually very useful)
;;; you could simply set the lo and high bounds to some enormous numbers, or:

(defmacro brownian-noise (gr)
  `(incf (grn-output ,gr) (centered-random (grn-amp ,gr))))

(defun make-green-noise (&key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  (make-grn :freq (hz->radians frequency)
	    :amp amplitude
	    :hi high
	    :lo low))

(defmacro green-noise (r &optional (sweep 0.0))
  `(progn
     (if (>= (grn-phase ,r) two-pi)
	 (progn
	   (loop while (>= (grn-phase ,r) two-pi) do (decf (grn-phase ,r) two-pi))
	   (green-noise-1 ,r)))
     (incf (grn-phase ,r) (+ (grn-freq ,r) ,sweep))
     (loop while (minusp (grn-phase ,r)) do (incf (grn-phase ,r) two-pi))
     (grn-output ,r)))

(defun make-green-noise-interp (&key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  (make-grn :freq (hz->radians frequency)
	    :hi high
	    :lo low
	    :amp amplitude
	    :incr (* (centered-random amplitude) (/ frequency *srate*))))

(defmacro green-noise-interp (r &optional (sweep 0.0))
  `(progn
     (incf (grn-output ,r) (grn-incr ,r))
     (when (>= (grn-phase ,r) two-pi)
       (let ((cur (grn-output ,r))
	     (new (green-noise-1 ,r)))
	 (setf (grn-output ,r) cur)
	 (loop while (>= (grn-phase ,r) two-pi) do (decf (grn-phase ,r) two-pi))
	 (setf (grn-incr ,r) (* (- new cur) (/ (+ (grn-freq ,r) ,sweep) two-pi)))))
     ;; (both grn-freq and sweep are in terms of radians/sample, so by dividing by two-pi, we get
     ;; the distance we go to the next new number in terms of 0..1 (i.e. inverse of number of
     ;; samples per period) -- this is equivalent to the multiply in the make function.
     (incf (grn-phase ,r) (+ (grn-freq ,r) ,sweep))
     (loop while (minusp (grn-phase ,r)) do (incf (grn-phase ,r) two-pi))
     (grn-output ,r)))

#|
(definstrument green (beg end)
  (let* ((grn (make-green-noise :frequency 4000 :amplitude .01 :high .2 :low -.5)))
    (run
     (loop for i from beg below end do
       (outa i (+ .5 (green-noise grn)))))))

(definstrument green (beg end)
  (let* ((grn (make-green-noise-interp :frequency 1000 :amplitude .01 :high .2 :low -.5)))
    (run
     (loop for i from beg below end do
       (outa i (+ .5 (green-noise-interp grn)))))))

|#
