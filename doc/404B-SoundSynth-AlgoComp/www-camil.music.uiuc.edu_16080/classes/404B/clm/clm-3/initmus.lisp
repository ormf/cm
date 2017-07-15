;;; -*- syntax: common-lisp; package: clm; base: 10; mode: lisp -*-
;;;
;;; main defpackage is in clm-package.lisp. Export statement is in export.lisp.

(in-package :clm)

(defvar *clm* (find-package :clm) "CLM synthesis package")
(defvar *clm-version* 3)
(defvar *clm-revision* 21)
(defvar *clm-date* "12-May-05")
(defvar *clm-news* 
  "
2-May:  vowel.ins thanks to Michelle Daniels and Oded Ben-Tal.
27-Apr: checked sbcl 0.9.0.
11-Apr: mixer* and friends can take float args (replacing mixer-scale).
5-Apr:  run and run* bugfixes.
23-Mar: changed interpretation or frame->frame args.
21-Mar: removed bessel.lisp -- it's very much out of date and should be rewritten to use the polynomial function and libm.
          the libm part of this is in place (bes-j0, etc)
16-Mar: polyshape generator (oscil + polynomial).
        mus-chebyshev-first|second-kind (as 'kind' arg to partials->polynomial)
        partials->waveshape no long normalizes.
14-Mar: sbcl can now handle multi-instrument files, and no longer uses the *.sbcl kludge.
")

(defvar *clm-source-directory* "")
(defvar *clm-binary-directory* "")
(defvar *clm-ins-directory* nil)
(defvar *clm-compiler-name* #-windoze "cc" #+windoze "cl")         
					;this is set in all.lisp via the Envirionment variable "CC"

(defconstant +clm-interrupted+ 0)
(defconstant +clm-beg+ 1) ; two words for bignum
(defconstant +clm-end+ 3) ; ditto
(defconstant +int-block-size+ 5)
(defconstant +float-block-size+ 0)

;;; this is reflected in cmus.h
(defconstant +no-type+ 0)
(defconstant +integer+ 1)
(defconstant +real+ 2)
(defconstant +oscil+ 3)
(defconstant +sum-of-cosines+ 4)
(defconstant +rand+ 5)
(defconstant +rand-interp+ 6)
(defconstant +table-lookup+ 7)
(defconstant +square-wave+ 8)
(defconstant +pulse-train+ 9)
(defconstant +sawtooth-wave+ 10)
(defconstant +triangle-wave+ 11)
(defconstant +asymmetric-fm+ 12)
(defconstant +wave-train+ 13)
(defconstant +one-pole+ 14)
(defconstant +two-pole+ 15)
(defconstant +one-zero+ 16)
(defconstant +two-zero+ 17)
(defconstant +delay+ 18)
(defconstant +tap+ 19)
(defconstant +comb+ 20)
(defconstant +notch+ 21)
(defconstant +all-pass+ 22)
(defconstant +filter+ 23)
(defconstant +fir-filter+ 24)
(defconstant +iir-filter+ 25)
(defconstant +array+ 26)
(defconstant +env+ 27)
(defconstant +locsig+ 28)
(defconstant +src+ 29)
(defconstant +granulate+ 30)
(defconstant +readin+ 31)
(defconstant +convolve+ 32)
(defconstant +sine-summation+ 33)
(defconstant +waveshape+ 34)
(defconstant +formant+ 35)
(defconstant +real-array+ 36)
(defconstant +integer-array+ 37)
(defconstant +string+ 38)
(defconstant +frame+ 39)
(defconstant +mixer+ 40)
(defconstant +phase-vocoder+ 41)
(defconstant +bignum+ 42)
(defconstant +average+ 43)
(defconstant +sum-of-sines+ 44)
(defconstant +ssb-am+ 45)
(defconstant +file2sample+ 46)
(defconstant +file2frame+ 47)
(defconstant +sample2file+ 48)
(defconstant +frame2file+ 49)
(defconstant +polyshape+ 50)

(defgeneric mus-frequency (gen))   (defgeneric (setf mus-frequency) (val gen))
(defgeneric mus-phase (gen))       (defgeneric (setf mus-phase) (val gen))
(defgeneric mus-cosines (gen))     (defgeneric (setf mus-cosines) (val gen))
(defgeneric mus-data (gen))        (defgeneric (setf mus-data) (val gen))
(defgeneric mus-offset (gen))      (defgeneric (setf mus-offset) (val gen))
(defgeneric mus-scaler (gen))      (defgeneric (setf mus-scaler) (val gen))
(defgeneric mus-width (gen))       (defgeneric (setf mus-width) (val gen))
(defgeneric mus-length (gen))      (defgeneric (setf mus-length) (val gen))
(defgeneric mus-location (gen))    (defgeneric (setf mus-location) (val gen))
(defgeneric mus-increment (gen))   (defgeneric (setf mus-increment) (val gen))
(defgeneric mus-feedback (gen))    (defgeneric (setf mus-feedback) (val gen))
(defgeneric mus-feedforward (gen)) (defgeneric (setf mus-feedforward) (val gen))
(defgeneric mus-order (gen))     
(defgeneric mus-xcoeffs (gen))
(defgeneric mus-ycoeffs (gen))
(defgeneric mus-channels (gen))
(defgeneric mus-channel (gen))     (defgeneric (setf mus-channel) (val gen))
(defgeneric mus-hop (gen))         (defgeneric (setf mus-hop) (val gen))
(defgeneric mus-ramp (gen))        (defgeneric (setf mus-ramp) (val gen))
(defgeneric mus-interp-type (gen))
(defgeneric mus-xcoeff (gen index)) (defgeneric (setf mus-xcoeff) (val gen index))
(defgeneric mus-ycoeff (gen index)) (defgeneric (setf mus-ycoeff) (val gen index))
(defgeneric mus-describe (gen))
(defgeneric mus-file-name (gen))
(defgeneric mus-name (gen))
(defgeneric mus-reset (gen))

(defgeneric oscil? (gen))
(defgeneric table-lookup? (gen))
(defgeneric delay? (gen))
(defgeneric comb? (gen))
(defgeneric notch? (gen))
(defgeneric all-pass? (gen))
(defgeneric average? (gen))
(defgeneric filter? (gen))
(defgeneric fir-filter? (gen))
(defgeneric iir-filter? (gen))
(defgeneric one-zero? (gen))
(defgeneric one-pole? (gen))
(defgeneric two-pole? (gen))
(defgeneric two-zero? (gen))
(defgeneric formant? (gen))
(defgeneric rand? (gen))
(defgeneric rand-interp? (gen))
(defgeneric env? (gen))
(defgeneric triangle-wave? (gen))
(defgeneric square-wave? (gen))
(defgeneric sawtooth-wave? (gen))
(defgeneric pulse-train? (gen))
(defgeneric waveshape? (gen))
(defgeneric polyshape? (gen))
(defgeneric sum-of-cosines? (gen))
(defgeneric sum-of-sines? (gen))
(defgeneric sine-summation? (gen))
(defgeneric asymmetric-fm? (gen))
(defgeneric locsig? (gen))
(defgeneric file->sample? (gen))
(defgeneric mus-input? (gen))
(defgeneric file->frame? (gen))
(defgeneric sample->file? (gen))
(defgeneric mus-output? (gen))
(defgeneric frame->file? (gen))
(defgeneric readin? (gen))
(defgeneric frame? (gen))
(defgeneric mixer? (gen))
(defgeneric wave-train? (gen))
(defgeneric src? (gen))
(defgeneric convolve? (gen))
(defgeneric granulate? (gen))
(defgeneric phase-vocoder? (gen))
(defgeneric ssb-am? (gen))

(defgeneric sample->frame (gen s &optional outf))
(defgeneric frame->sample (gen fin))
(defgeneric mus-run (gen &optional arg1 arg2))


(defvar *clm-instruments* nil)		;list of currently loaded instruments
(defvar *clm-linked* nil)
(defconstant two-pi (* pi 2))

(defun double (x) (coerce x 'double-float))
#+openmcl (defun double-float (x) (coerce x 'double-float)) ; backwards compatibility, I hope

(defmacro make-double-float-array (lim &key initial-contents initial-element)
  (let ((ic initial-contents)
	(ie initial-element))
    (if ic
	`(make-array ,lim :element-type 'double-float :initial-contents (map 'list #'(lambda (n) (double n)) ,ic))
      (if ie
	  `(make-array ,lim :element-type 'double-float :initial-element (double ,ie))
	`(make-array ,lim :element-type 'double-float :initial-element (coerce 0.0 'double-float))
	))))
      
(defmacro make-double-array (lim &key initial-contents initial-element)
  (let ((ic initial-contents)
	(ie initial-element))
    (if ic
	`(make-array ,lim :element-type 'double-float :initial-contents (map 'list #'(lambda (n) (double n)) ,ic))
      (if ie
	  `(make-array ,lim :element-type 'double-float :initial-element (double ,ie))
	`(make-array ,lim :element-type 'double-float :initial-element (coerce 0.0 'double-float))
	))))
      
(defmacro make-integer-array (len &rest args)
  `(make-array ,len :element-type
	       #-(or cmu sbcl openmcl allegro-v7.0) 'fixnum
               #+cmu '(unsigned-byte 32) ; should be signed-byte here, but CMUCL is full of bugs
               #+sbcl '(signed-byte 32)
	       #+(or allegro-v7.0 openmcl) '(signed-byte 32)
               ,@args))
 
(defun print-hash (tab &optional (stream t)) (maphash #'(lambda (a b) (format stream "~A ~A~%" a b)) tab))

(defun clm-print (fstr &rest args) 
  ;; 30-Sep-96 allow file output(?)
  (if (stringp fstr)
      (princ (apply #'format nil fstr args))
    (apply #'format fstr (car args) (cdr args))))

(defun run-in-shell (prog args)
  (let ((str (format nil "~A ~A" prog args)))
    #+debug (progn (print str) (force-output))
    #+excl (excl:shell str)
    #+cmu (extensions:run-program "/bin/csh" (list "-fc" str) :output t)
    #+openmcl (ccl:run-program "/bin/csh" (list "-fc" str) :output t)
    #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" str) :output t)
    #-(or excl cmu sbcl openmcl) (warn "can't run ~A in a shell" prog)
    ))

(defun cwd (&optional def)
  (if def
      (truename def)
    #+excl (excl:current-directory)
    #-excl (truename "./"))
  )

;;; take care of some minor differences in file names and so on

#-sbcl
(defconstant *clm-fasl-name*
  #+cmu (c:backend-fasl-file-type c:*backend*)
  #+openmcl (pathname-type ccl:*.fasl-pathname*)
  #+excl excl:*fasl-default-type*
  #-(or cmu openmcl excl) "fasl")
#+sbcl (defvar *clm-fasl-name* "fasl")

(defvar *clm-lisp-name* "lisp")

(defvar *clm-c-options* 
  #+sgi " -DSGI" 
  #+sun " -DSUN" 
  #+(and linux (not alsa)) " -DLINUX" #+(and linux alsa) " -DLINUX -DHAVE_ALSA"
  #+hpux " -DHPUX"
  #+windoze " -DWINDOZE"
  #-(or sgi openmcl sun linux hpux windoze) "")

#+(and (or excl cmu) sgi) (defvar *sgi-cflag* nil)
					; set at end of all.lisp
(defvar *so-ext* nil)

#+cltl2 (defmacro without-warnings (&body body)
	  `(handler-bind ((simple-warning 
			   #'(lambda (c) 
			       (declare (ignore c))
			       (when (find-restart 'muffle-warning) 
				 (invoke-restart 'muffle-warning)))))
	     ,@body))

#-cltl2 (defmacro without-warnings (&body body) `(progn ,.body))

;;; we also need restart-case in all lisps.  
;;;  In later ACL's it is built-in.
;;;  In CMU-CL it is broken.

;#+cmu (defmacro restart-case (expr &rest rest) (declare (ignore rest)) expr)

(defun clm-cerror (continue-control continue-default-value continue-test error-control &rest args)
  ;; like cerror, except provides a default continuation value, and if continue-test, prompts for new value
  (apply #'cerror continue-control error-control args)
  ;; if we get here, we've been told to go on
  (if continue-test
      (loop do 
	(progn
	  (princ (format nil "new value (return=~A):" continue-default-value))
	  (multiple-value-bind (new-str eof) (read-line)
	    (if (or eof (zerop (length new-str)))
		(return-from clm-cerror continue-default-value)
	      (let ((new-val (read-from-string new-str)))
		(if (funcall continue-test new-val)
		    (return-from clm-cerror new-val)
		  (print (format nil "~A is not a valid value in this context" new-val))))))))
    continue-default-value))

;;; The documentation lists the make-<gen> function arguments as &optional-key -- the
;;; meaning is that keyword names can be omitted, and the successive arguments are filled
;;; in order until a keyword is encountered, after which only keyword-arg pairs can occur.
;;; These can also have optional values (without the &optional in the declaration).

(defmacro def-optkey-fun (name (&rest args) &body body)
  (let ((keyed-name (intern (concatenate 'string (symbol-name name) "-1")))
	(argnames (loop for arg in args collect (intern (symbol-name (if (listp arg) (first arg) arg)) (find-package :keyword)))))
  `(progn
     (defun ,keyed-name (&key ,@args) ,@body)
     (defun ,name (&rest passed-args)
       (if (or (null passed-args) (keywordp (first passed-args)))
	   (apply #',keyed-name passed-args)
	 (let ((parglen (length passed-args)))
	   (if (or (= parglen 1) (and (> parglen 2) (keywordp (second passed-args))))
	       (apply #',keyed-name ,(first argnames) (first passed-args) (rest passed-args))
	     (if (or (= parglen 2) (and (> parglen 3) (keywordp (third passed-args))))
		 (apply #',keyed-name 
			,(first argnames) (first passed-args) 
			,(second argnames) (second passed-args) 
			(nthcdr 2 passed-args))
	       (let ((i 0))
		 (loop for arg in passed-args while (not (keywordp arg)) do (incf i))
		 (let ((unkeyed-args (loop for arg in passed-args and keyarg in ',argnames while (not (keywordp arg)) collect keyarg collect arg))
		       (keyed-args (nthcdr i passed-args)))
		   (apply #',keyed-name (append unkeyed-args keyed-args))))))))))))


#+(or (not (or cmu sbcl excl)) windoze) (defun clm-send-snd (arg) (declare (ignore arg)) 0)
#+(or (not (or cmu sbcl excl)) windoze) (defun clm-start-snd () 0)
#+(or (not (or cmu sbcl excl)) windoze) (defun clm-init-x (arg) (declare (ignore arg)) 0)
#+(or (not (or cmu sbcl excl)) windoze) (defun clm-receive-snd () 0)

(defconstant mus-unsupported 0)
(defconstant mus-next 1)
(defconstant mus-aifc 2)
(defconstant mus-riff 3)
(defconstant mus-bicsf 4)
(defconstant mus-nist 5)
(defconstant mus-inrs 6)
(defconstant mus-esps 7)
(defconstant mus-svx 8)
(defconstant mus-voc 9)
(defconstant mus-sndt 10)
(defconstant mus-raw 11)
(defconstant mus-smp 12)
(defconstant mus-avr 13)
(defconstant mus-ircam 14)
(defconstant mus-sd1 15)
(defconstant mus-sppack 16)
(defconstant mus-mus10 17)
(defconstant mus-hcom 18)
(defconstant mus-psion 19)
(defconstant mus-maud 20)
(defconstant mus-ieee 21)
(defconstant mus-matlab 22)
(defconstant mus-adc 23)
(defconstant mus-midi 24)
(defconstant mus-soundfont 25)
(defconstant mus-gravis 26)
(defconstant mus-comdisco 27)
(defconstant mus-goldwave 28)
(defconstant mus-srfs 29)
(defconstant mus-midi-sample-dump 30)
(defconstant mus-diamondware 31)
(defconstant mus-adf 32)
(defconstant mus-sbstudioii 33)
(defconstant mus-delusion 34)
(defconstant mus-farandole 35)
(defconstant mus-sample-dump 36)
(defconstant mus-ultratracker 37)
(defconstant mus-yamaha-sy85 38)
(defconstant mus-yamaha-tx16 39)
(defconstant mus-digiplayer 40)
(defconstant mus-covox 41)
(defconstant mus-avi 42)
(defconstant mus-omf 43)
(defconstant mus-quicktime 44)
(defconstant mus-asf 45)
(defconstant mus-yamaha-sy99 46)
(defconstant mus-kurzweil-2000 47)
(defconstant mus-aiff 48)
(defconstant mus-paf 49)
(defconstant mus-csl 50)
(defconstant mus-file-samp 51)
(defconstant mus-pvf 52)
(defconstant mus-soundforge 53)
(defconstant mus-twinvq 54)
(defconstant mus-akai4 55)
(defconstant mus-impulsetracker 56)
(defconstant mus-korg 57)
(defconstant mus-nvf 58)
(defconstant mus-maui 59)
(defconstant mus-sdif 60)

(defun mus-header-type-ok (n)
  (and (> n mus-unsupported)
       (<= n mus-maui)))

(defconstant mus-unknown 0)
(defconstant mus-bshort 1)
(defconstant mus-mulaw 2)
(defconstant mus-byte 3)
(defconstant mus-bfloat 4)
(defconstant mus-bint 5)
(defconstant mus-alaw 6)
(defconstant mus-ubyte 7)
(defconstant mus-b24int 8)
(defconstant mus-bdouble 9)
(defconstant mus-lshort 10)
(defconstant mus-lint 11)
(defconstant mus-lfloat 12)
(defconstant mus-ldouble 13)
(defconstant mus-ubshort 14)
(defconstant mus-ulshort 15)
(defconstant mus-l24int 16)
(defconstant mus-bintn 17)
(defconstant mus-lintn 18)
(defconstant mus-blfoatu 19)
(defconstant mus-lfloatu 20)
(defconstant mus-bdoubleu 21)
(defconstant mus-ldoubleu 22)

(defun mus-data-format-ok (n)
  (and (> n mus-unknown)
       (<= n mus-ldoubleu)))

(defun mus-audio-pack-system (n) (ash n 16))
(defun mus-audio-system (n) (logand (ash n -16) #xffff))
(defun mus-audio-device (n) (logand n #xffff))

(defconstant mus-audio-default 0)
(defconstant mus-audio-duplex-default 1)
(defconstant mus-audio-adat-in 2)
(defconstant mus-audio-aes-in 3)
(defconstant mus-audio-line-out 4)
(defconstant mus-audio-line-in 5)
(defconstant mus-audio-microphone 6)
(defconstant mus-audio-speakers 7)
(defconstant mus-audio-digital-in 8)
(defconstant mus-audio-digital-out 9)
(defconstant mus-audio-dac-out 10)
(defconstant mus-audio-adat-out 11)
(defconstant mus-audio-aes-out 12)
(defconstant mus-audio-dac-filter 13)
(defconstant mus-audio-mixer 14)
(defconstant mus-audio-line1 15)
(defconstant mus-audio-line2 16)
(defconstant mus-audio-line3 17)
(defconstant mus-audio-aux-input 18)
(defconstant mus-audio-cd 19)
(defconstant mus-audio-aux-output 20)
(defconstant mus-audio-spdif-in 21)
(defconstant mus-audio-spdif-out 22)
(defconstant mus-audio-amp 23)
(defconstant mus-audio-srate 24)
(defconstant mus-audio-channel 25)
(defconstant mus-audio-format 26)
(defconstant mus-audio-imix 27)
(defconstant mus-audio-igain 28)
(defconstant mus-audio-reclev 29)
(defconstant mus-audio-pcm 30)
(defconstant mus-audio-pcm2 31)
(defconstant mus-audio-ogain 32)
(defconstant mus-audio-line 33)
(defconstant mus-audio-synth 34)
(defconstant mus-audio-bass 35)
(defconstant mus-audio-treble 36)
(defconstant mus-audio-port 37)
(defconstant mus-audio-samples-per-channel 38)
(defconstant mus-audio-direction 39)

(defun mus-audio-device-ok (a)
  (and (>= a mus-audio-default)
       (<= a mus-audio-direction)))

(defconstant mus-interp-none 0)
(defconstant mus-interp-linear 1)
(defconstant mus-interp-sinusoidal 2)
(defconstant mus-interp-all-pass 3)
(defconstant mus-interp-lagrange 4)
(defconstant mus-interp-bezier 5)
(defconstant mus-interp-hermite 6)

(defconstant mus-chebyshev-first-kind 1)
(defconstant mus-chebyshev-second-kind 2)
