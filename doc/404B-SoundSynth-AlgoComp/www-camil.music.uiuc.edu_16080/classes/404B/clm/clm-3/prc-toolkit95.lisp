;;; -*- syntax: common-lisp; base: 10; mode: lisp -*-

;;; this is a translation to CLM of Perry Cook's Physical Modelling Toolkit.
;;;
;;; with "release" bug fix from Alastair.Jenkins@nrsc.no 17-July-98
;;; this could be simplified -- see prc95.scm in the Snd tarball.

;;; reedtable
(def-clm-struct reed (offset 0.6) (slope -0.8))

(defmacro reedtable (r sample) 
  `(min 1.0 (+ (reed-offset ,r) (* (reed-slope ,r) ,sample))))

;;; bowtable
(def-clm-struct bowt (offset 0.0) (slope 1.0))

(defmacro bowtable (b sample) 
  `(max 0.0 (- 1.0 (abs (* (bowt-slope ,b) (+ ,sample (bowt-offset ,b)))))))

;;;jettable
(defmacro jettable (sample0) 
  `(let ((sample ,sample0)) (max -1.0 (min 1.0 (* sample (- (* sample sample) 1.0))))))

;;; one-zero filter (slightly different from clm's)
(defun make-onez (&key (gain 0.5) (zerocoeff 1.0))
  (make-one-zero gain (* gain zerocoeff)))

;;; one-pole filter (also slightly different)
(def-clm-struct onep (polecoeff 0.9) (gain 1.0) (sgain 0.1) (output 0.0))

(defun fixup-sgain (p)
  (if (> (onep-polecoeff p) 0.0) 
      (setf (onep-sgain p) (* (onep-gain p) (- 1.0 (onep-polecoeff p))))
    (setf (onep-sgain p) (* (onep-gain p) (+ 1.0 (onep-polecoeff p))))))

(defun set-pole (p aval) (setf (onep-polecoeff p) aval) (fixup-sgain p))

(defun set-gain (p aval) (setf (onep-gain p) aval) (fixup-sgain p))

(defmacro onepole (p sample0) 
  `(let ((sample ,sample0))
     (setf (onep-output ,p) (+ (* sample (onep-sgain ,p)) 
			       (* (onep-polecoeff ,p) (onep-output ,p))))))

;;; biquadfilter (a version of clm's formant generator)
(def-clm-struct biq (pc0 0.0) (pc1 0.0) (zc0 0.0) (zc1 0.0) (gain 1.0) (out0 0.0) (out1 0.0) (in0 0.0) (in1 0.0))

(defmacro biquad (b sample0)
  `(let ((sample ,sample0))
     (let ((temp (+ (* (biq-zc0 ,b) (biq-in0 ,b)) (* (biq-zc1 ,b) (biq-in1 ,b)))))
       (setf (biq-in1 ,b) (biq-in0 ,b))
       (setf (biq-in0 ,b) (* (biq-gain ,b) sample))
       (incf temp (+ (biq-in0 ,b) (* (biq-pc0 ,b) (biq-out0 ,b)) (* (biq-pc1 ,b) (biq-out1 ,b))))
       (setf (biq-out1 ,b) (biq-out0 ,b))
       (setf (biq-out0 ,b) temp))))

;;;lipfilter (a biquad filter)
(defun lip-set-freq (b freq) 
  (setf (biq-pc0 b) (* 2.0 0.999 (cos (/ (* pi 2 freq) *srate*))))
  (setf (biq-pc1 b) (* -0.999 0.999))
  (setf (biq-gain b) 0.02))

(defmacro lip (b mouthsample0 boresample0)
  `(let ((mouthsample ,mouthsample0)
	 (boresample ,boresample0))
     (let ((temp (biquad ,b (- mouthsample boresample))))
       (setf temp (min 1.0 (* temp temp)))
       (+ (* temp mouthsample) (* (- 1.0 temp) boresample)))))

(def-clm-struct dcb (input 0.0) (output 0.0))
(defmacro dcblock (b sample0) 
  `(let ((sample ,sample0))
     (prog1
	 (setf (dcb-output ,b) (+ sample (- (* 0.99 (dcb-output ,b)) (dcb-input ,b))))
       (setf (dcb-input ,b) sample))))
;;; in the sndtools program sndblockdc, the 0.99 is replaced by (- 1.0 (/ 7.0 adaption_time))

;;;delaylinea -- "a" for "allpass" I think (this comment added 5 years later without re-reading the code...)
(def-clm-struct dla inpoint outpoint (lastin 0.0) length (output 0.0) input alpha coeff)

(defun make-delaya (len) 
  (let ((nd (make-dla :length len :input (make-double-array len :initial-element 0.0) :inpoint 0 :outpoint 0)))
    (set-delaya nd (* 0.5 len))
    nd))

(defun set-delaya (d lag)
  (let ((outpointer (+ (dla-inpoint d) (- 2.0 lag))))
    (loop while (minusp outpointer) do (incf outpointer (dla-length d)))
    (setf (dla-outpoint d) (floor outpointer))
    (setf (dla-alpha d) (- outpointer (dla-outpoint d)))
    (setf (dla-coeff d) (/ (- 1.0 (dla-alpha d)) (+ 1.0 (dla-alpha d))))))

(defmacro delaya (d sample0)
  `(let ((sample ,sample0))
     (let ((temp 0.0))
       (setf (aref (dla-input ,d) (dla-inpoint ,d)) sample)
       (incf (dla-inpoint ,d))
       (if (= (dla-inpoint ,d) (dla-length ,d)) (setf (dla-inpoint ,d) 0))
       (setf temp (aref (dla-input ,d) (dla-outpoint ,d)))
       (incf (dla-outpoint ,d))
       (if (>= (dla-outpoint ,d) (dla-length ,d)) (decf (dla-outpoint ,d) (dla-length ,d)))
       (setf (dla-output ,d) (+ (* (- (dla-coeff ,d)) (dla-output ,d)) (dla-lastin ,d) (* temp (dla-coeff ,d))))
       (setf (dla-lastin ,d) temp)
       (dla-output ,d))))

;;; delaylinel
(def-clm-struct dll inpoint outpoint length (output 0.0) input alpha omalpha)

(defun make-delayl (len) 
  (let ((nd (make-dll :length len :input (make-double-array len :initial-element 0.0) :inpoint 0 :outpoint 0)))
    (set-delayl nd (* 0.5 len))
    nd))

(defun set-delayl (d lag)
  (let ((outpointer (+ (dll-inpoint d) (- 1 lag))))
    (loop while (minusp outpointer) do (incf outpointer (dll-length d)))
    (setf (dll-outpoint d) (floor outpointer))
    (setf (dll-alpha d) (- outpointer (dll-outpoint d)))
    (setf (dll-omalpha d) (- 1.0 (dll-alpha d)))))

(defmacro delayl (d sample0)
  `(let ((sample ,sample0))
     (setf (aref (dll-input ,d) (dll-inpoint ,d)) sample)
     (incf (dll-inpoint ,d))
     (if (= (dll-inpoint ,d) (dll-length ,d)) (setf (dll-inpoint ,d) 0))
     (setf (dll-output ,d) (* (aref (dll-input ,d) (dll-outpoint ,d)) (dll-omalpha ,d)))
     (incf (dll-outpoint ,d))
     (if (= (dll-outpoint ,d) (dll-length ,d)) (setf (dll-outpoint ,d) 0))
     (incf (dll-output ,d) (* (aref (dll-input ,d) (dll-outpoint ,d)) (dll-alpha ,d)))))


;;; now some example instruments

(definstrument plucky (beg dur freq amplitude &optional (maxa 1.0))
  (let* ((lowestfreq 100.0)
	 (len (1+ (floor (/ *srate* lowestfreq))))
	 (delayline (make-delaya len))
	 (filter (make-onez))
	 (st (floor (* *srate* beg)))
	 (nd (+ st (floor (* *srate* dur)))))
    (set-delaya delayline (- (/ *srate* freq) 0.5))
    (loop for i from 0 below len do
      (delaya delayline (double (+ (* 0.99 (dla-output delayline)) (* maxa (- 1.0 (random 2.0)))))))
    (run
     (loop for i from st to nd do
       (outa i (* amplitude (delaya delayline (one-zero filter (dla-output delayline)))))))))

;;; freq is off in this one (in prc's original also)
(definstrument bow-1 (beg dur frq amplitude &optional (maxa 1.0))
  (let* ((lowestfreq 100.0)
	 (len (1+ (floor (/ *srate* lowestfreq))))
	 (neckdelay (make-delayl len))
	 (bridgedelay (make-delayl (floor len 2)))
	 (bowtab (make-bowt :slope 3.0))
	 (filt (make-onep))
	 (rate .001)
	 (bowing t)
	 (bowvelocity rate)
	 (maxvelocity maxa)
	 (attackrate rate)
	 (st (floor (* *srate* beg)))
	 (nd (+ st (floor (* *srate* dur))))
	 (release (+ st (floor (* .8 (- nd st))))))
    (set-pole filt 0.6)
    (set-gain filt 0.3)
    (let ((ratio 0.8317)
	  (temp (- (/ *srate* frq) 4.0)))
      (set-delayl neckdelay (* temp ratio))
      (set-delayl bridgedelay (* temp (- 1.0 ratio))))
    (run
     (loop for i from st to nd do 
       (let* ((bridgerefl 0.0)
	      (nutrefl 0.0) 
	      (veldiff 0.0) 
	      (stringvel 0.0) 
	      (bowtemp 0.0))
	 (if bowing
	     (if (/= maxvelocity bowvelocity)
		 (if (< bowvelocity maxvelocity)
		     (incf bowvelocity attackrate)
		   (decf bowvelocity attackrate)))
	   (if (> bowvelocity 0.0) (decf bowvelocity attackrate)))
	 (setf bowtemp (* 0.3 bowvelocity))
	 (setf bridgerefl (- (onepole filt (dll-output bridgedelay))))
	 (setf nutrefl (- (dll-output neckdelay)))
	 (setf stringvel (+ bridgerefl nutrefl))
	 (setf veldiff (- bowtemp stringvel))
	 (setf veldiff (* veldiff (bowtable bowtab veldiff)))
	 (delayl neckdelay (+ bridgerefl veldiff))
	 (delayl bridgedelay (+ nutrefl veldiff))
	 (outa i (* amplitude 10.0 (onep-output filt)))
	 (when (= i release) 
	   (setf bowing nil)
	   (setf attackrate .0005)))))))

(definstrument brass (beg dur freq amplitude &optional (maxa 1.0))
  (let* ((lowestfreq 100.0)
	 (len (1+ (floor (/ *srate* lowestfreq))))
	 (delayline (make-delaya len))
	 (lipfilter (make-biq))
	 (dcblocker (make-dcb))
	 (blowing t)
	 (rate .001)
	 (breathpressure 0.0)  ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (floor (* *srate* beg)))
	 (nd (+ st (floor (* *srate* dur))))
	 (release (+ st (floor (* .8 (- nd st))))))
    (set-delaya delayline (+ 1.0 (/ *srate* freq)))
    (lip-set-freq lipfilter freq)
    (run
     (loop for i from st to nd do
       (if blowing
	   (if (/= maxpressure breathpressure)
	       (if (< breathpressure maxpressure)
		   (incf breathpressure attackrate)
		 (decf breathpressure attackrate)))
	 (if (> breathpressure 0.0)
	     (decf breathpressure attackrate)))
       (outa i (* amplitude (delaya delayline
				    (dcblock dcblocker
					     (lip lipfilter
						  (* 0.3 breathpressure)
						  (* 0.9 (dla-output delayline)))))))
       (when (= i release) 
	 (setf blowing nil)
	 (setf attackrate .0005))))))

(definstrument clarinet (beg dur freq amplitude &optional (maxa 1.0))
  (let* ((lowestfreq 100.0)
	 (len (1+ (floor (/ *srate* lowestfreq))))
	 (delayline (make-delayl len))
	 (reedtable (make-reed :offset 0.7 :slope -0.3))
	 (filter (make-onez))
	 (blowing t)
	 (breathpressure 0.0) ; 0.1 ?
	 (rate .001)
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (floor (* *srate* beg)))
	 (nd (+ st (floor (* *srate* dur))))
	 (release (+ st (floor (* .8 (- nd st))))))
    (set-delayl delayline (- (* 0.5 (/ *srate* freq)) 1.0))
    (run
     (loop for i from st to nd do
       (let ((pressurediff 0.0))
	 (if blowing
	     (if (/= maxpressure breathpressure)
		 (if (< breathpressure maxpressure)
		     (incf breathpressure attackrate)
		   (decf breathpressure attackrate)))
	   (if (> breathpressure 0.0)
	       (decf breathpressure attackrate)))
	 (setf pressurediff (- (one-zero filter (* -0.95 (dll-output delayline))) breathpressure))
	 (outa i (* amplitude (delayl delayline (+ breathpressure (* pressurediff (reedtable reedtable pressurediff))))))
	 (when (= i release)
	   (setf blowing nil)
	   (setf attackrate .0005)))))))

(definstrument flute (beg dur freq amplitude &optional (maxa 1.0))
  (let* ((lowestfreq 100.0)
	 (len (1+ (floor (/ *srate* lowestfreq))))
	 (jetdelay (make-delayl (floor len 2)))
	 (boredelay (make-delayl len))
	 (filter (make-onep))
	 (dcblocker (make-dcb))
	 (jetrefl 0.6)
	 (endrefl 0.6)
	 (sinphase 0.0)
	 (blowing t)
	 (rate .0005)
	 (breathpressure 0.0) ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (floor (* *srate* beg)))
	 (nd (+ st (floor (* *srate* dur))))
	 (release (+ st (floor (* .8 (- nd st))))))
    (set-pole filter 0.8)
    (set-gain filter -1.0)
    (let ((ratio 0.8)
	  (temp (- (/ *srate* freq) 5.0)))
      (set-delayl boredelay (* ratio temp))
      (set-delayl jetdelay (* temp (- 1.0 ratio))))
    (run
     (loop for i from st to nd do
       (let ((randpressure (* 0.1 breathpressure (random 1.0)))
	     (temp 0.0) 
	     (pressurediff 0.0))
	 (incf sinphase 0.0007)		;5 hz vibrato?
	 (if (> sinphase 6.28) (decf sinphase 6.28))
	 (incf randpressure (* 0.05 breathpressure (sin sinphase)))
	 (if blowing
	     (if (/= maxpressure breathpressure)
		 (if (< breathpressure maxpressure)
		     (incf breathpressure attackrate)
		   (decf breathpressure attackrate)))
	   (if (> breathpressure 0.0) (decf breathpressure attackrate)))
	 (setf temp (dcblock dcblocker (onepole filter (dll-output boredelay))))
	 (setf pressurediff (+ (jettable (delayl jetdelay (+ breathpressure (- randpressure (* jetrefl temp))))) (* endrefl temp)))
	 (outa i (* 0.3 amplitude (delayl boredelay pressurediff)))
	 (when (= i release)
	   (setf blowing nil)
	   (setf attackrate .0005)))))))


#|
      (with-sound () 
	(plucky 0 .3 440 .2)
	(bow-1 .5 .3 220 .2)
	(brass 1 .3 440 .2)
	(clarinet 1.5 .3 440 .2)
	(flute 2 .3 440 .2))
|#
