;;; ********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

;;; sample parsing utilities for clm-3. see end of file for example.
;;; Some public domain samples are available from UIOWA's sample
;;; database at http://theremin.music.uiowa.edu/MIS.html
;;;
;;; (PARSE-SAMPLES file start window on off &KEY chnnel dur
;;;                min-steady min-duration)
;;; Returns a list of the starting and ending times of instrument
;;; samples (tones) stored a single audio file. The beginning and
;;; ending of each tone are found by comparing specified on/off amp
;;; thresholds to a windowed amplitude average in the file.  positions
;;; of false on/off triggerings are returned as second value.
;;;   file           the sound file to parse samples from.
;;;   start          start time in file for parsing.
;;;   window         window length in seconds for amp averaging.
;;;   on             on amp threshold for sample start.
;;;   off            off amp threshold for sample end.
;;;   :channel       the audio channel to parse.
;;;   :dur           optional duration in file to parse.
;;;   :min-steady    a minimum time after On before finding off
;;;   :min-duration  a minimum duration for a sample to be valid.
;;;
;;; (PLAY-SAMPLES file samps &rest nums)
;;; Previews samples in file before cutting them.
;;;   file           the sound file to play samples in
;;;   samps          sample info list returned by parse-samples
;;;   nums           the sample numbers (1 based) in samps to play
;;;
;;; (CUT-SAMPLES file samps outdit &KEY from to offset sample-steps
;;;              env prefix dbeg dend names)
;;; REQUIRES: clm-3/fullmix.ins
;;; Generates a population of sample files given an audio input file,
;;; a list of samples positions returned by parse-samples and an
;;; output directory. Files are named as notes generated from a
;;; specfied keynumber offset. For example an :offset of 60 would
;;; generate sample files: "c4.aiff", "cs4.aiff", "d4.aiff" ...
;;;   file           the sound file to cut samples from
;;;   samps          sample info (list) returned by parse-samples
;;;   outdir         the directory to hold the samples
;;;   :from          starting index in samps to write (default 0)
;;;   :to            ending index in samps to wite (default end)
;;;   :offset        starting keynumber for sample files (default 0)
;;;   :sample-steps  number of chromatic steps between each sample
;;;   :env           envelope to apply to each cut sample file
;;;   :prefix        opional prefix for each sample file
;;;   :dbeg          add/remove constant start offset to each samples
;;;   :dend          add/remove constant end offset to each samples
;;;   :names         force sample file names to list of names.
;;;

(in-package :cm)

#-clm
(eval-when (:compile-toplevel :load-toplevel)
  (error "Attempt to compile or load samples.lisp without CLM loaded."))

(defun parse-samples (file start window on off 
		      &key (channel 0)
		      dur
		      (min-duration most-negative-fixnum)
		      (min-steady 0)
		      (trace t)
		      &aux (file? (probe-file file))
		      (max (and file? (sound-duration file)))
		      times (bad '())
		      )
  (when file?
    (unless dur (setf dur max))
    (let* ((*srate* (sound-srate file))
	   (beg (seconds->samples start))
	   (len (seconds->samples window))
	   (end (seconds->samples (min (+ beg dur) max)))
	   (file-array (make-array end))
	   (std (seconds->samples min-steady))
	   (buf (make-array len))	; window for amp averaging
	   (sum 0.0)
	   (val 0.0)
	   (avr 0.0)
	   (j 0)
	   (k 0)			; was -1
	   (on? nil)
	   (middle 0)
	   (ontime 0.0)
	   (offtime 0.0))

      ;; fills file-array according to the specified channel
      (if (= channel 0)
	  (file->array file 0 0 end file-array)
	  (file->array file 1 0 end file-array)
	  )
  
      ;; fill buf with first window
      (loop while (< beg end)
	 repeat len
	 do
	 (setf val (abs (elt file-array j)))
	 (incf sum val)
	 (setf (elt buf j) val)
	 (incf j)
	 (incf beg))
        
      (setf j 0)

      (loop while (< beg end)
	 do
	 (setf avr (/ sum len))		; test average
	 (if (not on?)
	     (if (>= avr on)
		 (progn
		   (setf ontime (samples->seconds (1+ (- beg len))))
		   (setf middle (+ beg std))
		   (when trace
		     (format t "~%~3d ~F" (incf k)
			     ontime))
		   (setf on? t))
		 nil)
	     (if (and (>= beg middle) (<= avr off))
		 (let (sdur)
		   (setf offtime (samples->seconds (- beg len)))
		   (setf sdur (- offtime ontime))
		   (when trace
		     (format t " : ~F (~F" offtime sdur))
		   (when (< sdur min-duration)
		     (when trace
		       (format t "    too short!"  ))
		     (push k bad))
		   (when trace
		     (format t ")"))
		   (push (list (* 1.0 ontime) (* 1.0 offtime) 
			       (* 1.0 sdur)) times)
		   (setf on? nil))
		 nil)
	     )
	 (decf sum (elt buf j))		       ; subract earliest 
	 (setf val (abs (elt file-array beg))) ;; get new value
	 (setf (elt buf j) val)		; store it
		
	 (incf sum val)			; add in new value
	 (incf j)
	 (if (= j len) (setf j 0))
	 (incf beg)
	 )
      )
    (if (or times bad)
	(if bad
	    (values (nreverse times) (nreverse bad))
	    (values (nreverse times)))
	nil)
    )
  )

(defun samples->seconds (s)
  (/ s (coerce *srate* 'float)))

#+(and clm2 (not clm3))
(defun snd-maxamp (f)
  ;; return amp, time and channel of max amp
  (let* ((file (namestring (truename f )))
         (arry (clm:make-integer-array (* 2 (clm:sound-chans f)))))
    (clm:sound-maxamp file arry)
    (let ((*srate* (sound-srate file))
          (win 0)
          (loc (elt arry 0))
          (val (clm::fix-to-real (elt arry 1))))
      (loop for i from 0 below (length arry) by 2
         for v = (clm::fix-to-real (elt arry (+ i 1)))
         do 
           (when (> v val)
             (setf win i)
             (setf loc (elt arry i))
             (setf val v)
             ))
      (values val (samples->seconds loc) win))))

#+clm3
(defun snd-maxamp (f)
  ;; return amp, time and channel of max amp
  (let* ((file (namestring (truename f )))
         (*srate* (clm:sound-srate file))
         (chans (clm:sound-chans file))
	 (vals (clm:make-double-array chans))
	 (times (clm:make-integer-array chans))
         (m most-negative-fixnum )
         j )
    (clm:sound-maxamp file chans vals times)
    (loop for i below chans 
       for v = (abs (aref vals i))
       when (> v m)
       do (setq m v j i))
    (values m (samples->seconds (aref times j)) j)))

(defun snd-ampinfo (file &optional (start 0) stop (channel 0))
  ;; return average, max and sample len from region of soundfile.
  (let ((fil (open-input* file)))
    (unwind-protect 
         (let* ((*srate* (sound-srate file))
                (beg (seconds->samples start))
                (end (seconds->samples (or stop
                                           (sound-duration file))))
                (sum 0.0)
                (max most-negative-single-float)
                ;(min most-positive-single-float)
                (val 0.0))
           (loop for i from beg below end
              do
                (setf val (abs (if (= channel 0)
                               (ina i fil)
                               (inb i fil))))
                (if (> val max) (setf max val))
                ;(if (< val min) (setf min val))
                (incf sum val))
           (values max (/ sum (- end beg)) (- end beg)))
      (close-input fil))))

;;;
;;; Sample list munging (playing, cutting etc)
;;;

(defmacro sampbeg (n) `(first ,n))
(defmacro sampend (n) `(second ,n))
(defmacro sampdur (n) `(or (third ,n)
                           (- (second ,n) (first ,n))))

(defun cut-samples (file samps outdir &key
                    from to (offset 21) (sample-steps 1)
                    (prefix "")
                    (dbeg 0) (dend 0) ;; add/remove constant amount
                    names)
  (unless (probe-file file)
    (error "Sample file ~S does not exist!" file))
  (unless (probe-file outdir)
    (error "Destination directory ~S does not exist!" outdir))
  (when names
    (unless (= (length names) (length samps))
      (error "Samps length and names disagree.")))
  (unless from (setf from 1))		; 1 based
  (unless to (setf to (length samps)))
  (format t "~%Cutting ~S samples from ~A:" (length samps) file)
  (loop	for i from from to to by sample-steps
     for note = (elt samps (1- i))
     for outfile = (format nil "~a~(~a~a~).aiff" 
			   outdir
			   prefix
			   (if names
			       (elt names (1- i))
			       (note (+ offset (1- i)))))
     do
     (let ((beg (+ (sampbeg note) dbeg))
	   (end (+ (sampend note) dend)))
       (unless (> end beg) 
	 (error "End value ~S less than start ~S." end beg))
       (format t "~%~3D (~7f ~7f) -> ~S"
	       i beg end outfile)
       (cut-sample file beg end outfile))))

(defun cut-sample (file beg end outfile &key play)
  (unless (find "FULLMIX" clm:*clm-instruments* :test #'equal :key #'string)
    (error "cut-samples: clm-3/fullmix.ins is not loaded, can't cut anything."))
  (let* ((afile (namestring file))
         (srate (clm:sound-srate afile))
         (chans (clm:sound-chans afile)))
    (with-sound (:output outfile :srate srate :channels chans
                             :play play)
        (let* ((*srate* srate)
               (frame1 (seconds->samples beg))
               (frame2 (seconds->samples end)))
          (mix afile :input-frame frame1 :output-frame 0 
	       :frames (- frame2 frame1))))))
 
(defun remsamps (samps bad)
  (loop for n in samps
        for i from 1 ; 1 based
        unless (member i bad)
        collect n))

;(defun wait-dac (filename)
;  (ccl:run-program "/bin/csh" (list "-fc"
;                                    (format nil "~A ~A"
;                                            (or *clm-player* "sndplay")
;                                            filename))
;                   :output t :wait t))

(defun play-sample (f samps n &key (dbeg 0) (dend 0) (trace t)
                    (wait t) name)
  (unless name (setf name "/tmp/cut.aiff"))
  (let ((samp (elt samps (- n 1))))   ; sample number N is 1 based
    (when samp
      (when trace
        (format t "~3D: ~S to ~S.~%"
                n (first samp) (second samp))
        (force-output t))
;      #+clm3
      (dac f :start (+ (first samp) dbeg)
           :end (+ (second samp) dend)
           :wait wait)
;      #+(and clm2 (not clm3))
;      (progn
;        (cut-sample f 
;                    (+ (first samp) dbeg)
;                    (+ (second samp) dend)
;                    name :play nil)
;        (if wait
;          (begin (wait-dac name)
;                 (delete-file name))
;          (dac name)))
      (values))))

(defun play-samples (file samps &rest nums)
  (format t "File: ~a~%" file)
  ;; for i from 0 below (length samps)
  (loop for i from 1 to (length samps)
       for n = (format nil "/tmp/cut-~(~a~).aiff" (gentemp ))
       when (or (null nums) (member i nums))
       do (play-sample file samps i :wait t :name n)))


#|
;;; Parsing flute samples from the UIOWA Sample database Download
;;; these from "http://theremin.music.uiowa.edu/MIS.html"

;;; compile/load this file
(cload "samples")

;; Make one of the IOWA flute files our input
(defparameter file "flute-vib-ff-B3B4.aiff")

;; set our output sample DIRECTORY:
(defparameter dest "/tmp/")

;; now parse the audio samples....

(parse-samples file  0 .2 .005 .001 :min-steady .5 )

;;; ...to generate this list, each element is (start end dur)
;;; of one sample in the audio file.

(setq raw
      '((0.33181405 2.2267573 1.8949432) (2.9189796 5.1050115 2.1860318)
        (5.7282314 8.0012245 2.272993) (8.615918 10.832699 2.2167807)
        (11.32907 13.797574 2.468504) (14.3455105 16.606236 2.260725)
        (17.18619 19.522947 2.3367577) (20.122313 22.445396 2.3230839)
        (23.04882 25.434807 2.3859863) (25.962925 28.273085 2.3101597)
        (28.81306 31.18186 2.3687992) (31.81127 34.28703 2.4757595)
        (34.83465 37.129387 2.2947388)))

;; preview the samples.

(play-samples file raw)

;; cut audio samples into the audio destination with names offset from
;; keynum 59 (b3)

(cut-samples file raw dest :offset 59)
||#


#|
(cd "/Lisp/dump/")
(make-file->sample "test.aiff")
(sound-chans "test.aiff")
(file->sample "test.aiff" 0)  this doesn't work outside of 'run'

(defparameter test-array-1 (make-array 101))
(defparameter test-array-2 (make-array 101))

;; (file->array file chan start samples arr)
;; 1. if you try to fill less than the whole array, you get garbage
;; 2. if you try to fill the array with stuff beyond the end of the file, you
;;    get garbage and/or zeros (as you should)
;; 3. if you try to fill the array with more than can fit, you get hosed bad
(file->array "test-1.aiff" 0 0 101 test-array-1)
(file->array "test-2.aiff" 0 0 101 test-array-2)
(pprint test-array-1)
(pprint test-array-2)

(sound-srate "test-1.aiff")
(sound-srate "test-2.aiff")

(sound-frames "test-1.aiff")
(sound-frames "test-2.aiff")

(sound-length "test-1.aiff")
(sound-length "test-2.aiff")

(* 44100 (sound-duration "test-1.aiff"))
(* 44100 (sound-duration "test-2.aiff"))

(sound-samples "test-1.aiff")
(sound-samples "test-2.aiff")

(length test-array-1)
(length test-array-2)
||#