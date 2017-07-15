;;; **********************************************************************
;;; VKEY instrument for clm-3 with modular backend rendering. includes
;;; VKEY-DB, a function that creates vkey sample databases from
;;; directories of sample sound files. the default backends requre
;;; the following instruments to be loaded:
;;; "fullmix.ins" (clm-3 distribution)
;;; "expandn.ins" michael klingbeil's expansion ins with dynamic
;;;               support for multi channel io and reverb
;;;
;;; (VKEY time duration keynum samples &KEY base amplitude amp-env
;;;       normalize reverb input-file-start-time degree backend
;;;       expand object)
;;; time {number}
;;;     start time in seconds
;;; duration {number | nil}
;;;     duration in seconds. clipped so as not to exceed file length
;;;     taking into account srate change and input start time. defaults
;;;     to the length of the soundfile.
;;; keynum {number}
;;;     MIDI keynumber or SRC ratio (see samples)
;;; samples {list | symbol | string}
;;;     a sample database list or a symbol whose value is a sample
;;;     database list or a string pathname of a single sample
;;;     soundfile.  if the latter, then keynum is interpreted as a
;;;     key number if :base is set, or as a SRC ratio if :base is not
;;;     set. for example:
;;;       (vkey 0.0 2.0 72.0 "sounds:guitarC4.aiff" :base 60.0)
;;;       (vkey 0.0 2.0 2.0 "sounds:guitarC4.aiff")
;;;     both play guitarC4.aiff transposed ane octave up 
;;; :base {knum}
;;;     base MIDI key number for a single sound file sample (see 
;;;     explanation under samples)
;;; :amplitude {number}
;;;     an amplitude scaler on the soundfile. defaults to 1.
;;; :amp-env {list}
;;;     envelope shape to apply to the note
;;; :normalize {boolean}
;;;     scales amplitude by normalization factor in sample database. 
;;;     see doc on vkey-db for more information.
;;; :bend {list}
;;;     an envelope on srate 
;;; :reverb {number}
;;;     reverb amount or nil for no reverb
;;; :input-file-start-time {number}
;;;     where to start in the input sample
;;; :degree {number}
;;;     panning, uses locsig to determine the correct panning values.
;;;     should be able to handle n input channels and m output 
;;;     channels correctly.
;;; :backend {symbol | list}
;;;     a symbol naming the backend sound rendering method. If a list
;;;     then the rest of the list is passed as the "data" parameter to
;;;     vkey-render. if :backend is not specified it defaults to
;;;     either *vkey-expand* or *vkey-mix* depending on the value of
;;;     :expand
;;; :expand {number}
;;;     an optional expansion factor passed to the backend.
;;; :object {instance}
;;;     an object to pass to vkey-render along with data
;;;
;;; (VKEY-DB spec &KEY keyfn type neighbors)
;;; builds a sample database from a specified directory of samples:
;;; spec {directory | list}
;;;     a directory string or list of sound files to parse
;;; :keyfn
;;;     if specified, a function of one argument that returns a
;;;     sample's key number for each soundfile (string) passed to
;;;     it. The default :keyfn function assumes the files are note
;;;     names or keynums in the standard chromatic scale, ie 60.aiff,
;;;     c4.aiff, ef2.aiff, gs5.aiff, and so on.
;;; :type
;;;     the file extension of sound files in directory.
;;; :neighbors
;;;     the number of neighbor files on either side of current sample
;;;     file whose amps will be averaged to determine an amplitude
;;;     normalizing coeff. this value can  be used by vkey to smooth
;;;     amplitude differences between adjacent samples.
;;;
;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2005/10/16 22:15:44 $

(in-package :cm)

#-clm
(eval-when (:compile-topelevel :load-toplevel)
  (error "Attempt to compile or load VKEY without CLM  loaded."))

(defparameter *vkey-samples* (list))
(defparameter *vkey-mix* 'fullmix)    ; default mixing backend 
(defparameter *vkey-expand* 'expandn) ; default expansion backend
(defparameter *vkey-normalize* t)     ; default normalization

(defclass vkey (event)
  ((duration :initarg :duration :initform nil :accessor vkey-duration)
   (keynum :initarg :keynum :initarg :ratio :initform nil
           :accessor vkey-keynum)
   (amplitude :initarg :amplitude :initform nil :accessor vkey-amplitude)
   (amp-env :initarg :amp-env :initform nil :accessor vkey-amp-env)
   (normalize :initarg :normalize :initform t :accessor vkey-normalize)
   (degree :initarg :degree :initform nil :accessor vkey-degree)
   (reverb :initarg :reverb :initform nil :accessor vkey-reverb)
   (input-file-start-time :initarg :input-file-start-time
                          :initform nil
                          :accessor vkey-input-file-start-time)
   (samples :initarg :samples :accessor vkey-samples)
   (base :initarg :base :initform nil :accessor vkey-base)
   (bend :initarg :bend :initform nil :accessor vkey-bend)
   (expand :initarg :expand :initform nil :accessor vkey-expand)
   (backend :initarg :backend :initform nil :accessor vkey-backend)
   ))

(defmethod write-event ((obj vkey) (score audio-file) scoretime)
  (when (eql (audio-file-output-trace score) t)
    (format t "~a ~s..." (object-name obj) (decimals scoretime 3)))
  (vkey scoretime (vkey-duration obj) (vkey-keynum obj)
        (vkey-samples obj)
        :amplitude (or (vkey-amplitude obj) 1.0)
        :amp-env (vkey-amp-env obj)
        :normalize (or (vkey-normalize obj)
                       *vkey-normalize*)
        :degree (vkey-degree obj)
        :reverb (vkey-reverb obj)
        :base (vkey-base obj)
        :bend (vkey-bend obj)
        :input-file-start-time (or (vkey-input-file-start-time obj)
                                   0.0)
        :expand (vkey-expand obj)
        :backend (vkey-backend obj)
        :object obj))

;;; vk holds information about a single sample file.

(defstruct (vk (:type list))
  key    ; keynumber
  fil    ; file on disk
  dur    ; duration of file
  amp    ; maxmap of file
  avr    ; norm factor based on neighborhood average
  chn)   ; nchans of file

(defun vkey (time duration keynum samples &key (amplitude 1.0)
             amp-env reverb (input-file-start-time 0.0) expand
             degree bend base (normalize *vkey-normalize*)
             backend object)
  (let ((samps (or samples *vkey-samples*))
        maxdur srate in-chans out-chans file)
    (cond ((or (stringp samps) (pathnamep samps))
           ;; samples is a filename. sound-duration cant
           ;; handle logical pathnames
           (setf file (namestring (truename samps)))
           (setf maxdur (sound-duration file))
           (setf srate
                 (if base
                   (/ (hertz keynum) (hertz base)) ; base=sample's keynum
                   keynum)))                       ; keynum is a ratio
          (t
           ;; value is symbol, list or vkey-db
           (let ((db (cond ((and (symbolp samps) (not (null samps)))
                            (symbol-value samps))
                           ((consp samps) 
                            samps)
                           (t
                            (error "Illegal vkey samples value: ~S."
                                   samps))))
                 vk)
             ;; find closest key and determine appropriate
             ;; srate shift of it for achiveing keynum
             (setf keynum (keynum keynum))
             (setf vk (closest-key keynum db))
             (setf srate (expt 2 (/ (- keynum (vk-key vk)) 12.0)))
             (setf file (vk-fil vk))
             (setf maxdur (vk-dur vk))
             (setf in-chans (vk-chn vk))
             (if normalize
               (setq amplitude (* amplitude (vk-avr vk))))
             )))
    
    ;; TODO: warn if out of range of sample database
    ;; fix up duration not to exceed file length (taking
    ;; into account srate change and input start)
    (when (> input-file-start-time maxdur)
      (warn "input-file-start-time greater than length of sound file!"))
    (setf maxdur (/ (- maxdur input-file-start-time) srate))
    (if expand (setf maxdur (* maxdur expand)))
    (if (not duration)
      (setf duration maxdur)
      (setf duration (min duration maxdur)))
    ;; when doing matrixing we need to know the number of channels in
    ;; and out
    (when (or degree amp-env)
      (unless in-chans (setf in-chans (clm:sound-chans file)))
      (setf out-chans (clm:mus-channels *output*)))
    (let ((meth (if (consp backend) (car backend) backend))
          (data (if (consp backend) (cdr backend) (list))))
      ;; if :backend is not specified then default to *vkey-expand* if
      ;; :expand is set else use *vkey-mix*
      (vkey-render (or meth (if expand *vkey-expand* *vkey-mix*))
                   time (namestring (truename file))
                   input-file-start-time duration srate bend
                   amplitude amp-env in-chans out-chans expand 
                   degree reverb data object))))

;;;
;;; backend rendering. Three are predefind: fullmix, expandn and expsrc.
;;; maybe the args should just be &rest...

(defmethod vkey-render ((backend (eql 'fullmix)) start file
                        filebeg duration srate bend amp amp-env in-chans
                        out-chans expand degree reverb data obj)
  ;; regular mix, no expansion
  bend obj data expand
  (funcall backend file start duration (or filebeg 0.0)
           (if degree
               (degree->matrix degree in-chans out-chans amp amp-env)
               (if amp-env 
                   (ampenv->matrix amp-env in-chans out-chans amp)
                   amp))
           (or srate 1)
           reverb))

(defmethod vkey-render ((backend (eql 'expandn)) start file
                        filebeg duration srate bend amp amp-env in-chans
                        out-chans expand degree reverb data obj)
  ;; expansion/contraction factor or nil for no expansion
  ;; note that sample rate conversion (pitch change) happens 
  ;; *after* expansion so extreme pitch changes will result in
  ;; much larger duration between grains from the expand generator
  obj
  (when (consp bend) ; convert bend to srate env
    (setq srate (loop for (x y) on bend by #'cddr
                   collect x collect (* y srate))))
  (funcall backend start duration file amp
           :expand expand
           :ramp (getf data ':ramp .2)
           :seglen (getf data ':seglen .15)
           :srate srate
           :hop (getf data ':hop .04)
           :amp-env (or amp-env '(0 1 100 1))
           :input-start filebeg
           :grain-amp (getf data ':grain-amp .9)
           :matrix (if degree
                     (degree->matrix degree in-chans out-chans 1 nil)
                     nil)
           :reverb reverb))

(defmethod vkey-render ((backend (eql 'expsnd)) start file
                        filebeg duration srate bend amp amp-env in-chans
                        out-chans expand degree reverb data obj)
  ;; data hold optionals: (ramp seglen sr hop ampenv)
  bend filebeg in-chans out-chans degree reverb obj
  (funcall backend file start duration amp expand 
           (getf data ':ramp)
           (getf data ':seglen)
           srate
           (getf data ':hop)
           amp-env))

;;;
;;; helpers
;;;

(defun degree->matrix (degree in-chans out-chans scaler-amp amp-env)
  ;; deal with panning, use locsig to get current values.
  (let ((loc (clm:make-locsig degree :channels out-chans)))
    ;; check for amplitude values that are neary zero
    ;; but not quite due to trig roundoff
    (loop for i from 0 below out-chans do
         (if (< (abs (locsig-ref loc i)) 1e-10)
           (setf (locsig-ref loc i) 0.0)))
    (loop for inp from 0 below in-chans
       collect
       (loop for outp from 0 below out-chans
          collect
          (let ((a (locsig-ref loc outp)))
            (setf a (* a scaler-amp))
            ;; if the input channel is not going to
            ;; its corresponding original output channel
            ;; scale back gain slightly to maintain some
            ;; sense of the original "spread" of the 
            ;; input sound
            (if (/= (mod inp out-chans)
                    (mod outp in-chans))
              (setf a (* a (expt 2 -0.5))))
            (if (and amp-env (/= 0 a))
              (loop for (x y) on amp-env by #'cddr
                 collect x collect (* y a))
              a))))))

(defun ampenv->matrix (amp-env in-chans out-chans scaler-amp)
  ;; no panning but ampenv
  (let ((env (loop for (x y) on amp-env by #'cddr
                collect x collect (* y scaler-amp))))
    (loop for inp from 0 below in-chans
       collect
       (loop for outp from 0 below out-chans
          if (= (mod inp out-chans)
                (mod outp in-chans))
          collect env else collect 0.0))))

(defun closest-key (keynum vkdb)
  ;; return closest vk in vkdb to keynum, which can be a float
  (let (best bestkey)
    (loop for key in vkdb
          for diff = (abs (- (vk-key key) keynum))
          if (or (not best) (<= diff best))
          do (setf best diff bestkey key)
          until (>= (vk-key key) keynum))
    bestkey))

(defun sound-max-maxamp (f)
  ;; get maxamp of all channels.
  (let* ((file (namestring (truename f )))
         (chans (clm:sound-chans file))
	 (vals (clm:make-double-array chans))
	 (times (clm:make-integer-array chans)))
    (clm:sound-maxamp file chans vals times)
    (loop for i below chans
          maximize (elt vals i))))

;;; sample database creation. spec is directory or list of files.

(defun vkey-db (spec &key keyfn (type "aiff")  (neighbors 2))
  (flet ((filetokeynum (f)
           (let* ((n (pathname-name f))
                  (x (read-from-string n)))
             (if (symbolp x)
               (keynum x)
               (if (integerp x)
                 x nil)))))
    (unless keyfn (setf keyfn #'filetokeynum))
    (let ((db ())
          (max 0))
      (loop for f in (if (consp spec) spec
                         (directory (merge-pathnames
                                     (format nil "*.~a" type)
                                     spec)))
         for n = (namestring f)
         for k = (funcall keyfn (pathname-name f))
         do
         (if k
           (progn
             (push (make-vk :key k
                            :fil (namestring 
                                  (if (consp spec) 
                                    f
                                    (merge-pathnames
                                     (format nil "~a.~a"
                                             (pathname-name f)
                                             (pathname-type f))
                                     spec)))
                            :dur (sound-duration n) 
                            :amp (coerce (sound-max-maxamp n) 'single-float)
                            :avr NIL
                            :chn (sound-chans n)) 
                   db)
             (incf max))
           (format t "~%Skipping ~s, can't convert ~s to keynum."
                   f (pathname-name n))))
      (setf db (sort db #'< :key #'first))
      (decf max)
      (unless (or (not neighbors) (eql neighbors 0))
        ;; compute normalization factor for each file based on the
        ;; average amp of +- neighbor files.
        (loop with sum 
           for n in db
           for i from 0
           do
           (setf sum 0.0)
           (loop for j from (- i neighbors) to (+ i neighbors)
              do
              (let ((k (if (< j 0) 
                         (mod j (1+ (* neighbors 2)))
                         (if (> j max) 
                           (- max (- j max))
                           j))))
                (incf sum (vk-amp (elt db k)))))
           (setf (vk-avr n)
                 ;; divide avr amp of neighborhood by amp
                 (coerce (/ (/ sum (1+ (* neighbors 2)))
                            (vk-amp n))
                         'single-float))))
        db)))


