;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 92, 93, 94, 98, 99, 2000, 2001 Fernando Lopez Lezcano. 
;;; All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as 
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice. Suggestions, comments and bug reports are welcome. Please 
;;; address email to: nando@ccrma.stanford.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic multichannel three-dimentional signal locator
;;; (wow that sound good! :-)
;;;
;;; by Fernando Lopez Lezcano
;;;    CCRMA, Stanford University
;;;    nando@ccrma.stanford.edu
;;;
;;; Thanks to Juan Pampin for help in the initial coding of the new version
;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
;;; for insights into the Ambisonics coding and decoding process. 
;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

;;; CHANGES:
;;; 04/29/2002: fixed reverb envelopes for no reverb under clisp
;;; 01/14/2001: added multichannel reverb output with local and global control
;;;             in the reverberator (the hrtf code is currently not merged into
;;;             this version). Reverb-amount can now be an envelope. Ambisonics
;;;             now outputs signal to the reverb stream. 
;;; 02/05/2000: . don't compile as part of the clm package, import symbols
;;;             . switched over to clm-2, otherwise convolve HAS to operate
;;;               on a file, we want convolve to process the output of the 
;;;               doppler delay line (for the hrtf code)
;;; 02/03/2000: started working on hrtf's
;;; 01/15/2000: rewrote transform-path code to account for 3d paths
;;; 01/13/2000: . changed order of b-format output file to W:X:Y:Z from X:Y:Z:W
;;;             . plot method would bomb with paths that had constant velocity,
;;;               fixed norm function
;;;             . added make-literal-path and friends to enable to create
;;;               paths with user supplied coordinates
;;;             . decoded-ambisonics was rotating sounds in the wrong direction
;;;             . in change-direction: only check for change if both points are
;;;               different (intersect-inside-radius can create a redundant point)
;;; 11/28/1999: decoded-ambisonics is now working for N channels
;;;             includes reverberation send
;;; 11/27/1999: set minimum segment distance for rendering, otherwise for long
;;;             lines the amplitude envelope does not reflect power curve. 
;;; 11/26/1999: added code to check for intersection with inner radius
;;;             fixed nearest-point to handle trivial cases

;;; TODO:
;;; 01/21/2001: fix envelope generated for mono reverberation stream.
;;;             change input and output to use frames and mixers
;;;             fix supersonic movement warning code
;;; > add warnings when object goes outside of area covered by speakers
;;; > fix one common vertice case of 3 speaker group transitions
;;; > redo the old code for multiple images (reflections in a rectangular room)
;;;     a bit of a pain, would have to add z (ceiling and floor reflections)
;;;     would be better to find general purpose code for non-rectangular rooms
;;; > we really need a good N-channel reverb [fixed with freeverb]
;;; > change reverb to be multichannel, add local and global reverb
;;;   11/24/1999: should use a waveguide reverb like pph@ccrma project
;;;               would be a good idea to inject the signal through a center
;;;               injection point that moves inside the virtual cube, more like
;;;               a physical model of what actually happens in a room
;;; | add ambisonics back-end
;;;   11/24/1999: code to b-format sort of working
;;;                 how to deal with the inner space and 0:0:0?
;;;               decoded format not working if we incorporate distance att
;;;                 formulas are wrong...
;;; > add hrtf back-end
;;; > extract a subpath from an existing path
;;; > recode the transformation functions
;;; > add arcs of circles and other basic geometric paths
;;;     make it so that you can concatenate them...
;;; | 11/25/1999 fix the "diagonal case" (sounds go through the head of the listener)

;;; We use a couple of internal functions in the clm package: export them...

(eval-when (compile)
  (let* ((current *package*))
    (in-package :clm)
    (export '(clm::def-optkey-fun clm::x-norm)) ; shouldn't be needed
    (setf *package* current)))

;;;;;;;;;;;;;;;;;;;;;
;;; Global Parameters

;;; Define the base in which all angles are expressed
(defparameter dlocsig-one-turn 360)

(defun one-turn-is (unit)
  (setf dlocsig-one-turn unit))

(defun angles-in-degree ()
  (one-turn-is 360))

(defun angles-in-radians ()
  (one-turn-is (* 2 pi)))

(defun angles-in-turns ()
  (one-turn-is 1))

;; speed of sound in air, in meters per second under normanl conditions
(defparameter dlocsig-speed-of-sound 344)

(defun distances-in-meters ()
  (setf dlocsig-speed-of-sound 344))

(defun distances-in-feet ()
  (setf dlocsig-speed-of-sound 1128))

;; default for whether to use two or three-dimensional speaker configurations
(defparameter dlocsig-3d nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speaker Configuration

(defstruct group
  id
  ;; size of the group
  size
  ;; points that define the group
  vertices
  ;; speakers or output channels
  speakers
  ;; inverse matrix used to calculate gains
  matrix)

(defstruct speaker-config
  ;; number of speakers, one based
  number
  ;; dimensionality of speaker arrangement
  dimension
  ;; list of polar coordinates for all speakers
  coords
  ;; groups of speakers
  groups
  ;; delays for speakers
  delays
  ;; mapping of speakers to output channels
  map)

;;; Create a speaker configuration structure based on a list of speakers
;;;
;;; speakers:  list of angles of speakers with respect to 0
;;; delays:    list of delays for each speaker, zero if nil
;;; distances: list relative speaker distances, 
;;;            (instead of delays)
;;; map:       mapping of speakers to output channels
;;;            content should be output channel number, zero based

(def-optkey-fun  arrange-speakers ((speakers '())
				   (groups '())
				   (delays '())
				   (distances '())
				   (map '()))
  ;; sanity checking of configuration
  (if (< (length speakers) 1)
      (error "a speaker configuration must have at least one speaker!"))
  (if groups
      (let* ((first-len (length (first groups))))
	(loop 
	  for group in groups do
	  (if (/= (length group) first-len)
	      (error "all groups must be of the same length! (~s)" first-len))))
    ;; if the speakers are defined with only azimut angles (no elevation)
    (if (not (listp (first speakers)))
	;; then create the groups ourselves because it is a 2d configuration;
	;; we could create the 3d configuration groups but the algorithm for
	;; doing that in the generic case is not trivial
	(setf groups (loop
		       with len = (length speakers)
		       repeat (length speakers)
		       for i from 0 
		       for j from 1
		       collect (if (/= len 1)
				   (list i (if (< j len) j 0))
				 (list 0))))))
  (if (null groups)
      (error "no groups specified, speakers must be arranged in groups"))
  (if (and delays distances)
      (error "please specify delays or distances but not both"))
  (if (not (null delays))
      (cond ((> (length speakers)(length delays))
	     (error "all speaker delays have to be specified, only ~s supplied [~s]" 
		    (length delays) delays))
	    ((< (length speakers)(length delays))
	     (error "more speaker delays than speakers, ~s supplied instead of ~s [~s]"
		    (length delays) (length speakers) delays))))
  (if (not (null delays))
      (loop for delay in delays do
	(if (< delay 0.0)(error "delays must be all positive, ~s is negative" delay))))
  (if (not (null distances))
      (cond ((> (length speakers)(length distances))
	     (error "all speaker distances have to be specified, only ~s supplied [~s]" 
		    (length distances) distances))
	    ((< (length speakers)(length distances))
	     (error "more speaker distances than speakers, ~s supplied instead of ~s [~s]"
		    (length distances) (length speakers) distances))))
  (if (not (null distances))
      (loop for delay in distances do
	(if (< delay 0.0)(error "distances must be all positive, ~s is negative" delay))))
  (if (not (null map))
      (cond ((> (length speakers)(length map))
	     (error "must map all speakers to output channels, only ~s mapped [~s]" 
		    (length map) map))
	    ((< (length speakers)(length map))
	     (error "trying to map more channels than there are speakers, ~s supplied instead of ~s [~s]"
		    (length map) (length speakers) map))))

  (flet (;; invert a 3x3 matrix using cofactors
	 (invert3x3 (mat)
           (let* ((m (make-array '(3 3)))
		  (det 0.0)
		  (invdet 0.0))
	     (loop for i from 0 below 3 do
	       (loop for j from 0 below 3 do
		 (setf (aref m i j)(aref mat i j))))
	     (setf (aref mat 0 0)(- (* (aref m 1 1)(aref m 2 2))(* (aref m 1 2)(aref m 2 1)))
		   (aref mat 0 1)(- (* (aref m 0 2)(aref m 2 1))(* (aref m 0 1)(aref m 2 2)))
		   (aref mat 0 2)(- (* (aref m 0 1)(aref m 1 2))(* (aref m 0 2)(aref m 1 1)))
		   (aref mat 1 0)(- (* (aref m 1 2)(aref m 2 0))(* (aref m 1 0)(aref m 2 2)))
		   (aref mat 1 1)(- (* (aref m 0 0)(aref m 2 2))(* (aref m 0 2)(aref m 2 0)))
		   (aref mat 1 2)(- (* (aref m 0 2)(aref m 1 0))(* (aref m 0 0)(aref m 1 2)))
		   (aref mat 2 0)(- (* (aref m 1 0)(aref m 2 1))(* (aref m 1 1)(aref m 2 0)))
		   (aref mat 2 1)(- (* (aref m 0 1)(aref m 2 0))(* (aref m 0 0)(aref m 2 1)))
		   (aref mat 2 2)(- (* (aref m 0 0)(aref m 1 1))(* (aref m 0 1)(aref m 1 0)))
		   det (+ (* (aref m 0 0)(aref mat 0 0))
			  (* (aref m 0 1)(aref mat 1 0))
			  (* (aref m 0 2)(aref mat 2 0))))
	     (if (<= (abs det) 1e-06)
		 nil
	       (progn
		 (setf invdet (/ det))
		 (loop for row from 0 below 3 do
		   (loop for col from 0 below 3 do
		     (setf (aref mat row col)(* (aref mat row col) invdet))))
		 mat))))
	 ;; invert a 2x2 matrix
	 (invert2x2 (mat)
           (let* ((m (make-array '(2 2)))
		  (det (- (* (aref mat 0 0)(aref mat 1 1))
			  (* (aref mat 1 0)(aref mat 0 1)))))
	     (if (<= (abs det) 1e-06)
		 nil
	       (progn
		 (setf (aref m 0 0)(/ (aref mat 1 1) det)
		       (aref m 1 1)(/ (aref mat 0 0) det)
		       (aref m 0 1)(- (/ (aref mat 0 1) det))
		       (aref m 1 0)(- (/ (aref mat 1 0) det)))
		 m)))))
    
    (let* (;; collect unit vectors describing the speaker positions
	   (coords (loop 
		     for s in speakers
		     for a = (if (listp s)(first s) s)
		     for e = (if (listp s)(or (second s) 0d0) 0d0)
		     for evec = (cis (* (/ e dlocsig-one-turn) 2 pi))
		     for dxy = (realpart evec)
		     for avec = (cis (* (/ a dlocsig-one-turn) 2 pi))
		     for x = (coerce (* dxy (imagpart avec)) 'long-float)
		     for y = (coerce (* dxy (realpart avec)) 'long-float)
		     for z = (coerce (imagpart evec) 'long-float)
		     for mag = (sqrt (+ (* x x)(* y y)(* z z)))
		     collect (list (/ x mag)(/ y mag)(/ z mag))))
	   ;; minimum distance
	   (min-dist (loop for distance in distances minimize distance))
	   ;; find delay times from specified distances or delays
	   (times (make-array (length speakers)
			      :initial-contents
			      (loop 
				repeat (length speakers)
				for distance = (pop distances)
				for delay = (pop delays)
				collect (or delay
					    (if distance 
						(/ (- distance min-dist) dlocsig-speed-of-sound)
					      0.0)))))
	   ;; create the group structures
	   (groups (loop
		     for id from 0
		     for group in groups 
		     for size = (length group)
		     for vertices = (loop for vertice in group 
				      collect (nth vertice coords))
		     for matrix = (cond (;; three speaker groups
					 (= size 3)
					 (invert3x3 (make-array '(3 3)
								:initial-contents
								vertices)))
					(;; two speaker groups
					 (= size 2)
					 (invert2x2 (make-array '(2 2)
								:initial-contents
								;; ignore z coordinates of vertices
								(list (butlast (first vertices))
								      (butlast (second vertices))))))
					(;; only one speaker, no matrix
					 t nil))
		     collect (make-group 
			      :id id
			      :size size
			      :speakers group
			      :vertices vertices
			      :matrix matrix))))
      ;; check validity of map entries
      (if map
	  (progn
	    (loop 
	      with entries = (length map)
	      for entry in map do
	      (if (>= entry entries)
		  (error "channel ~s in map ~s is out of range (max=~s)"
			 entry map entries)))
	    (if (/= (length (remove-duplicates map))(length map))
		(error "there are duplicate channels in map ~s" map))))
      ;; create the speaker configuration structure
      (make-speaker-config :number (length speakers)
			   :dimension (group-size (first groups))
			   :coords coords
			   :groups groups
			   :delays times
			   :map (make-array (length speakers)
					    :initial-contents
					    (loop 
					      repeat (length speakers)
					      for chan from 0
					      for whereto = (pop map)
					      collect (or whereto chan)))))))

;;; Default speaker configurations

(defparameter dlocsig-speaker-configs
  ;; by default up to eigth channels, 2-d and 3-d configurations
  (make-array '(2 9)
	      :initial-contents
	      (list
	       ;;
	       ;; 2-D speaker configurations
	       (list
		;; no channels: impossible
		nil
		;; mono
		(arrange-speakers :speakers '(0))
		;; stereo
		(arrange-speakers :speakers '(-60 60))
		;; three channels
		(arrange-speakers :speakers '(-45 45 180))
		;; four channels
		(arrange-speakers :speakers '(-45 45 135 225))
		;; five channels (5.1 arrangement)
		(arrange-speakers :speakers '(-45 0 45 135 -135))
		;; six channels
		(arrange-speakers :speakers '(-60 0 60 120 180 240))
		;; seven channels
		(arrange-speakers :speakers '(-45 0 45 100 140 -140 -100))
		;; eight speakers
		(arrange-speakers :speakers '(-22.5 22.5 67.5 112.5 157.5 202.5 247.5 292.5)))
	       ;;
	       ;; 3-D speaker configurations
	       ;;
	       (list
		;; no channels: impossible
		nil
		;; mono
		nil
		;; stereo
		nil
		;; three channels
		nil
		;; four channels 3d
		(arrange-speakers :speakers '((-60 0)(60 0)(180 0)
					      (0 90))
				  :groups '((0 1 3)(1 2 3)(2 0 3)
					    ;; floor
					    (0 1 2)))
		;; five channels 3d
		(arrange-speakers :speakers '((-45 0)(45 0)(135 0)(-135 0)
					      (0 90))
				  :groups '((0 1 4)(1 2 4)(2 3 4)(3 0 4)
					    ;; floor
					    (0 1 2)(2 3 0)))
		;; six channels 3d
		(arrange-speakers :speakers '((-45 0)(45 0)(135 0)(-135 0)
					      (-90 60)(90 60))
				  :groups '((0 1 4)(1 4 5)(1 2 5)(2 3 5)(3 4 5)(3 0 4)
					    ;; floor
					    (0 1 2)(2 3 0)))
		;; seven channels 3d
		(arrange-speakers :speakers '((-45 0)(45 0)(135 0)(-135 0)
					      (-60 60)(60 60)(180 60))
				  :groups '((0 1 4)(1 4 5)(1 2 5)(2 6 5)(2 3 6)(3 4 6)(3 0 4)(4 5 6)
					    ;; floor
					    (0 1 2)(2 3 0)))
		;; eight speakers 3d
		(arrange-speakers :speakers '((-45 -10)(45 -10)(135 -10)(225 -10)
					      (-45 45)(45 45)(135 45)(225 45))
				  :groups '((0 4 5)(0 5 1)(5 1 2)(2 6 5)(6 7 2)(2 3 7)
					    (3 7 4)(3 0 4)
					    ;; ceiling
					    (4 7 6)(6 5 4)
					    ;; floor
					    (0 1 2)(2 3 0)))))))

;;; Set a particular speaker configuration

(defun set-speaker-configuration (config 
				  &key
				  (configs dlocsig-speaker-configs))
  (setf (aref configs 
	      (if (< (speaker-config-dimension config) 3) 0 1)
	      (speaker-config-number config))
	config))

;;; Get the speaker configuration for a given number of output channels

(defun get-speaker-configuration (channels
				  &key
				  (3d dlocsig-3d)
				  (configs dlocsig-speaker-configs))
    (let* ((config  (aref configs (if 3d 1 0) channels)))
      (if (null config)
	  (error "no speaker configuration exists for ~s ~s output channel~s~%" 
		 (if 3d "tridimensional" "bidimensional")
		 channels (if (= channels 1) "s" "")))
      config))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dlocsig unit generator
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; structure that defines the unit generator

(defstruct dlocs
  ;; absolute sample number at which samples first reach the listener
  (start 0)
  ;; absolute sample number of end of input samples
  (end 0)
  ;; number of output channels in soundfile
  (out-channels 0)
  ;; number of reverb channels in soundfile
  (rev-channels 0)
  ;; delay lines for output channels that have additional delays
  out-delays
  ;; mapping of speakers to output channels
  out-map
  ;; gain envelopes, one for each output channel
  gains
  ;; reverb gain envelopes, one for each reverb channel
  rev-gains
  ;; delay tap envelope for doppler
  delay
  ;; interpolated delay line for doppler
  path
  ;; reverberation ammount
  rev)

;;; global dlocsig parameters

(defparameter dlocsig-path '())
(defparameter dlocsig-scaler 1.0)
(defparameter dlocsig-direct-power 1.5d0)
(defparameter dlocsig-inside-direct-power 1.5d0)
(defparameter dlocsig-reverb-power 0.5d0)
(defparameter dlocsig-inside-reverb-power 0.5d0)
(defparameter dlocsig-initial-delay nil)
(defparameter dlocsig-unity-gain-distance nil)
(defparameter dlocsig-reverb-amount 0.04)
(defparameter dlocsig-inside-radius 1d0)
(defparameter dlocsig-minimum-segment-length 1d0)

;; render using:

(defconstant amplitude-panning 1)
(defconstant b-format-ambisonics 2)
(defconstant decoded-ambisonics 3)
(defconstant stereo-hrtf 4)

(defparameter dlocsig-render-using amplitude-panning)

;; globals for ambisonics

(defconstant point707 (cos (/ (* pi 2) 8)))
(defparameter dlocsig-ambisonics-scaler point707)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a new dlocsig structure

(def-optkey-fun make-dlocsig ((start-time nil)
			      (duration nil)
			      (path dlocsig-path)
			      (scaler dlocsig-scaler)
			      (direct-power dlocsig-direct-power)
			      (inside-direct-power dlocsig-inside-direct-power)
			      (reverb-power dlocsig-reverb-power)
			      (inside-reverb-power dlocsig-inside-reverb-power)
			      (reverb-amount dlocsig-reverb-amount)
			      (initial-delay dlocsig-initial-delay)
			      (unity-gain-dist dlocsig-unity-gain-distance)
			      (inside-radius dlocsig-inside-radius)
			      (minimum-segment-length dlocsig-minimum-segment-length)
			      (render-using dlocsig-render-using)
			      (out-channels nil)
			      (rev-channels nil))
  (if (null start-time)
      (error "a start time is required in make-dlocsig"))
  (if (null duration)
      (error "a duration has to be specified in make-dlocsig"))
  ;; check to see if we have the rigth number of channels for b-format ambisonics
  (if (and (= render-using b-format-ambisonics)
	   (/= (or out-channels (mus-channels *output*)) 4))
      (error "ambisonics b-format requires four output channels, current number is ~s"
	     (or out-channels (mus-channels *output*))))

  (let* (;; number of output channels
	 (out-channels (or out-channels (mus-channels *output*)))
	 (rev-channels (or rev-channels (if *reverb* (mus-channels *reverb*) 0)))
	 ;; speaker configuration for current number of channels
	 (speakers (get-speaker-configuration out-channels))
	 ;; array of gains
	 (channel-gains (make-array out-channels))
	 (channel-rev-gains (make-array rev-channels))
	 ;; speaker output delays
	 (max-out-delay 0.0)
	 (out-delays (make-array out-channels))
	 ;; coordinates of rendered path
	 (xpoints (path-x path))
	 (ypoints (path-y path))
	 (zpoints (path-z path))
	 (tpoints (path-time path))
	 ;; speed of sound expressed in terms of path time coordinates
	 (speed-limit (/ (* dlocsig-speed-of-sound 
			    (- (first (last tpoints))(first tpoints)))
			 duration))
	 (start 0)
	 (delay '())
	 (real-dur 0)
	 (prev-time nil)
	 (prev-dist nil)
	 (prev-group nil)
	 (prev-x nil)
	 (prev-y nil)
	 (prev-z nil)
	 (first-dist nil)
	 (last-dist nil)
	 (min-dist nil)
	 (max-dist nil)
	 (min-delay nil)
	 (min-dist-unity nil)
	 (unity-gain 1.0)
	 (unity-rev-gain 1.0)
	 (run-beg)
	 (run-end))

    (labels (;; convert distance to samples
	     (dist->samples (d) (* d (/ *srate* dlocsig-speed-of-sound)))
	     ;;
	     ;; convert distance to seconds of delay
	     (dist->seconds (d) (/ d dlocsig-speed-of-sound))
	     ;;
	     ;; convert time to samples
	     (time->samples (time) (* time *srate*))
	     ;;
	     ;; calculate transition point between two adyacent three-speaker groups
	     ;;
	     (transition-point-3 (vert-a vert-b xa ya za xb yb zb) 
	       (declare (optimize (speed 3)(safety 1)))
	       (macrolet ((cross (v1 v2)
			  `(list (- (* (second ,v1)(third ,v2))
				    (* (third ,v1)(second ,v2)))
				 (- (* (third ,v1)(first ,v2))
				    (* (first ,v1)(third ,v2)))
				 (- (* (first ,v1)(second ,v2))
				    (* (second ,v1)(first ,v2)))))
			  (dot (v1 v2)
			  `(+ (* (first ,v1)(first ,v2))
			      (* (second ,v1)(second ,v2))
			      (* (third ,v1)(third ,v2))))
			  (sub (v1 v2)
			  `(list (- (first ,v1)(first ,v2))
				 (- (second ,v1)(second ,v2))
				 (- (third ,v1)(third ,v2))))
			  (add (v1 v2)
			  `(list (+ (first ,v1)(first ,v2))
				 (+ (second ,v1)(second ,v2))
				 (+ (third ,v1)(third ,v2))))
			  (scale (v1 c)
			  `(list (* (first ,v1) ,c)
				 (* (second ,v1) ,c)
				 (* (third ,v1) ,c))))
		 (let* ((tolerance 1d-6)
			(line-b (list xa ya za))
			(line-m (sub (list xb yb zb) line-b))
			(normal (cross vert-a vert-b))
			(denominator (dot normal line-m)))
		   (if (<= (abs denominator) tolerance)
		       ;; line and plane defined by edge and origin are parallel
		       (return-from transition-point-3 nil)
		     ;; return intersection
		     (add line-b (scale line-m (/ (- (dot normal line-b)) denominator)))))))
	     ;;
	     ;; calculate transition point between two adyacent two-speaker groups
	     ;; original line intersection code from Graphic Gems III
	     ;;
	     (transition-point-2 (vert xa ya xb yb)
	       (let* ((Ax (first vert))
		      (Bx (- xa xb))
		      (Ay (second vert))
		      (By (- ya yb))
		      (Cx (- xa))
		      (Cy (- ya))
		      (d (- (* By Cx)(* Bx Cy)))
		      (f (- (* Ay Bx)(* Ax By))))
		 (if (= f 0)
		     nil
		   (list (/ (* d Ax) f)
			 (/ (* d Ay) f)))))
	     ;;
	     ;; calculate speaker gains for group
	     ;;
	     (calculate-gains (x y z group)
	       (let* ((zero-coord 1d-10)
		      (zero-gain 1d-10)
		      (size (group-size group))
		      (mat (group-matrix group)))
		 (cond (;; gains at (0 0 0) would be very VERY high...
			;; enough to make the listener's head explode.
			;; we match any group and return all ones
			(and (< (abs x) zero-coord)
			     (< (abs y) zero-coord)
			     (< (abs z) zero-coord))
			(values t (list 1d0 1d0 1d0)))
		       (;; three speaker groups
		       (= size 3)
		       (let* ((gain-a (+ (* (aref mat 0 0) x)
					 (* (aref mat 1 0) y)
					 (* (aref mat 2 0) z)))
			      (gain-b (+ (* (aref mat 0 1) x)
					 (* (aref mat 1 1) y)
					 (* (aref mat 2 1) z)))
			      (gain-c (+ (* (aref mat 0 2) x)
					 (* (aref mat 1 2) y)
					 (* (aref mat 2 2) z)))
			      (mag (sqrt (+ (* gain-a gain-a)
					    (* gain-b gain-b)
					    (* gain-c gain-c)))))
			 ;; truncate to zero roundoff errors
			 (if (< (abs gain-a) zero-gain)
			     (setf gain-a 0d0))
			 (if (< (abs gain-b) zero-gain)
			     (setf gain-b 0d0))
			 (if (< (abs gain-c) zero-gain)
			     (setf gain-c 0d0))
			 (values (and (>= gain-a 0)(>= gain-b 0)(>= gain-c 0))
				 (list (/ gain-a mag)(/ gain-b mag)(/ gain-c mag)))))
		       (;; two speaker groups
			(= size 2)
			(let* ((gain-a (+ (* (aref mat 0 0) x)
					  (* (aref mat 1 0) y)))
			       (gain-b (+ (* (aref mat 0 1) x)
					  (* (aref mat 1 1) y)))
			       (mag (sqrt (+ (* gain-a gain-a)
					     (* gain-b gain-b)))))
			  ;; truncate to zero roundoff errors
			  (if (< (abs gain-a) zero-gain)
			      (setf gain-a 0d0))
			  (if (< (abs gain-b) zero-gain)
			      (setf gain-b 0d0))
			  (values (and (>= gain-a 0)(>= gain-b 0))
				  (list (/ gain-a mag)(/ gain-b mag)))))
		       (;; one speaker group, must be the mono configuration
			;; it should be the only group so it always matches
			;; and there's no scaling of amplitude
			(= size 1)
			(values t (list 1d0))))))
	     ;;
	     ;; find the speaker group that contains a point
	     ;;
	     (find-group (x y z)
	       (loop 
		 for group in (speaker-config-groups speakers) do
		 (multiple-value-bind (inside gains)
		     (calculate-gains x y z group)
		   (if inside 
		       (return-from find-group
			 (values group gains)))))
	       (values nil nil))
	     ;;
	     ;; push zero gains on all channels
	     ;;
	     (push-zero-gains (time)
	       ;; push channel gains
	       (loop for i from 0 below (speaker-config-number speakers) do 
		 (push time (aref channel-gains i))
		 (push 0.0 (aref channel-gains i)))
	       ;; push reverb gain
	       (loop for i from 0 below rev-channels do
		 (push time (aref channel-rev-gains i))
		 (push 0.0 (aref channel-rev-gains i))))
	     ;;
	     ;; push gain and time into envelopes
	     ;;
	     (push-gains (group gains dist time)
	       (let* ((outputs (make-array out-channels :initial-element 0.0))
		      (rev-outputs (make-array rev-channels :initial-element 0.0))
		      ;; attenuation with distance of direct signal
		      (att (if (>= dist inside-radius)
			       (/ (expt dist direct-power))
			     (- 1d0 (expt (/ dist inside-radius)(/ inside-direct-power)))))
		      ;; attenuation with distance of reverberated signal
		      (ratt (if (>= dist inside-radius)
				(/ (expt dist reverb-power))
			      (- 1d0 (expt (/ dist inside-radius)(/ inside-reverb-power))))))
		 (if (>= dist inside-radius)
		     ;; outside the inner sphere, signal is sent to group
		     (loop 
		       for speaker in (group-speakers group)
		       for gain in gains do
		       (setf (aref outputs speaker)(* gain att))
		       (if (> rev-channels 1) (setf (aref rev-outputs speaker)(* gain ratt))))
		   ;; inside the inner sphere, signal is sent to all speakers
		   (loop 
		     with gain
		     for speaker from 0 below (speaker-config-number speakers)
		     for found = (position speaker (group-speakers group)) do
		     (if found
			 ;; speaker belongs to group, add to existing gain
			 (progn
			   (setf gain (nth found gains)
				 (aref outputs speaker)(+ gain (* (- 1d0 gain) att)))
			   (if (> rev-channels 1) (setf (aref rev-outputs speaker)(+ gain (* (- 1d0 gain) ratt)))))
		       ;; speaker outside of group
		       (progn
			 (setf (aref outputs speaker) att)
			 (if (> rev-channels 1) (setf (aref rev-outputs speaker) ratt))))))
		 ;; push all channel gains into envelopes
		 (loop for i from 0 below (speaker-config-number speakers) do 
		   (push time (aref channel-gains i))
		   (push (aref outputs i)(aref channel-gains i))
		   (if (> rev-channels 1)
		       (progn
			 (push time (aref channel-rev-gains i))
			 (push (aref rev-outputs i)(aref channel-rev-gains i)))))
		 ;; push reverb gain into envelope for mono reverb
		 (if (= rev-channels 1)
		     (progn
		       (push time (aref channel-rev-gains 0))
		       (push ratt (aref channel-rev-gains 0))))))
	     ;;
	     ;; Render a trajectory breakpoint through amplitude panning
	     ;;
	     (amplitude-panning (x y z dist time q)
				(declare (ignore q))
	       ;; output gains for current point
	       (if prev-group
		   (multiple-value-bind (inside gains)
		       (calculate-gains x y z prev-group)
		     ;; check that the source is not moving faster than sound
		     (if (/= time prev-time)
			 (let* ((speed (/ (- dist prev-dist)(- time prev-time))))
			   (if (> speed speed-limit)
			       (warn "supersonic radial movement at [~f,~f,~f], speed=~f~%"
				     x y z time speed))))
		     (if inside
			 ;; still in the same group
			 (progn
			   (push-gains prev-group gains dist time)
			   (setf prev-x x
				 prev-y y
				 prev-z z))
		       ;; left the group
		       (multiple-value-bind (group gains)
			   (find-group x y z)
			 (if group
			     ;; we have to interpolate a new point that lies on the shared
			     ;; edge of the adyacent groups so that the speakers opposite
			     ;; the edge have zero gain when the trajectory switches groups
			     (let* ((edge (intersection (group-vertices group)
							(group-vertices prev-group)
							:test #'equalp)))
			       (cond (;; the groups have two shared points (ie: share an edge)
				      ;; this must be a three speaker groups transition
				      (= (length edge) 2)
				      (let* ((pint (transition-point-3 (first edge)(second edge)
								       x y z prev-x prev-y prev-z)))
					(if pint
					    (let* ((xi (first pint))
						   (yi (second pint))
						   (zi (third pint))
						   (di (distance xi yi zi))
						   (ti (+ prev-time (* (/ (distance (- xi prev-x)
										    (- yi prev-y)
										    (- zi prev-z))
									  (distance (- x prev-x)
										    (- y prev-y)
										    (- z prev-z)))
								       (- time prev-time)))))
					      ;; see if we are inside the previous group
					      ;; we can be on either side due to roundoff errors
					      (multiple-value-bind (inside gains)
						  (calculate-gains xi yi zi prev-group)
						(if inside 
						    (push-gains prev-group gains di ti)
						  (multiple-value-bind (inside gains)
						      (calculate-gains xi yi zi group)
						    (if inside
							(push-gains group gains di ti)
						      ;; how did we get here?
						      (error "Outside of both adyacent groups [~s:~s:~s @~s]~%"
							     xi yi zi ti)))))))))
				     (;; two two-speaker groups share one point
				      ;; z coordinates are silently ignored
				      (and (= (length edge) 1)(= (group-size group) 2))
				      (let* ((pint (transition-point-2 (first edge) x y prev-x prev-y)))
					(if pint
					    (let* ((xi (first pint))
						   (yi (second pint))
						   (di (distance xi yi 0.0))
						   (ti (+ prev-time (* (/ (distance (- xi prev-x)
										    (- yi prev-y)
										    0.0)
									  (distance (- x prev-x)
										    (- y prev-y)
										    0.0))
						   (- time prev-time)))))
					      ;; see if we are inside the previous group
					      ;; we can be on either side due to roundoff errors
					      (multiple-value-bind (inside gains)
						  (calculate-gains xi yi 0.0 prev-group)
						(if inside 
						    (push-gains prev-group gains di ti)
						  (multiple-value-bind (inside gains)
						      (calculate-gains xi yi 0.0 group)
						    (if inside
							(push-gains group gains di ti)
						      ;; how did we get here?
						      (error "Outside of both adyacent groups [~s:~s @~s]~%"
							     xi yi ti)))))))))
				     (;; groups share only one point... for now a warning
				      ;; we should calculate two additional interpolated
				      ;; points as the trajectory must be crossing a third
				      ;; group
				      (= (length edge) 1)
				      (loop 
					for int-group in (speaker-config-groups speakers) do
					(if (and (member (first edge)(group-vertices int-group))
						 (not (equalp int-group group))
						 (not (equalp int-group prev-group)))
					    (let* ((edge1 (intersection (group-vertices int-group)
									(group-vertices prev-group)
									:test #'equalp))
						   (edge2 (intersection (group-vertices int-group)
									(group-vertices group)
									:test #'equalp)))
					      (format t "e1=~s; e2=~s~%" edge1 edge2))))

				      (warn "crossing between groups with only one point in common~%  prev=~s~%  curr=~s~%"
					    prev-group group))
				     (;; groups don't share points... how did we get here?
				      (= (length edge) 0)
				      (warn "crossing between groups with no common points, ~s~s to ~s~s~%"
					    (group-id prev-group)(group-speakers prev-group)
					    (group-id group)(group-speakers group))))
			       ;; finally push gains for current group
			       (push-gains group gains dist time)
			       (setf prev-group group
				     prev-x x
				     prev-y y
				     prev-z z))
			   ;; current point is outside all defined groups
			   ;; we should send a warning at this point...
			   (progn
			     (push-zero-gains time)
			     (setf prev-group nil))))))
		 ;; first time around
		 (multiple-value-bind (group gains)
		     (find-group x y z)
		   (if group
		       (progn
			 (push-gains group gains dist time)
			 (setf prev-group group
			       prev-x x
			       prev-y y
			       prev-z z))
		     (progn
		       (push-zero-gains time)
		       (setf prev-group nil)))))
	       ;; remember current parameters for next point
	       (setf prev-time time
		     prev-dist dist))
	     ;;
	     ;; Render a trajectory breakpoint for ambisonics b-format coding
	     ;; http://www.york.ac.uk/inst/mustech/3d_audio/ambis2.htm
	     ;;
	     ;; Ambisonics b-format has four discrete channels encoded as follows:
	     ;; W: (* signal 0.707) (omnidirectional component)
	     ;; X: (* signal (cos A)(cos B))
	     ;; Y: (* signal (sin A)(cos B))
	     ;; Z: (* signal (sin B))
	     ;; where:
	     ;; A: counter-clockwise angle of rotation from the front center
	     ;; B: the angle of elevation above the horizontal plane
	     ;; 
	     ;; in our coordinate system:
	     ;; xy: (* dist (cos B))
	     ;; (cos A): (/ y xy)
	     ;; (sin A): (/ -x xy)
	     ;; (cos B): (/ xy dist)
	     ;; (sin B): (/ z dist)
	     ;; so:
	     ;; W: (* signal 0.707)
	     ;; X: (* signal (/ y dist))
	     ;; Y: (* signal (/ -x dist))
	     ;; Z: (* signal (/ z dist))
	     ;;
	     (b-format-ambisonics (x y z dist time)
	       (let* ((att (if (> dist inside-radius)
			       (expt (/ inside-radius dist) direct-power)
			     (expt (/ dist inside-radius)(/ inside-direct-power))))
		      (attW (if (> dist inside-radius)
				(* point707 att)
			      (- 1 (* (- 1 point707) (expt (/ dist inside-radius) direct-power)))))
		      (ratt (if (> dist inside-radius)
				(expt (/ inside-radius dist) reverb-power)
			      (expt (/ dist inside-radius)(/ inside-reverb-power))))
		      (rattW (if (> dist inside-radius)
				 (* point707 ratt)
			       (- 1 (* (- 1 point707) (expt (/ dist inside-radius) reverb-power))))))
		 ;; output encoding gains for point
		 ;; W: 0.707
		 (push time (aref channel-gains 0))
		 (push attW (aref channel-gains 0))
		 ;; X: (* (cos A)(cos B))
		 (push time (aref channel-gains 1))
		 (push (* (if (zerop dist) 0 (/ y dist)) att)(aref channel-gains 1))
		 ;; Y: (* (sin A)(cos B))
		 (push time (aref channel-gains 2))
		 (push (* (if (zerop dist) 0 (/ (- x) dist)) att)(aref channel-gains 2))
		 ;; Z: (sin B)
		 (push time (aref channel-gains 3))
		 (push (* (if (zerop dist) 0 (/ z dist)) att)(aref channel-gains 3))
		 ;; push reverb gain into envelope
		 (if (= rev-channels 1)
		     (progn
		       ;; mono reverb output
		       (push time (aref channel-rev-gains 0))
		       (push (if (>= dist inside-radius)
				 (/ (expt dist reverb-power))
			       (- 1d0 (expt (/ dist inside-radius)(/ inside-reverb-power))))
			     (aref channel-rev-gains 0)))
		   (progn
		     ;; multichannel reverb, send ambisonics components
		     ;; W: 0.707
		     (push time (aref channel-rev-gains 0))
		     (push rattW (aref channel-rev-gains 0))
		     ;; X: (* (cos A)(cos B))
		     (push time (aref channel-rev-gains 1))
		     (push (* (if (zerop dist) 0 (/ y dist)) ratt)(aref channel-rev-gains 1))
		     ;; Y: (* (sin A)(cos B))
		     (push time (aref channel-rev-gains 2))
		     (push (* (if (zerop dist) 0 (/ (- x) dist)) ratt)(aref channel-rev-gains 2))
		     ;; Z: (sin B)
		     (push time (aref channel-rev-gains 3))
		     (push (* (if (zerop dist) 0 (/ z dist)) ratt)(aref channel-rev-gains 3))))))
	     ;;
	     ;; Render a trajectory breakpoint to a room for decoded ambisonics
	     ;;
	     ;; for a given speaker located in 3d space in polar coordinates:
	     ;; az: azimut angle, increments clockwise
	     ;; el: elevation angle
	     ;;
	     ;; S: (+ W (* X (cos az)(cos el))
	     ;;         (* Y (sin az)(cos el))
	     ;;         (* Z (sin el)))
	     ;; 
	     (decoded-ambisonics (x y z dist time)
	       (let* ((att (if (> dist inside-radius)
			       (expt (/ inside-radius dist) direct-power)
			     (expt (/ dist inside-radius)(/ inside-direct-power))))
		      (attW (if (> dist inside-radius)
				(* point707 att)
			      (- 1 (* (- 1 point707)(expt (/ dist inside-radius) direct-power)))))
		      (ratt (if (> dist inside-radius)
			       (expt (/ inside-radius dist) reverb-power)
			     (expt (/ dist inside-radius)(/ inside-reverb-power))))
		      (rattW (if (> dist inside-radius)
				(* point707 ratt)
			      (- 1 (* (- 1 point707)(expt (/ dist inside-radius) reverb-power))))))
		 ;; output decoded gains for point
		 (loop 
		   for i from 0 below (speaker-config-number speakers) 
		   for s in (speaker-config-coords speakers)
		   for signal = (* dlocsig-ambisonics-scaler
				   (+ 
				    ;; W
				    (* attW point707)
				    ;; (* X (cos az)(cos el))
				    (* att (if (zerop dist) 0 (/ y dist))(second s))
				    ;; (* Y (sin az)(cos el))
				    (* att (if (zerop dist) 0 (/ x dist))(first s))
				    ;; (* Z (sin el)
				    (* att (if (zerop dist) 0 (/ z dist))(third s))))
		   do 
		   (push time (aref channel-gains i))
		   (push signal (aref channel-gains i)))
		 ;; push reverb gain into envelope
		 (if (= rev-channels 1)
		     (progn
		       ;; mono reverberation
		       (push time (aref channel-rev-gains 0))
		       (push (if (>= dist inside-radius)
				 (/ (expt dist reverb-power))
			       (- 1d0 (expt (/ dist inside-radius)(/ inside-reverb-power))))
			     (aref channel-rev-gains 0)))
		   ;; multichannel reverb
		   (loop 
		     for i from 0 below rev-channels
		     for s in (speaker-config-coords speakers)
		     for signal = (* dlocsig-ambisonics-scaler
				     (+ 
				      ;; W
				      (* rattW point707)
				      ;; (* X (cos az)(cos el))
				      (* ratt (if (zerop dist) 0 (/ y dist))(second s))
				      ;; (* Y (sin az)(cos el))
				      (* ratt (if (zerop dist) 0 (/ x dist))(first s))
				      ;; (* Z (sin el)
				      (* ratt (if (zerop dist) 0 (/ z dist))(third s))))
		     do 
		     (push time (aref channel-rev-gains i))
		     (push signal (aref channel-rev-gains i))))))
	     ;;
	     ;; Loop through all virtual rooms for one breakpoint in the trajectory
	     ;;
	     (walk-all-rooms (x y z time)
	       (let ((room 0)
		     (dist (distance x y z)))
		 ;; remember first and last distances
		 (if (null first-dist)
		     (setf first-dist dist))
		 (setf last-dist dist)
		 ;; remember maximum and minimum distances
		 (if (or (null min-dist)(< dist min-dist))
		     (setf min-dist dist))
		 (if (or (null max-dist)(> dist max-dist))
		     (setf max-dist dist))
		 ;; push delay for current point (for doppler)
		 (push time delay)
		 (push (dist->samples dist) delay)
		 ;; do the rendering of the point
		 (cond
		  (;; amplitude panning
		   (= render-using amplitude-panning)
		   (amplitude-panning x y z dist time 1))
		  (;; ambisonics b format
		   (= render-using b-format-ambisonics)
		   (b-format-ambisonics x y z dist time))
		  (;; ambisonics decoded
		   (= render-using decoded-ambisonics)
		   (decoded-ambisonics x y z dist time)))
		 (incf room)
		 ;; return number of rooms processed
		 room))
	     ;;
	     ;; Check to see if a segment changes radial direction:
	     ;;
	     ;;   a change in radial direction implies a change in 
	     ;;   doppler shift that has to be reflected as a new
	     ;;   point in the rendered envelopes
	     ;;
	     (change-direction (xa ya za ta xb yb zb tb)
	       (walk-all-rooms xa ya za ta)
	       (if (or (/= xa xb)(/= ya yb)(/= za zb)(/= ta tb))
		   (multiple-value-bind (xi yi zi)
		       (nearest-point xa ya za xb yb zb 0 0 0)
		     (if (and (if (< xa xb)(<= xa xi xb)(<= xb xi xa))
			      (if (< ya yb)(<= ya yi yb)(<= yb yi ya))
			      (if (< za zb)(<= za zi zb)(<= zb zi za)))
			 (walk-all-rooms xi yi zi
					 (+ tb (* (- ta tb)
						  (/ (distance (- xb xi)(- yb yi)(- zb zi))
						     (distance (- xb xa)(- yb ya)(- zb za))))))))))
	     ;;
	     ;; Check to see if a segment intersects the inner sphere:
	     ;;
	     ;;   points inside are rendered differently so we need to
	     ;;   create additional envelope points in the boundaries
	     ;;
	     (intersects-inside-radius (xa ya za ta xb yb zb tb)
	       (let* ((mag (distance (- xb xa)(- yb ya)(- zb za)))
		      (vx (/ (- xb xa) mag))
		      (vy (/ (- yb ya) mag))
		      (vz (/ (- zb za) mag))
		      (bsq (+ (* xa vx)(* ya vy)(* za vz)))
		      (u (- (+ (* xa xa)(* ya ya)(* za za))
			    (* inside-radius inside-radius)))
		      (disc (- (* bsq bsq) u))
		      (hit (>= disc 0.0)))
		 (if hit
		     ;; ray defined by two points hits sphere
		     (let* ((root (sqrt disc))
			    (rin  (- (- bsq) root))
			    (rout (+ (- bsq) root))
			    xi yi zi ti xo yo zo to)
		       (if (and (> rin 0)(< rin mag))
			   ;; intersects entering sphere
			   (setf xi (+ xa (* vx rin))
				 yi (+ ya (* vy rin))
				 zi (+ za (* vz rin))
				 ti (+ tb (* (- ta tb)
					     (/ (distance (- xb xi)(- yb yi)(- zb zi))
						(distance (- xb xa)(- yb ya)(- zb za)))))))
		       (if (and (> rout 0)(< (abs rout) mag))
			   ;; intersects leaving sphere
			   (setf xo (+ xa (* vx rout))
				 yo (+ ya (* vy rout))
				 zo (+ za (* vz rout))
				 to (+ tb (* (- ta tb)
					     (/ (distance (- xb xo)(- yb yo)(- zb zo))
						(distance (- xb xa)(- yb ya)(- zb za)))))))
		       (if xi
			   (progn
			     (change-direction xa ya za ta xi yi zi ti)
			     (if xo
				 (progn
				   (change-direction xi yi zi ti xo yo zo to)
				   (change-direction xo yo zo to xb yb zb tb))
			       (change-direction xi yi zi ti xb yb zb tb)))
			 (if xo
			     (progn
			       (change-direction xa ya za ta xo yo zo to)
			       (change-direction xo yo zo to xb yb zb tb))
			   (change-direction xa ya za ta xb yb zb tb))))
		   (change-direction xa ya za ta xb yb zb tb))))
	     ;;
	     ;; Recursively split segment if longer than minimum rendering distance:
	     ;;
	     ;;   otherwise long line segments that have changes in distance render 
	     ;;   the amplitude envelope as a linear function that does not reflect
	     ;;   the chosen power function (1/d^n)
	     ;;
	     (minimum-segment-length (xa ya za ta xb yb zb tb)
	       (let* ((dist (distance (- xb xa)(- yb ya)(- zb za))))
		 (if (< dist minimum-segment-length)
		     (intersects-inside-radius xa ya za ta xb yb zb tb)
		   ;; interpolate a new point half way thorugh the segment
		   (let* ((xi (/ (+ xa xb) 2))
			  (yi (/ (+ ya yb) 2))
			  (zi (/ (+ za zb) 2))
			  (ti (+ tb (* (- ta tb)
				       (/ (distance (- xb xi)(- yb yi)(- zb zi))
					  (distance (- xb xa)(- yb ya)(- zb za)))))))
		     (minimum-segment-length xa ya za ta xi yi zi ti)
		     (minimum-segment-length xi yi zi ti xb yb zb tb))))))
      ;;
      ;; Loop for each pair of points in the position envelope and render them
      ;;
      (if (= (length xpoints) 1)
	  ;; static source (we should check if this is inside the inner radius?)
	  (walk-all-rooms (first xpoints)(first ypoints)(first zpoints)(first tpoints))
	;; moving source
	(loop 
	  for xa in xpoints
	  for ya in ypoints
	  for za in zpoints
	  for ta in tpoints 
	  for xb in (cdr xpoints)
	  for yb in (cdr ypoints)
	  for zb in (cdr zpoints)
	  for tb in (cdr tpoints) 
	  do (minimum-segment-length xa ya za ta xb yb zb tb)
	  finally (walk-all-rooms xb yb zb tb)))
      ;; create delay lines for output channels that need them
      #-clisp (loop 
	for channel from 0
	for delay across (speaker-config-delays speakers) do
	(setf (aref out-delays channel)(if (/= delay 0.0)(make-delay (time->samples delay)) nil)
	      max-out-delay (max max-out-delay delay)))
      #+clisp
      	(let ((delays (speaker-config-delays speakers)))
	  (loop for channel from 0 below (length delays) do
	    (let ((delay (aref delays channel)))
	      (setf (aref out-delays channel)(if (/= delay 0.0)(make-delay (time->samples delay)) nil)
		    max-out-delay (max max-out-delay delay)))))
      (setf ;; delay from the minimum distance to the listener
            min-delay (dist->samples min-dist)
            ;; sample at which signal first arrives to the listener
	    start (dist->samples (- first-dist (if initial-delay 0.0 min-dist)))
	    ;; duration of sound at listener's position after doppler src
	    real-dur (+ duration (dist->seconds (- last-dist first-dist)))
	    ;; start and end of the run loop in samples
	    run-beg (floor (time->samples start-time))
	    run-end (floor (- (+ (time->samples (+ start-time duration))
				 (dist->samples last-dist)
				 (time->samples max-out-delay))
			      (if initial-delay 0.0 min-delay)))
	    ;; minimum distance for unity gain calculation
	    min-dist-unity (if (< min-dist inside-radius)
			       inside-radius 
			     min-dist)
	    ;; unity-gain gain scalers
	    unity-gain (* scaler 
			  (if (numberp unity-gain-dist)
			      (expt unity-gain-dist direct-power)
			    (if (null unity-gain-dist)
				(expt min-dist-unity direct-power)
			      1.0)))
	    unity-rev-gain (* scaler
			      (if (numberp unity-gain-dist)
				  (expt unity-gain-dist reverb-power)
				(if (null unity-gain-dist)
				    (expt min-dist-unity reverb-power)
				  1.0))))
      (values 
       ;; return runtime structure with all the information
       (make-dlocs :start start
		   :end (floor (* (+ start-time duration) *srate*))
		   :out-channels (speaker-config-number speakers)
		   :rev-channels rev-channels
		   :out-map (speaker-config-map speakers)
		   :out-delays out-delays
		   :gains (make-array (speaker-config-number speakers)
				      :initial-contents
				      (loop
					for i from 0 below out-channels
					collect (make-env :envelope (nreverse (aref channel-gains i))
							  :scaler (if (= render-using b-format-ambisonics)
								      1.0 unity-gain)
							  :duration real-dur)))
		   :rev-gains (if (/= rev-channels 0)
				  (make-array rev-channels
					      :initial-contents
					      (loop
						for i from 0 below rev-channels
						collect (make-env :envelope (nreverse (aref channel-rev-gains i))
								  :scaler (if (= render-using b-format-ambisonics)
									      1.0 unity-rev-gain)
								  :duration real-dur)))
				nil)
		   :delay (make-env :envelope (nreverse delay)
				    :offset (if initial-delay 0.0 (- min-delay))
				    :duration real-dur)
		   :path (make-delay 0 
		           :max-size (max 1 (+ (ceiling (dist->samples max-dist)) 1)))
		   :rev (make-env :envelope (if (numberp reverb-amount)
						(list 0 reverb-amount 1 reverb-amount)
					      reverb-amount)
				  :duration real-dur))
       ;; return start and end samples for the run loop
       run-beg
       run-end))))

#|

(with-sound(:channels 6 :play nil :statistics t)(sinewave 0 10 440 0.5 :path (make-path '((-10 10)(0.5 0.5)(10 10)) :error 0.001)))

(with-sound(:statistics t :channels 4 :reverb-channels 4 :reverb freeverb :decay-time 3)
  (move 0 "/usr/ccrma/snd/nando/sounds/kitchen/bowl/small-medium-large-1.snd"
	:paths (list (make-spiral-path :start-angle 0 :turns 2.5)
		     (make-spiral-path :start-angle 180 :turns 3.5))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run macro to localize samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dlocsig (i in-sig start end out-channels rev-channels out-delays out-map gains rev-gains delay path rev)
  ;; generate names for internal variables so there is no name space collision
  (let ((input (gensym))
	(sample (gensym))
	(amount (gensym)))
    ;; And now, let's locate the signal
    `(let ((,input ,in-sig))
       (if (< ,i ,start)
	   ;; feed the delay while waiting for the signal to appear at the output
	   (progn
	     (delay ,path (if (> ,i ,end) 0.0 ,input) 0.0)
	     ;; in the meantime send 0 to the outputs
	     (loop for chan from 0 below ,out-channels do
	       (out-any ,i 0.0 chan)))
	 ;; signal coming out of the delay line... process it
	 (let* ((,sample (delay ,path (if (> ,i ,end) 0.0 ,input) 
				 (env ,delay))))
	   ;; set gain for each channel and send to output
	   (loop for chan from 0 below ,out-channels do
	     (out-any ,i (if (aref ,out-delays chan)
			     (delay (aref ,out-delays chan)
				     (* ,sample (env (aref ,gains chan))))
			   (* ,sample (env (aref ,gains chan))))
		      (aref ,out-map chan)))
	   (if *reverb*
	       ;; mono reverb
	       (if (= ,rev-channels 1)
		   (outa ,i (* ,sample (env ,rev)(env (aref ,rev-gains 0)))
			 *reverb*)
		 ;; multichannel reverb
		 (loop for chan from 0 below ,rev-channels
		   for ,amount = (env ,rev) do
		   (out-any ,i (* ,sample ,amount (env (aref ,rev-gains chan)))
			    (aref ,out-map chan) *reverb*)))))))))

;;;;;;;;;
;;; Paths
;;;;;;;;;

;;; Generic path class

(defclass path ()
  (;; rendered coordinates
   (rx :initform '())
   (ry :initform '())
   (rz :initform '())
   (rv :initform '())
   (rt :initform '())
   ;; transformed coordinates
   (tx :initform '())
   (ty :initform '())
   (tz :initform '())
   (tt :initform '())))

;;; Inquiries into the state of the path

(defmethod not-rendered ((path path))
  (null (slot-value path 'rx)))

(defmethod not-transformed ((path path))
  (null (slot-value path 'tx)))

;;; Reset any transformations on the originally rendered path

(defmethod reset-transformation ((path path))
  (setf (slot-value path 'tt) nil
	(slot-value path 'tx) nil
	(slot-value path 'ty) nil
	(slot-value path 'tz) nil)
  path)

;;; Reset the rendered path (and any transformations)

(defmethod reset-rendering ((path path))
  (setf (slot-value path 'rt) nil
	(slot-value path 'rv) nil
	(slot-value path 'rx) nil
	(slot-value path 'ry) nil
	(slot-value path 'rz) nil)
  (reset-transformation path))

;;; Return the best possible set of coordinates

(defmethod path-x ((path path))
  (if (slot-value path 'tx)
      (slot-value path 'tx)
    (slot-value (if (slot-value path 'rx)
		    path
		  (render-path path)) 'rx)))

(defmethod path-y ((path path))
  (if (slot-value path 'ty)
      (slot-value path 'ty)
    (slot-value (if (slot-value path 'ry)
		    path
		  (render-path path)) 'ry)))

(defmethod path-z ((path path))
  (if (slot-value path 'tz)
      (slot-value path 'tz)
    (slot-value (if (slot-value path 'rz)
		    path
		  (render-path path)) 'rz)))

(defmethod path-time ((path path))
  (if (slot-value path 'tt)
      (slot-value path 'tt)
    (slot-value (if (slot-value path 'rt)
		    path
		  (render-path path)) 'rt)))

;;;;;;;;;;;;;;;;
;;; Bezier paths
;;;;;;;;;;;;;;;;

;;; Parse a path as two or three-dimensional paths

(defparameter path-3d t)

;;; Path class for bezier rendered paths

(defclass bezier-path (path) 
  (;; path
   (path :initform '() :initarg :path)
   ;; it is parsed as a 3d or 2d path?
   (3d :initform t :initarg :3d)
   ;; by default a path is cartesian
   (polar :initform nil :initarg :polar)
   ;; parsed coordinates and velocity of original points
   (x :initform '())
   (y :initform '())
   (z :initform '())
   (v :initform '())
   ;; control points for bezier curve fitting 
   (bx :initform '())
   (by :initform '())
   (bz :initform '())
   (error :initform 0.01 :initarg :error)
   (curvature :initform nil :initarg :curvature)))

;;; Path class for open bezier paths

(defclass open-bezier-path (bezier-path)
  (;; bezier curve fitting control parameters
   (initial-direction :initform '(0.0 0.0 0.0) :initarg :initial-direction)
   (final-direction :initform '(0.0 0.0 0.0) :initarg :final-direction)))

;;; Path class for closed bezier paths

(defclass closed-bezier-path (bezier-path)())

;;; Generic error when method passed illegal path

(defun illegal-path-argument (path)
  (error "~s is not a path or a list describing a path" path))

;;;
;;; Generic defining function (for open, closed, polar and cartesian paths)
;;;

(def-optkey-fun make-path (path
			   (3d path-3d)
			   (polar nil)
			   (closed nil)
			   (curvature nil)
			   (error 0.01)
			   ;; only for open paths
			   initial-direction
			   final-direction)
  ;; some sanity checks
  (if (null path)
      (error "Can't define a path with no points in it"))
  (if (and closed initial-direction)
      (error "Can't specify initial direction ~s for a closed path ~s"
	     initial-direction path))
  (if (and closed final-direction)
      (error "Can't specify final direction ~s for a closed path ~s"
	     final-direction path))
  (if (and closed
	   (not (if (listp (first path))
		    (let* ((start (first path))
			   (end (car (last path))))
		      (and (= (first start)(first end))
			   (= (second start)(second end))
			   (if path-3d
			       (= (third start)(third end)) t)))
		  (let* ((end (last path (if path-3d 3 2))))
		    (and (= (first path)(first end))
			 (= (second path)(second end))
			 (if path-3d
			     (= (third path)(third end)) t))))))
      (error "Closed path ~s is not closed" path))
  ;; create the path structure
  (if closed
      (make-instance 'closed-bezier-path
		     :path path
		     :3d 3d
		     :polar polar
		     :curvature curvature
		     :error error)
    (make-instance 'open-bezier-path
		   :path path
		   :3d 3d
		   :polar polar
		   :curvature curvature
		   :error error
		   :initial-direction initial-direction
		   :final-direction final-direction)))


;;; Some convenient abbreviations

(def-optkey-fun make-polar-path (path
				 (3d path-3d)
				 (closed nil)
				 (curvature nil)
				 (error 0.01)
				 ;; only for open paths
				 initial-direction
				 final-direction)
  (if closed
      (make-path :path path
		 :3d 3d
		 :polar t
		 :closed closed
		 :curvature curvature
		 :error error)
    (make-path :path path
	       :3d 3d
	       :polar t
	       :closed closed
	       :curvature curvature
	       :error error
	       :initial-direction initial-direction
	       :final-direction final-direction)))

(def-optkey-fun make-closed-path (path
				  (3d path-3d)
				  (polar nil)
				  (curvature nil)
				  (error 0.01))
  (make-path :path path
	     :3d 3d
	     :polar polar
	     :closed t
	     :curvature curvature
	     :error error))

;;; Set components of a path and reset appropriate part of the rendering process
;;; Rendering steps:
;;;   (fit-path path)        calculate the bezier curve control points
;;;   (render-path path)     derive a linear approximation to the bezier segments

;;; Set a new cartesian set of points

(defmethod set-path ((path bezier-path) points)
  (setf (slot-value path 'path) points
	(slot-value path 'polar) nil)
  (parse-path path))

;;; Set a new polar set of points

(defmethod set-polar-path ((path bezier-path) points)
  (setf (slot-value path 'path) points
	(slot-value path 'polar) t)
  (parse-path path))

;;; Set a new path curvature

(defmethod set-path-curvature ((path bezier-path) curvature)
  (when curvature
    (setf (slot-value path 'curvature) curvature)
    (reset-fit path)))

;;; Set a new path rendering error bound

(defmethod set-path-error ((path bezier-path) error)
  (when error
    (setf (slot-value path 'error) error)
    (reset-rendering path)))

;;;
;;; Parse a path and transform it into cartesian coordinates
;;;

(defmethod not-parsed ((path bezier-path))
  (null (slot-value path 'x)))

(defmethod reset-parsing ((path bezier-path))
  (setf (slot-value path 'x) nil
	(slot-value path 'y) nil
	(slot-value path 'z) nil
	(slot-value path 'v) nil)
  (reset-fit path))

(defmethod parse-path ((path t))
  (illegal-path-argument path))

(defmethod parse-path ((path cons))
  (parse-path (make-path path)))

(defmethod parse-path :after ((path bezier-path))
  (reset-fit path))

(defmethod parse-path ((path bezier-path))
  (let* ((polar (slot-value path 'polar))
	 (points (slot-value path 'path))
	 (3d (slot-value path '3d)))
    (if polar
	;; parse a polar path
	(multiple-value-bind (x y z v)
	    (parse-polar-coordinates points 3d)
	  (setf (slot-value path 'x) x
		(slot-value path 'y) y
		(slot-value path 'z) z
		(slot-value path 'v) v))
      ;; parse a cartesian path
      (multiple-value-bind (x y z v)
	  (parse-cartesian-coordinates points 3d)
	(setf (slot-value path 'x) x
	      (slot-value path 'y) y
	      (slot-value path 'z) z
	      (slot-value path 'v) v))))
  (loop for v in (slot-value path 'v) do
    (if (and v (< v 0))(error "velocities for path ~s must be all positive"
			      (slot-value path 'path))))
  path)

;;; Parse a set of 2d or 3d points into the separate coordinates

(defun parse-cartesian-coordinates (points 3d)
  (if (listp (first points))
      ;; decode a list of lists into x:y:z:v components
      ;; 3d -> t [default]
      ;;   '((x0 y0 z0 v0)(x1 y1 z1 v1)...(xn yn zn vn))
      ;;   '((x0 y0 z0)(x1 y1 z1)...(xn yn zn))
      ;;   '((x0 y0)(x1 y1)...(xn yn)) 
      ;;      v: relative velocity
      ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
      ;; 3d -> nil
      ;;   '((x0 y0 v0)(x1 y1 v1)...(xn yn vn))
      ;;   '((x0 y0)(x1 y1)...(xn yn))
      ;;      v: relative velocity
      ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
      (loop
	for p in points
	collect (first p) into x
	collect (second p) into y
	collect (if 3d (if (third p)(third p) 0.0) 0.0) into z
	collect (if 3d 
		    (if (fourth p)(fourth p) nil)
		  (if (third p)(third p) nil)) 
	into v
	finally (return (values x y z v)))
    ;; decode a plain list
    (if 3d
	;; it's a three dimensional list
	;; '(x0 y0 z0 x1 y1 z1 ... xn yn zn)
	;;     x, y, z: coordinates of source
	(loop
	  for px in points by #'cdddr
	  for py in (cdr points) by #'cdddr
	  for pz in (cddr points) by #'cdddr
	  collect px into x
	  collect py into y
	  collect pz into z
	  finally (return (values x y z (loop repeat (length x) collect nil))))
      ;; it's a two dimensional list
      ;; '(x0 y0 x1 y1 ... xn yn)
      ;;     x, y, z: coordinates of source [missing z's assumed 0.0]
      (loop 
	for number from 0
	for px in points by #'cddr
	for py in (cdr points) by #'cddr
	collect px into x
	collect py into y
	finally (return (values x y 
				(loop repeat (length x) collect 0.0)
				(loop repeat (length x) collect nil)))))))

;;; Parse a set of 2d or 3d polar points into the separate coordinates

(defun parse-polar-coordinates (points 3d)
  ;; parse a polar path
  (if (listp (first points))
      ;; decode a list of lists of d:a:e:v into x:y:z:v components
      ;; 3d --> t [default]
      ;;   '((d0 a0 e0 v0)(d1 a1 e1 v1)...(dn an en vn))
      ;;   '((d0 a0 e0)(d1 a1 e1)...(dn an en))
      ;;   '((d0 a0)(d1 a1)...(dn an))  
      ;; 3d --> nil
      ;;   '((d0 a0 v0)(d1 a1 v1)...(dn an vn))
      ;;   '((d0 a0)(d1 a1)...(dn an))
      ;;      v: velocity
      ;;      d: distance
      ;;      a: azimut angle
      ;;      e: elevarion angle [missing elevations assumed 0.0]
      (loop 
	for p in points
	for d = (first p)
	for a = (second p)
	for e = (if 3d (if (third p)(third p) 0.0) 0.0)
	for evec = (cis (* (/ e dlocsig-one-turn) 2 pi))
	for dxy = (* d (realpart evec))
	for avec = (cis (* (/ a dlocsig-one-turn) 2 pi))
	collect (* d (imagpart evec)) into z
	collect (* dxy (imagpart avec)) into x
	collect (* dxy (realpart avec)) into y
	collect (if 3d 
		    (if (fourth p)(fourth p) nil)
		  (if (third p)(third p) nil)) 
	into v
	finally (return (values x y z v)))
    ;; decode a list of d:a:e components
    (if 3d
	;; decode a three dimensional list
	;;   '(d0 a0 e0 d1 a1 e1 ... dn an en)
	;;      d: distance
	;;      a: azimut angle
	;;      e: elevarion angle [missing elevations assumed 0.0]
	(loop 
	  for d in points by #'cdddr
	  for a in (cdr points) by #'cdddr
	  for e in (cddr points) by #'cdddr
	  for evec = (cis (* (/ e dlocsig-one-turn) 2 pi))
	  for dxy = (* d (realpart evec))
	  for avec = (cis (* (/ a dlocsig-one-turn) 2 pi))
	  collect (* d (imagpart evec)) into z
	  collect (* dxy (imagpart avec)) into x
	  collect (* dxy (realpart avec)) into y
	  finally (return (values x y z
				  ;; no velocity points, collect nil's
				  (loop repeat (length x) collect nil))))
      ;; decode a two dimensional list
      ;;   '(d0 a0 d1 a1 ... dn an)
      ;;      d: distance
      ;;      a: azimut angle
      ;;      e: elevarion angle [missing elevations assumed 0.0]
      (loop 
	for d in points by #'cddr
	for a in (cdr points) by #'cddr
	for avec = (cis (* (/ a dlocsig-one-turn) 2 pi))
	collect (* d (imagpart avec)) into x
	collect (* d (realpart avec)) into y
	finally (return (values x y
				;; no z points, collect 0's
				(loop repeat (length x) collect 0.0)
				;; no velocity points, collect nil's
				(loop repeat (length x) collect nil)))))))

;;;
;;; Bezier curve fitting auxiliary functions
;;;

;;; Pythagoras

(defun distance (x y z)
  (sqrt (+ (* x x)(* y y)(* z z))))

;;; Nearest point in a line

(defun nearest-point (x0 y0 z0 x1 y1 z1 px py pz)
  (labels (;; magnitude of a 3d vector
	   (vmag (a b c)
		 (sqrt (+ (* a a)(* b b)(* c c))))
	   ;; cos of the angle between two 3d vectors
	   (vcos (a0 b0 c0 a1 b1 c1)
		 (/ (+ (* a0 a1)(* b0 b1)(* c0 c1))
		    (* (vmag a0 b0 c0)(vmag a1 b1 c1))))
	   ;; see if two points are equal
	   (same (a0 b0 c0 a1 b1 c1)
		 (and (= a0 a1)(= b0 b1)(= c0 c1))))
    (cond ((same x0 y0 z0 px py pz)
	   (values x0 y0 z0))
	  ((same x1 y1 z1 px py pz)
	   (values x1 y1 z1))
	  ((same x0 y0 z0 x1 y1 z1)
	   (values x0 y0 z0))
	  (t
	   (let* (;; vector for line
		  (xm0 (- x1 x0))
		  (ym0 (- y1 y0))
		  (zm0 (- z1 z0))
		  ;; vector from p to origin of line
		  (xm1 (- px x0))
		  (ym1 (- py y0))
		  (zm1 (- pz z0))
		  ;; projection of p vector on line
		  (p (* (vmag xm1 ym1 zm1)(vcos xm0 ym0 zm0 xm1 ym1 zm1)))
		  (l (vmag xm0 ym0 zm0))
		  (ratio (/ p l)))
	     (values (+ x0 (* xm0 ratio))
	     (+ y0 (* ym0 ratio))
	     (+ z0 (* zm0 ratio))))))))

;;; Bezier curve fitting auxilliary functions

(defparameter path-ak-even nil)
(defparameter path-ak-odd nil)
(defparameter path-maxcoeff 8)

(defparameter path-gtab nil)

(defun make-a-even()
  (flet ((g(m)
	   (if (null path-gtab)
	       (progn
		 (setf path-gtab (make-array path-maxcoeff)
		       (aref path-gtab 0) 1
		       (aref path-gtab 1) -4)
		 (loop for i from 2 to (- path-maxcoeff 1) do
		   (setf (aref path-gtab i)(- (* -4 (aref path-gtab (- i 1)))
					 (aref path-gtab (- i 2)))))))
	   (aref path-gtab m)))
    (setf path-ak-even (make-array (- path-maxcoeff 1)))
    (loop for m from 1 to (- path-maxcoeff 1) do
      (setf (aref path-ak-even (- m 1))(make-array m))
      (loop for k from 1 to m do
	(setf (aref (aref path-ak-even (- m 1))(- k 1))
	  (float (/ (- (g (- m k)))(g m))))))))

(defparameter path-ftab nil)

(defun make-a-odd()
  (flet ((f(m)
	   (if (null path-ftab)
	       (progn
		 (setf path-ftab (make-array path-maxcoeff)
		       (aref path-ftab 0) 1
		       (aref path-ftab 1) -3)
		 (loop for i from 2 to (- path-maxcoeff 1) do
		   (setf (aref path-ftab i)(- (* -4 (aref path-ftab (- i 1)))
					 (aref path-ftab (- i 2)))))))
	   (aref path-ftab m)))
    (setf path-ak-odd (make-array (- path-maxcoeff 1)))
    (loop for m from 1 to (- path-maxcoeff 1) do
      (setf (aref path-ak-odd (- m 1))(make-array m))
      (loop for k from 1 to m do
	(setf (aref (aref path-ak-odd (- m 1))(- k 1))
	  (float (/ (- (f (- m k)))(f m))))))))

(defun a(k n)
  (if (oddp (min (+ (* path-maxcoeff 2) 1) n))
      (progn
	(if (null path-ak-odd)(make-a-odd))
	(aref (aref path-ak-odd (/ (- n 3) 2))(- k 1)))
    (progn
      (if (null path-ak-even)(make-a-even))
      (aref (aref path-ak-even (/ (- n 4) 2))(- k 1)))))

(defun ac(k n)
  (setf n (min n path-maxcoeff))
  (if (null path-ak-even)(make-a-even))
  (aref (aref path-ak-even (- n 2))(- k 1)))

;;; Calculate bezier difference vectors for the given path

(defmethod calculate-fit ((path closed-bezier-path))
  (let* ((n (- (length (slot-value path 'x)) 1))
	 (m (/ (- n (if (oddp n) 3 4)) 2))
	 ;; data points P(i)
	 (p (make-array `( 3 ,(+ n 1)) 
			:initial-contents (list (slot-value path 'x)
						(slot-value path 'y)
						(slot-value path 'z))))
	 ;; control points D(i)
	 (d (make-array `( 3 ,n) :initial-element 0.0)))
    (flet ((ref(z j i)
	     (cond ((> i (- n 1))(aref z j (- i n)))
		   ((< i 0)(aref z j (+ i n)))
		   (t (aref z j i)))))
      (loop for i from 0 below n do
	    (loop for k from 1 to m do 
	      (loop for a from 0 to 2 do
		(incf (aref d a i)
		      (* (a k n)
			 (- (ref p a (+ i k))
			    (ref p a (- i k))))))))
      (if (slot-value path 'curvature)
	  (loop for i from 0 to (- n 1)
	      for curve = (slot-value path 'curvature) do
		(setf (aref d 0 i)(* (aref d 0 i) curve)
		      (aref d 1 i)(* (aref d 1 i) curve)
		      (aref d 2 i)(* (aref d 2 i) curve))))
      (values (- n 1) p d))))


(defmethod calculate-fit ((path open-bezier-path))
  (let* ((n (- (length (slot-value path 'x)) 1))
	 (m (- n 1))
	 ;; data points P(i)
	 (p (make-array `( 3 ,(+ n 1)) 
			:initial-contents (list (slot-value path 'x)
						(slot-value path 'y)
						(slot-value path 'z))))
	 ;; control points D(i)
	 (d (make-array `( 3 ,(+ n 1)) 
			:initial-element 0.0)))
    (flet ((ref(z j i)
	     (cond ((> i n)(aref z j (- i n)))
		   ((< i 0)(aref z j (+ i n)))
		   ((= i n)(- (aref z j n)(aref d j n)))
		   ((= i 0)(+ (aref z j 0)(aref d j 0)))
		   (t (aref z j i)))))
      ;; forced initial direction
      (if (slot-value path 'initial-direction)
	  (setf (aref d 0 0)(first (slot-value path 'initial-direction))
		(aref d 1 0)(second (slot-value path 'initial-direction))
		(aref d 2 0)(third (slot-value path 'initial-direction)))
	(setf (aref d 0 0) 0.0
	      (aref d 1 0) 0.0
	      (aref d 2 0) 0.0))
      ;; forced final direction
      (if (slot-value path 'final-direction)
	  (setf (aref d 0 n)(first (slot-value path 'final-direction))
		(aref d 1 n)(second (slot-value path 'final-direction))
		(aref d 2 n)(third (slot-value path 'final-direction)))
	(setf (aref d 0 n) 0.0
	      (aref d 1 n) 0.0
	      (aref d 2 n) 0.0))
      ;; calculate fit
      (loop for i from 1 below n do
	    (loop for k from 1 to (min (- path-maxcoeff 1) m) do 
		  (incf (aref d 0 i)
			(* (ac k n)
			   (- (ref p 0 (+ i k))
			      (ref p 0 (- i k)))))
		  (incf (aref d 1 i)
			(* (ac k n)
			   (- (ref p 1 (+ i k))
			      (ref p 1 (- i k)))))
		  (incf (aref d 2 i)
			(* (ac k n)
			   (- (ref p 2 (+ i k))
			      (ref p 2 (- i k)))))))
      (values n p d))))

;;; Calculate bezier control points for the given open path

(defmethod not-fitted ((path bezier-path))
  (null (slot-value path 'bx)))

(defmethod reset-fit ((path bezier-path))
  (setf (slot-value path 'bx) nil
	(slot-value path 'by) nil
	(slot-value path 'bz) nil)
  (reset-rendering path))

(defmethod fit-path ((path t))
  (illegal-path-argument path) nil)

(defmethod fit-path ((path cons))
  (fit-path (make-path path)))

(defmethod fit-path :before ((path bezier-path))
  (if (not-parsed path)
      (parse-path path)))

(defmethod fit-path :after ((path bezier-path))
  (reset-rendering path))

(defmethod fit-path ((path open-bezier-path))
  (let* ((points (length (slot-value path 'x))))
    (cond (;; enough points, calculate fit
	   (> points 2)
	   (multiple-value-bind (n p d)
	       (calculate-fit path)
	     (let* ((c (slot-value path 'curvature))
		    (cs (make-array n)))
	       ;; setup the curvatures array
	       (cond
		(;; no curvature specified, default is 1.0
		 (null c)
		 (loop for i from 0 below n do
		   (setf (aref cs i) '(1.0 1.0))))
		(;; same curvature for all segments
		 (numberp c)
		 (loop for i from 0 below n do
		   (setf (aref cs i) (list c c))))
		(;; list of curvatures
		 (and (listp c)(= n (length c)))
		 (loop 
		   for i from 0 
		   for ci in c do
		   (setf (aref cs i)
			 (if (listp ci) 
			     (if (/= (length ci) 2)
				 (error "curvature sublist must have two elements ~s"
					ci)
			       ci)
			   (list ci ci)))))
		(;; hummm
		 t (error "bad curvature argument ~s to path, need ~s elements"
			  c n)))
	       ;; calculate control points
	       (loop 
		 for i from 0 to (- n 1)
		 collect 
		 (list (aref p 0 i)
		       (+ (aref p 0 i)(* (aref d 0 i)(first (aref cs i))))
		       (- (aref p 0 (+ i 1))(* (aref d 0 (+ i 1))(second (aref cs i))))
		       (aref p 0 (+ i 1))) into xc
		 collect 
		 (list (aref p 1 i)
		       (+ (aref p 1 i)(* (aref d 1 i)(first (aref cs i))))
		       (- (aref p 1 (+ i 1))(* (aref d 1 (+ i 1))(second (aref cs i))))
		       (aref p 1 (+ i 1))) into yc
		 collect 
		 (list (aref p 2 i)
		       (+ (aref p 2 i)(* (aref d 2 i)(first (aref cs i))))
		       (- (aref p 2 (+ i 1))(* (aref d 2 (+ i 1))(second (aref cs i))))
		       (aref p 2 (+ i 1))) into zc
		 finally (setf (slot-value path 'bx) xc
			       (slot-value path 'bz) zc
			       (slot-value path 'by) yc)))))
	  ;; just a line, stays a line
	  ((= points 2)
	   (let* ((x1 (first (slot-value path 'x)))
		  (x2 (second (slot-value path 'x)))
		  (y1 (first (slot-value path 'y)))
		  (y2 (second (slot-value path 'y)))
		  (z1 (first (slot-value path 'z)))
		  (z2 (second (slot-value path 'z))))
	     (setf (slot-value path 'bx)(list (list x1 x1 x2 x2))
		   (slot-value path 'by)(list (list y1 y1 y2 y2))
		   (slot-value path 'bz)(list (list z1 z1 z2 z2)))))
	  ;; just one point, bezier won't do much here
	  ((= points 1)
	   (setf (slot-value path 'bx) nil
		 (slot-value path 'by) nil
		 (slot-value path 'bz) nil))))
  path)

;;; Calculate bezier control points for the given closed path

(defmethod fit-path ((path closed-bezier-path))
  (if (> (length (slot-value path 'x)) 4)
      ;; enough points, fit path
      (multiple-value-bind (n p d)
	  (calculate-fit path)
	(loop 
	  for i from 0 to (- n 1)
	  collect 
	  (list (aref p 0 i)
		(+ (aref p 0 i)(aref d 0 i))
		(- (aref p 0 (+ i 1))(aref d 0 (+ i 1)))
		(aref p 0 (+ i 1))) into xc
	  collect 
	  (list (aref p 1 i)
		(+ (aref p 1 i)(aref d 1 i))
		(- (aref p 1 (+ i 1))(aref d 1 (+ i 1)))
		(aref p 1 (+ i 1))) into yc
	  collect 
	  (list (aref p 2 i)
		(+ (aref p 2 i)(aref d 2 i))
		(- (aref p 2 (+ i 1))(aref d 2 (+ i 1)))
		(aref p 2 (+ i 1))) into zc
	  finally 
	  (setf 
	   (slot-value path 'bx)
	   (nconc xc (list (list (aref p 0 n)
				 (+ (aref p 0 n)(aref d 0 n))
				 (- (aref p 0 0)(aref d 0 0))
				 (aref p 0 0))))
	   (slot-value path 'by)
	   (nconc yc (list (list (aref p 1 n)
				 (+ (aref p 1 n)(aref d 1 n))
				 (- (aref p 1 0)(aref d 1 0))
				 (aref p 1 0))))
	   (slot-value path 'bz)
	   (nconc zc (list (list (aref p 2 n)
				 (+ (aref p 2 n)(aref d 2 n))
				 (- (aref p 2 0)(aref d 2 0))
				 (aref p 2 0)))))))
    ;; not enough points to fit a closed path
    (loop 
      for x1 in (slot-value path 'x)
      for x2 in (cdr (slot-value path 'x))
      for y1 in (slot-value path 'y)
      for y2 in (cdr (slot-value path 'y))
      for z1 in (slot-value path 'z)
      for z2 in (cdr (slot-value path 'z))
      collect (list x1 x1 x2 x2) into xc
      collect (list y1 y1 y2 y2) into yc
      collect (list z1 z1 z2 z2) into zc
      finally (warn "[fit-path:closed-path] not enough points to do bezier fit (~s points)"
		    (length (slot-value path 'x)))
      finally (setf (slot-value path 'bx) xc
		    (slot-value path 'by) yc
		    (slot-value path 'bz) zc)))
  path)

;;; Transform a Bezier control point fit to a linear segment approximation

(defmethod render-path ((path t))
  (illegal-path-argument path) nil)

(defmethod render-path ((path cons))
  (render-path (make-path path)))

(defmethod render-path :before ((path bezier-path))
  (if (not-fitted path)
      (fit-path path)))

(defmethod render-path :after ((path bezier-path))
  (reset-transformation path))

(defmethod render-path ((path bezier-path))
  (let* (;; linear segment approximation of bezier segments
	 rx ry rz rv)
    (labels (;;
	     ;; Evaluate a point at parameter u in bezier segment
	     ;;
	     (bezier-point (u c)
	       (let* ((u1 (- 1 u))
		      (cr (make-array '(3 3) :element-type 'float)))
		 (loop for j from 0 to 2 do
		 (setf (aref cr 0 j)(+ (* u1 (aref c 0 j))(* u (aref c 0 (+ j 1))))
		       (aref cr 1 j)(+ (* u1 (aref c 1 j))(* u (aref c 1 (+ j 1))))
		       (aref cr 2 j)(+ (* u1 (aref c 2 j))(* u (aref c 2 (+ j 1))))))
		 (loop for i from 1 downto 0 do
		 (loop for j from 0 to i do
		   (setf (aref cr 0 j)(+ (* u1 (aref cr 0 j))(* u (aref cr 0 (+ j 1))))
			 (aref cr 1 j)(+ (* u1 (aref cr 1 j))(* u (aref cr 1 (+ j 1))))
			 (aref cr 2 j)(+ (* u1 (aref cr 2 j))(* u (aref cr 2 (+ j 1)))))))
		 (values (aref cr 0 0)
		 (aref cr 1 0)
		 (aref cr 2 0))))
	     ;;
	     ;; Create a linear segment rendering of a bezier segment
	     ;;
	     (berny (xl yl zl xh yh zh ul u uh c err)
	       (multiple-value-bind (x y z)
		   (bezier-point u c)
		 (multiple-value-bind (xn yn zn)
		     (nearest-point xl yl zl xh yh zh x y z)
		   (if (> (distance (- xn x)(- yn y)(- zn z)) err)
		       (multiple-value-bind (xi yi zi)
			   (berny xl yl zl x y z ul (/ (+ ul u) 2) u c err)
			 (multiple-value-bind (xj yj zj)
			     (berny x y z xh yh zh u (/ (+ u uh) 2) uh c err)
			   (values (nconc xi (list x) xj)
				   (nconc yi (list y) yj)
				   (nconc zi (list z) zj))))
		     (values nil nil nil))))))
      ;;
      ;; Create linear segment approximations of the bezier segments
      ;;
      ;; make sure there are initial and final velocity values
      (if (not (first (slot-value path 'v)))
	  (setf (first (slot-value path 'v)) 1
		(car (last (slot-value path 'v))) 1))
      ;; only one point means no movement, static source
      (if (= (length (slot-value path 'x)) 1)
	  (progn
	    (setf (slot-value path 'rx)(slot-value path 'x)
		  (slot-value path 'ry)(slot-value path 'y)
		  (slot-value path 'rz)(slot-value path 'z)
		  (slot-value path 'rt)'(0.0))
	    (return-from render-path path)))
      ;; render the path only if it has at least two points
      (loop 
	;; iterate over the bezier control points
	for x-bz in (slot-value path 'bx)
	for y-bz in (slot-value path 'by)
	for z-bz in (slot-value path 'bz)
	;; iterate over the path velocities
	for vi-bz in (slot-value path 'v)
	for vf-bz in (cdr (slot-value path 'v))
	;; initial and final coordinates for each segment
	for xi-bz = (first x-bz)
	for xf-bz = (car (last x-bz))
	for yi-bz = (first y-bz)
	for yf-bz = (car (last y-bz))
	for zi-bz = (first z-bz)
	for zf-bz = (car (last z-bz))
	do
	;; approximate the bezier curve with linear segments
	(multiple-value-bind (xs ys zs)
	    (berny xi-bz yi-bz zi-bz xf-bz yf-bz zf-bz 0.0 0.5 1.0 
		   (make-array '(3 4) :initial-contents (list x-bz y-bz z-bz))
		   (slot-value path 'error))
	  (setf rx `(,.rx ,xi-bz ,.xs)
		ry `(,.ry ,yi-bz ,.ys)
		rz `(,.rz ,zi-bz ,.zs)
		;; accumulate intermediate unknown velocities as nils
		rv `(,.rv ,vi-bz ,.(loop repeat (length xs) collect nil))))
	finally 
	;; add the last point
	(setf rx `(,.rx ,xf-bz)
	      ry `(,.ry ,yf-bz)
	      rz `(,.rz ,zf-bz)
	      rv `(,.rv ,vf-bz)))
      ;;
      ;; calculate times for each velocity segment
      ;;
      (loop
	  ;; initialize with start of path
	  with xseg = (list (car rx))
	  with yseg = (list (car ry))
	  with zseg = (list (car rz))
	  with vseg = (list (car rv))
	  ;; first velocity
	  with vi = (first rv)
	  ;; first time coordinate
	  with ti = 0
	  ;; acummulated times
	  with times = (list ti)
	  ;; iterate over rest of coordinates in the path
	  for x in (cdr rx)
	  for y in (cdr ry)
	  for z in (cdr rz)
	  for v in (cdr rv)
	  do
	  ;; add the point to the segment
	  (setf xseg `(,.xseg ,x)
		yseg `(,.yseg ,y)
		zseg `(,.zseg ,z)
		vseg `(,.vseg ,v))
	  ;; process the segment when it gets to a final velocity
	  (when v
	    (let* ((dseg (loop 
			   for xsi in xseg
			   for ysi in yseg
			   for zsi in zseg
			   for xsf in (cdr xseg)
			   for ysf in (cdr yseg)
			   for zsf in (cdr zseg)
			   sum (distance (- xsf xsi)(- ysf ysi)(- zsf zsi)) into sofar
			   collect sofar))
		   (df (first (last dseg)))
		   (tseg (loop 
			   with vf = v
			   with a = (/ (* (- vf vi)(+ vf vi)) df 4)
			   for d in dseg
			   collect (+ ti (if (= vf vi)
					     (/ d vi)
					   (/ (- (sqrt (+ (* vi vi)(* 4 a d))) vi)(* 2 a)))))))
	      ;; accumulate times
	      (setf times `(,.times ,.tseg))
	      ;; start the next timed segment
	      (setf xseg (list x)
		    yseg (list y)
		    zseg (list z)
		    vseg (list v)
		    vi v
		    ti (first (last tseg)))))
	  finally
	  ;; set rendered coordinates in path object
	  (setf (slot-value path 'rx) rx
		(slot-value path 'ry) ry
		(slot-value path 'rz) rz
		(slot-value path 'rt) (loop
					with tf = (first (last times))
					for ti in times
					collect (/ ti tf))))))
  path)

;; (setf p (make-path '((-10 10 0 0)(0 5 0 1)(10 10 0 0)) :error 0.01))
;; (plot-velocity p)
;; (setf p (make-path '((-10 10 0 1)(-7 7 0 0.9)(0 5 0 0)(7 7 0 0.2)(10 10 0 1)) :error 0.001))
;; (with-sound(:channels 4 :play nil)(sinewave 0 2 880 0.5 :path p))

;;;;;;;;;;;;;;;;;
;;; Literal paths
;;;;;;;;;;;;;;;;;

;;; Generic literal path class
(defclass literal-path (path)
  (;; points 
   (points :initform '() :initarg :points)
   ;; it is parsed as a 3d or 2d path?
   (3d :initform t :initarg :3d)
   ;; by default a path is cartesian
   (polar :initform nil :initarg :polar)))


;;; Generic literal path creation function
(def-optkey-fun make-literal-path ((points nil)
				   (3d path-3d)
				   (polar nil))
  (make-instance 'literal-path 
		 :points points
		 :3d 3d
		 :polar polar))

;;; Specific polar literal path creation function
(def-optkey-fun make-literal-polar-path ((points nil)
					 (3d path-3d))
  (make-instance 'literal-path 
		 :points points
		 :3d 3d
		 :polar t))

;;; Render a user-defined literal path from the data points

(defmethod render-path :after ((path literal-path))
  (reset-transformation path))

(defmethod render-path ((path literal-path))
  ;; decode the points into coordinates
  (let* ((points (slot-value path 'points))
	 (3d (slot-value path '3d))
	 (polar (slot-value path 'polar)))
    (if polar
	;; parse a polar path
	(multiple-value-bind (x y z v)
	    (parse-polar-coordinates points 3d)
	  (setf (slot-value path 'rx) x
		(slot-value path 'ry) y
		(slot-value path 'rz) z
		(slot-value path 'rv) v))
      ;; parse a cartesian path
      (multiple-value-bind (x y z v)
	  (parse-cartesian-coordinates points 3d)
	(setf (slot-value path 'rx) x
	      (slot-value path 'ry) y
	      (slot-value path 'rz) z
	      (slot-value path 'rv) v)))
    ;; make sure there are initial and final velocity values
    (if (not (first (slot-value path 'rv)))
	(setf (first (slot-value path 'rv)) 1
	      (car (last (slot-value path 'rv))) 1))
    ;; only one point means no movement, static source
    (if (= (length (slot-value path 'rx)) 1)
	(progn
	  (setf (slot-value path 'rt)'(0.0))
	  (return-from render-path path)))
    ;; calculate times for each velocity segment
    (loop
      with rx = (slot-value path 'rx)
      with ry = (slot-value path 'ry)
      with rz = (slot-value path 'rz)
      with rv = (slot-value path 'rv)
      ;; initialize with start of path
      with xseg = (list (car rx))
      with yseg = (list (car ry))
      with zseg = (list (car rz))
      with vseg = (list (car rv))
      ;; first velocity
      with vi = (first rv)
      ;; first time coordinate
      with ti = 0
      ;; acummulated times
      with times = (list ti)
      ;; iterate over rest of coordinates in the path
      for x in (cdr rx)
      for y in (cdr ry)
      for z in (cdr rz)
      for v in (cdr rv)
      do
      ;; add the point to the segment
      (setf xseg `(,.xseg ,x)
	    yseg `(,.yseg ,y)
	    zseg `(,.zseg ,z)
	    vseg `(,.vseg ,v))
      ;; process the segment when it gets to a final velocity
      (when v
	(let* ((dseg (loop 
		       for xsi in xseg
		       for ysi in yseg
		       for zsi in zseg
		       for xsf in (cdr xseg)
		       for ysf in (cdr yseg)
		       for zsf in (cdr zseg)
		       sum (distance (- xsf xsi)(- ysf ysi)(- zsf zsi)) into sofar
		       collect sofar))
	       (df (first (last dseg)))
	       (tseg (loop 
		       with vf = v
		       with a = (/ (* (- vf vi)(+ vf vi)) df 4)
		       for d in dseg
		       collect (+ ti (if (= vf vi)
					 (/ d vi)
				       (/ (- (sqrt (+ (* vi vi)(* 4 a d))) vi)(* 2 a)))))))
	  ;; accumulate times
	  (setf times `(,.times ,.tseg))
	  ;; start the next timed segment
	  (setf xseg (list x)
		yseg (list y)
		zseg (list z)
		vseg (list v)
		vi v
		ti (first (last tseg)))))
      finally
      ;; set rendered time coordinates in path object
      (setf (slot-value path 'rt)
	    (loop
	      with tf = (first (last times))
	      for ti in times
	      collect (/ ti tf))))
    path))

;;;;;;;;;;;
;;; Spirals
;;;;;;;;;;;

(defclass spiral-path (literal-path)
  (;; start angle
   (start-angle :initform 0d0 :initarg :start-angle)
   ;; total angle for the spiral
   (total-angle :initform nil :initarg :total-angle)
   ;; step angle for rendering
   (step-angle :initform (/ dlocsig-one-turn 100) :initarg :step-angle)
   ;; fractional number of turns
   (turns :initform '() :initarg :turns)
   ;; distance envelope
   (distance :initform '(0 10 1 10) :initarg :distance)
   ;; height envelope
   (height :initform '(0 0 1 0) :initarg :height)
   ;; velocity envelope
   (velocity :initform '(0 1 1 1) :initarg :velocity)))

;;; Spiral path creation function

(def-optkey-fun make-spiral-path ((start-angle 0d0)
				  total-angle
				  (step-angle (/ dlocsig-one-turn 100))
				  turns
				  (distance '(0 10 1 10))
				  (height '(0 0 1 0))
				  (velocity '(0 1 1 1)))
  (if (and total-angle turns)
      (error "can't specify total-angle [~s] and turns [~s] at the same time for the spiral path"
	     total-angle turns))
  (make-instance 'spiral-path 
		 :start-angle start-angle
		 :total-angle total-angle
		 :step-angle step-angle
		 :turns turns
		 :distance distance
		 :height height
		 :velocity velocity))

;;; Render a spiral path from the object data

(defmethod render-path :after ((path spiral-path))
  (reset-transformation path))

(defmethod render-path ((path spiral-path))
  (let* (;; express angles in radians
	 (start (* (/ (slot-value path 'start-angle) dlocsig-one-turn) 2 pi))
	 (total (if (slot-value path 'total-angle)
		    (* (/ (slot-value path 'total-angle) dlocsig-one-turn) 2 pi)
		  (if (slot-value path 'turns)
		      (* (slot-value path 'turns) 2 pi)
		    (error "a spiral-path needs either a total-angle or turns, none specified"))))
	 ;; approximate number of steps
	 (steps (abs (/ total 
			(* (/ (slot-value path 'step-angle) dlocsig-one-turn) 2 pi))))
	 ;; closest number of steps that matches the total angle
	 (step (/ total (ceiling steps)
		  (if (< (slot-value path 'step-angle) 0) -1 1)))
	 ;; normalize envelopes to the total angle
	 (distance (x-norm (slot-value path 'distance) total))
	 (height (x-norm (slot-value path 'height) total)))
    ;; create the rendered coordinate lists
    (loop
      with dp
      with df
      with tp
      with segments = (abs (/ total step))
      repeat (+ segments 1)
      for angle from start by step
      for xy = (cis angle)
      for d = (envelope-interp angle distance)
      collect (* d (imagpart xy)) into x
      collect (* d (realpart xy)) into y
      collect (envelope-interp angle height) into z
      finally (setf ;; distances
	            dp (loop
			 for xi in x
			 for xf in (cdr x)
			 for yi in y
			 for yf in (cdr y)
			 for zi in z
			 for zf in (cdr z)
			 sum (distance (- xf xi)(- yf yi)(- zf zi)) into sofar
			 collect sofar)
		    df (first (last dp))
		    ;; calculated times
		    tp (loop
			 with vp = (x-norm (slot-value path 'velocity) df)
			 for di in dp
			 for df in (cdr dp)
			 for vi = (envelope-interp di vp)
			 for vf = (envelope-interp df vp)
			 for td = 0 then (+ td (/ (- df di)(+ vi vf) 2))
			 collect td)
		    (slot-value path 'rx) x
		    (slot-value path 'ry) y
		    (slot-value path 'rz) z
		    (slot-value path 'rt)(loop
					   with tf = (first (last tp))
					   for ti in tp
					   collect (/ ti tf))))
    path))


;;;;;;;;;;;;;;;;;;;
;;; Transformations
;;;;;;;;;;;;;;;;;;;

;;; Transform a rendered path using scaling, translation and rotation 

(defmethod transform-path ((path t)
			   &key
			   scaling
			   translation
			   rotation
			   rotation-center
			   rotation-axis)
  (declare (ignore scaling translation rotation rotation-center rotation-axis))
  (illegal-path-argument path) 
  nil)

(defmethod transform-path ((path cons)
			   &key
			   scaling
			   translation
			   rotation
			   rotation-center
			   rotation-axis)
  (transform-path (make-path path) 
		  :scaling scaling
		  :translation translation
		  :rotation rotation
		  :rotation-center rotation-center
		  :rotation-axis rotation-axis))

;;; Derive a rotation matrix from an axis vector and an angle

(defun rotation-matrix (x y z angle)
  ;;; translated from C routine by David Eberly
  ;;; (http://www.magic-software.com/)
  (flet ((normalize (a b c)
           (let* ((mag (sqrt (+ (* a a)(* b b)(* c c)))))
	     (values (/ a mag)(/ b mag)(/ c mag)))))
    (multiple-value-bind (dx dy dz)
	(normalize x y z)
      (let* ((rotate (make-array '(3 3)))
	     (I (make-array '(3 3)))
	     (A (make-array '(3 3)))
	     (AA (make-array '(3 3)))
	     ;; in our frame of reference angle > 0 is clockwise
	     (sn (sin (- angle)))
	     (omcs (- 1 (cos (- angle)))))
	(setf ;; identity matrix
	      (aref I 0 0) 1d0
	      (aref I 0 1) 0d0
	      (aref I 0 2) 0d0
	      (aref I 1 0) 0d0
	      (aref I 1 1) 1d0
	      (aref I 1 2) 0d0
	      (aref I 2 0) 0d0
	      (aref I 2 1) 0d0
	      (aref I 2 2) 1d0
	      ;; infinitesimal rotation
	      (aref A 0 0) 0d0
	      (aref A 0 1) dz
	      (aref A 0 2)(- dy)
	      (aref A 1 0)(- dz)
	      (aref A 1 1) 0d0
	      (aref A 1 2) dx
	      (aref A 2 0) dy
	      (aref A 2 1)(- dx)
	      (aref A 2 2) 0d0)
	;; AA = A*A 
	(loop for row from 0 below 3 do
	  (loop for col from 0 below 3 do
	    (setf (aref AA row col) 0d0)
	    (loop for mid from 0 below 3 do
	      (incf (aref AA row col)(* (aref A row mid)(aref A mid col))))))
	;; rotation matrix is I+sin(angle)*A+[1-cos(angle)]*A*A 
	(loop for row from 0 below 3 do
	  (loop for col from 0 below 3 do
	    (setf (aref rotate row col)
		  (+ (aref I row col)
		     (* sn (aref A row col))
		     (* omcs (aref AA row col))))))
	rotate))))

;;; Transform a path (scaling + translation + rotation)

(defmethod transform-path ((path path)
			   &key
			   scaling
			   translation
			   rotation
			   rotation-center
			   (rotation-axis '(0d0 0d0 1d0)))
  (if (not-rendered path)
      (render-path path))
  (if (or scaling translation rotation)
      ;; there's at least one transformation to execute
      (let* ((rotation (if rotation (* 2 pi (/ rotation dlocsig-one-turn)) nil))
	     (matrix (if rotation (rotation-matrix (first rotation-axis)
						   (second rotation-axis)
						   (third rotation-axis)
						   rotation)
		       nil))
	     (xc (path-x path))
	     (yc (path-y path))
	     (zc (path-z path)))
	(if (and rotation-center (/= (length rotation-center) 3))
	    (error "rotation center has to have all three coordinates"))
	(if (and rotation-axis (/= (length rotation-axis) 3))
	    (error "rotation axis has to have all three coordinates"))
	(loop 
	  for x in xc
	  for y in yc
	  for z in zc 
	  for xw = x
	  for yw = y
	  for zw = z do
	  ;; rotating around non-triple zero? translate first
	  (if (and rotation-center rotation)
	      (progn
		(decf xw (first rotation-center))
		(decf yw (second rotation-center))
		(decf zw (third rotation-center))))
	  ;; rotation
	  (if rotation
	      (let* ((xr (+ (* (aref matrix 0 0) xw)
			    (* (aref matrix 1 0) yw)
			    (* (aref matrix 2 0) zw)))
		     (yr (+ (* (aref matrix 0 1) xw)
			    (* (aref matrix 1 1) yw)
			    (* (aref matrix 2 1) zw)))
		     (zr (+ (* (aref matrix 0 2) xw)
			    (* (aref matrix 1 2) yw)
			    (* (aref matrix 2 2) zw))))
		(setf xw xr
		      yw yr
		      zw zr)))
	  ;; rotating around non-triple zero? untranslate
	  (if (and rotation-center rotation)
	      (progn
		(incf xw (first rotation-center))
		(incf yw (second rotation-center))
		(incf zw (third rotation-center))))
	  ;; scaling
	  (if scaling
	      (progn
		(setf xw (* xw (first scaling)))
		(if (second scaling)
		    (setf yw (* yw (second scaling))))
		(if (third scaling)
		    (setf zw (* zw (third scaling))))))
	  ;; translating
	  (if translation
	      (progn
		(incf xw (first translation))
		(if (second translation)
		    (incf yw (second translation)))
		(if (third translation)
		    (incf zw (third translation)))))
	  ;; collect the points
	  collect xw into xtr
	  collect yw into ytr
	  collect zw into ztr
	  ;; set the path to the new trajectory
	  finally (setf (slot-value path 'tx) xtr
			(slot-value path 'ty) ytr
			(slot-value path 'tz) ztr)))
    ;; if there's no transformation just copy the rendered path
    (setf (slot-value path 'tt)(copy-list (slot-value path 'rt))
	  (slot-value path 'tx)(copy-list (slot-value path 'rx))
	  (slot-value path 'ty)(copy-list (slot-value path 'ry))
	  (slot-value path 'tz)(copy-list (slot-value path 'rz))))
  path)

;;; Scale a path

(defmethod scale-path ((path path) scaling)
  (transform-path path :scaling scaling))

;;; Translate a path

(defmethod translate-path ((path path) translation)
  (transform-path path :translation translation))

;;; Rotate a path

(defmethod rotate-path ((path path) rotation
			&key
			rotation-center
			(rotation-axis '(0d0 0d0 1d0)))
  (transform-path path 
		  :rotation rotation 
		  :rotation-center rotation-center
		  :rotation-axis rotation-axis))

;;; Mirror a path around an axis

(defun mirror-path (path &key (axis 'y)
			      (around 0))
  (if (not-transformed path)
      (transform-path path))
  (if (equal axis 'y)
      (setf (slot-value path 'tx)
	(loop for x in (slot-value path 'tx) collect (- around x)))
    (setf (slot-value path 'ty)
      (loop for y in (slot-value path 'ty) collect (- around y))))
  path)

;;; Change the times of the rendered envelope so that the velocity is constant

(defun constant-velocity (path)
  (if (not (slot-value path 'rx))
      (render-path path))
  (reset-transformation path)
  (let* ((xcoords (path-x path))
	 (ycoords (path-y path))
	 (zcoords (path-z path))
	 (tcoords (path-time path))
	 (total-distance (loop 
			     for x1 in xcoords
			     for y1 in ycoords
			     for z1 in zcoords
			     for x2 in (cdr xcoords)
			     for y2 in (cdr ycoords)
			     for z2 in (cdr zcoords)
			     sum (distance (- x2 x1)(- y2 y1)(- z2 z1))))
	 (start-time (first tcoords))
	 (end-time (first (last tcoords)))
	 (total-time (- end-time start-time))
	 (velocity (/ total-distance total-time)))
    (loop 
	for xp in xcoords
	for yp in ycoords
	for zp in zcoords
	for x in (cdr xcoords)
	for y in (cdr ycoords)
	for z in (cdr zcoords)
	sum (distance (- x xp)(- y yp)(- z zp)) into dist
	collect (/ dist velocity) into now
	finally (setf (slot-value path 'rt)(nconc (list start-time) now)
		      (slot-value path 'tx)(copy-list (slot-value path 'rx))
		      (slot-value path 'ty)(copy-list (slot-value path 'ry))
		      (slot-value path 'tz)(copy-list (slot-value path 'rz)))))
  path)


;;;;;;;;;;;;
;;; Plotting
;;;;;;;;;;;;

;;; Return the trajectory of the path

(defmethod path-trajectory ((path path))
  (let* ((xp (path-x path))
	 (yp (path-y path))
	 (zp (path-z path)))
    (loop 
      for x in xp
      for y in yp
      for z in zp
      collect x collect y collect z)))

(defmethod path-2d-trajectory ((path path))
  (let* ((xp (path-x path))
	 (yp (path-y path)))
    (loop 
      for x in xp
      for y in yp
      collect x collect y)))

;;; Return the velocity as a function of time

(defmethod path-velocity ((path path))
  (let* ((xp (path-x path))
	 (yp (path-y path))
	 (zp (path-z path))
	 (tp (path-time path)))
    (loop 
      for ti in tp
      for xi in xp
      for yi in yp
      for zi in zp
      for tf in (cdr tp)
      for xf in (cdr xp)
      for yf in (cdr yp)
      for zf in (cdr zp)
      for v = (/ (distance (- xf xi)(- yf yi)(- zf zi))
		 (- tf ti))
      collect (/ (+ ti tf) 2) collect v)))

;;; Return the doppler shift as a function of time

(defmethod path-doppler ((path path))
  (let* ((xp (path-x path))
	 (yp (path-y path))
	 (zp (path-z path))
	 (tp (path-time path)))
    (loop 
	for ti in tp
	for xi in xp
	for yi in yp
	for zi in zp
	for tf in (cdr tp)
	for xf in (cdr xp)
	for yf in (cdr yp)
	for zf in (cdr zp)
	collect (/ (+ tf ti) 2)
	collect (- (/ (- (distance xf yf zf)(distance xi yi zi))
		      (- tf ti))))))

;;; Return acceleration as a function of time

(defmethod path-acceleration ((path path))
  (let* ((v (path-velocity path)))
    (loop 
      for ti in v by #'cddr
      for vi in (cdr v) by #'cddr
      for tf in (cddr v) by #'cddr
      for vf in (cdddr v) by #'cddr
      for am = (/ (- vf vi)
		  (- tf ti))
      collect ti collect am
      collect tf collect am)))

;;; Plot the trajectory of a path

(defmethod plot-trajectory ((path path)
			    &key
			    (xrot)
			    (zrot)
			    (scale)
			    (zscale)
			    (label "trajectory")
			    (reset t))
  (if reset (plot-reset))
  (plot-set-autoscale)
  (let* ((3d (loop for z in (path-z path) do
	       (if (/= z 0)(return t))
	       finally (return nil))))
    (if 3d
	(plot-3d-curve (path-trajectory path)
		       :label label
		       :xrot xrot 
		       :zrot zrot
		       :scale scale 
		       :zscale zscale)
      (plot-2d-curve (path-2d-trajectory path)
		     :label label))))

;;; Plot the velocity of a path

(defmethod plot-velocity ((path path)
			  &key (reset t))
  (if reset (plot-reset))
  (plot-set-autoscale)
  (plot-2d-curve (path-velocity path) 
		 :label "velocity"
		 :style "steps"))

;;; Plot the doppler shift of a path

(defmethod plot-doppler ((path path)
			  &key (reset t))
  (if reset (plot-reset))
  (plot-reset)
  (plot-set-autoscale)
  (plot-2d-curve (path-doppler path) 
		 :label "doppler"
		 :style "steps"))

;;; Plot the acceleration of a path

(defmethod plot-acceleration ((path path)
			      &key (reset t))
  (if reset (plot-reset))
  (plot-reset)
  (plot-set-autoscale)
  (plot-2d-curve (path-acceleration path) 
		 :label "acceleration"
		 :style "steps"))

;; Multiplot trajectory, velocity, acceleration and doppler

(defmethod plot ((path path)
		 &key
		 (normalize t))
  (flet ((norm (env norm)
           (if (not norm)
	       env
	     (let* ((max (loop for y in (cdr env) by #'cddr maximize (abs y))))
	       (if (= max 0)
		   env
		 (loop
		   for time in env by #'cddr
		   for y in (cdr env) by #'cddr
		   collect time collect (/ y max)))))))
    (plot-reset)
    (plot-size 0 0 1 1)
    (plot-start-multiplot)
    ;; needed to let gnuplot pop up the plot window
    ;; otherwise plot window stays blank...
    (sleep 1)
    (plot-size 0 1/3 1 2/3)
    (plot-trajectory path :reset nil)
    (plot-size 0 0 1 1/3)
    (plot-2d-curves (list (norm (path-velocity path) normalize)
			  (norm (path-acceleration path) normalize)
			  (norm (path-doppler path) normalize))
		    :labels (list "velocity" 
				  "acceleration" 
				  "doppler")
		    :styles (list "steps"
				  "steps"
				  "steps"))
    (plot-end-multiplot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GnuPlot based plotting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar plot-stream nil)
(defvar plot-error nil)
(defvar plot-pid nil)

;;; Open a connection to a gnuplot process

#+excl
(defun open-plot ()
  (multiple-value-bind (input error pid)
      (excl:run-shell-command "gnuplot" 
			      :wait nil
			      :input :stream
			      :error-output :output)
    (setf plot-stream input
	  plot-error error
	  plot-pid pid)))

;;; Close and terminate gnuplot

#+excl
(defun close-plot ()
  (when plot-stream
    (format plot-stream "quit~%")
    (close plot-stream)
    (setf plot-stream nil)
    (multiple-value-bind (status pid)
        (sys:os-wait)
      (declare (ignore status pid)))))

#+cmu (defun open-plot ()
	(let ((*process* (ext:run-program "gnuplot" nil
					  :wait nil
					  :input :stream
					  :error :output)))
	  (setf *plot-process* *process*
		plot-stream (ext:process-input *process*)
		plot-error (ext:process-error *process*)
		plot-pid (ext:process-pid *process*))))

#+cmu (defun close-plot ()
	(when plot-stream
	  (format plot-stream "quit~%")
	  (ext:process-close *plot-process*)
	  (ext:process-wait *plot-process*)))

;;; Send an arbitrary command to gnuplot

(defun plot-command (&optional (command ""))
  (if (not plot-stream)
      (open-plot))
  (format plot-stream command)
  (format plot-stream "~%")
  (finish-output plot-stream))

;;; Reset all 'set' options to the default values

(defun plot-reset ()
  (plot-command "reset"))

;;; Set autoscale for selected axes

(defun plot-set-autoscale ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set autoscale~%")
  (finish-output plot-stream))

;;; Set x range

(defun plot-set-x-range (range)
  (if (not plot-stream)
      (open-plot))
  (when range
    (format plot-stream "set xrange [~f:~f]~%"
	    (first range)(second range))
    (finish-output plot-stream)))

;;; Set y range

(defun plot-set-y-range (range)
  (if (not plot-stream)
      (open-plot))
  (when range
    (format plot-stream "set yrange [~f:~f]~%"
	    (first range)(second range))
    (finish-output plot-stream)))

;;; Set z range

(defun plot-set-z-range (range)
  (if (not plot-stream)
      (open-plot))
  (when range
    (format plot-stream "set zrange [~f:~f]~%"
	    (first range)(second range))
    (finish-output plot-stream)))

;;; Set grid

(defun plot-set-grid ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set grid xtics; set grid ytics; set grid ztics~%")
  (finish-output plot-stream))

;;; Set surface

(defun plot-set-surface ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set surface~%")
  (finish-output plot-stream))

;;; Set parametric mode

(defun plot-set-parametric ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set parametric~%")
  (finish-output plot-stream))

;;; Set ticslevel

(defun plot-set-ticslevel (&optional (level 0))
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set ticslevel ~s~%" level)
  (finish-output plot-stream))

;;; Set title

(defun plot-set-title (&optional 
		       (title nil))
  (if (not plot-stream)
      (open-plot))
  (when title
    (format plot-stream "set title ~s~%" title)
    (finish-output plot-stream)))

;;; Set the labels for a plot

(defun plot-set-label (&optional 
		       (label nil))
  (if (not plot-stream)
      (open-plot))
  (when label
    (format plot-stream "set label ~s~%" label)
    (finish-output plot-stream)))

;;; Set the margins of a plot

(defun plot-set-margins (&optional 
			 (margin 1))
  (if (not plot-stream)
      (open-plot))
  (when margin
    (format plot-stream "set tmargin ~s~%" margin)
    (format plot-stream "set lmargin ~s~%" margin)
    (format plot-stream "set rmargin ~s~%" margin)
    (format plot-stream "set bmargin ~s~%" margin)
    (finish-output plot-stream)))

;;; Set the borders of a plot

(defun plot-set-border (&optional 
		       (border nil))
  (if (not plot-stream)
      (open-plot))
  (when border
    (format plot-stream "set border ~s~%" border)
    (finish-output plot-stream)))

;;; Start a multiplot

(defun plot-start-multiplot ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set multiplot~%")
  (finish-output plot-stream))

;;; End a multiplot

(defun plot-end-multiplot ()
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set nomultiplot~%")
  (finish-output plot-stream))

;;; Set origin and size of plot area

(defun plot-size (xorigin yorigin xsize ysize)
  (if (not plot-stream)
      (open-plot))
  (format plot-stream "set origin ~s,~s~%" 
	  (coerce xorigin 'float)
	  (coerce yorigin 'float))
  (format plot-stream "set size ~s,~s~%" 
	  (coerce xsize 'float)
	  (coerce ysize 'float))
  (finish-output plot-stream))

;;; Simple data plot

(defun plot-data (data 
		  &key
		  (style "linespoints")
		  (label ""))
  (if (not plot-stream)
      (open-plot))
  (plot-set-grid)
  (format plot-stream "plot '-'")
  (if label
      (format plot-stream " title ~s" label))
  (if style
      (format plot-stream " with ~a" style))
  (format plot-stream "~%")
  (loop 
    for x from 0 
    for y in data
    do (format plot-stream "~f ~f~%" x y))
  (format plot-stream "e~%")
  (finish-output plot-stream))

;;; Plot a supplied curve

(defun plot-2d-curve (curve 
		      &key
		      (style "linespoints")
		      (label ""))
  (if (not plot-stream)
      (open-plot))
  (plot-set-grid)
  (format plot-stream "plot '-'")
  (if label
      (format plot-stream " title ~s" label))
  (if style
      (format plot-stream " with ~a" style))
  (format plot-stream "~%")
  (loop 
    for x in curve by #'cddr
    for y in (cdr curve) by #'cddr
    do (format plot-stream "~f ~f~%" x y))
  (format plot-stream "e~%")
  (finish-output plot-stream))

;; Plot a list of supplied curves

(defun plot-2d-curves (curves 
		       &key
		       (styles "linespoints")
		       (labels ""))
  (if (not plot-stream)
      (open-plot))
  (plot-set-grid)
  (if (not (listp styles))
      (setf styles (loop repeat (length curves)
		     collect styles)))
  (if (not (listp labels))
      (setf labels (loop repeat (length curves)
		     collect labels)))
  (format plot-stream "plot")
  (loop 
    for index from 0
    for style in styles
    for label in labels do
    (format plot-stream " '-' ")
    (if label
	(format plot-stream " title ~s" label))
    (if style
	(format plot-stream " with ~a" style))
    (if (/= index (- (length curves) 1))
	(format plot-stream ", ")))
  (format plot-stream "~%")
  (loop for curve in curves do		    
    (loop 
      for x in curve by #'cddr
      for y in (cdr curve) by #'cddr
      do (format plot-stream "~f ~f~%" x y))
    (format plot-stream "e~%"))
  (finish-output plot-stream))

;;; Plot a 3d curve

(defun plot-3d-curve (3d-curve 
		      &key
		      (style "linespoints")
		      (label "")
		      (zstyle "impulses")
		      (xrot)
		      (zrot)
		      (scale)
		      (zscale))
  (if (not plot-stream)
      (open-plot))
  (plot-set-border (+ 127 256 512))
  (plot-set-grid)
  (plot-set-surface)
  (plot-set-parametric)
  (plot-set-ticslevel 0)
  (if (or xrot zrot scale zscale)
      (format plot-stream "set view ~a,~a,~a,~a~%"
	      (if xrot xrot "")
	      (if zrot zrot "")
	      (if scale scale "")
	      (if zscale zscale "")))
  (format plot-stream "splot '-'")
  (if label
      (format plot-stream " title ~s" label))
  (if style
      (format plot-stream " with ~a 1" style))
  (if zstyle
      (format plot-stream ", '-' notitle with ~a 1" zstyle))
  (format plot-stream "~%")
  (loop 
    for x in 3d-curve by #'cdddr
    for y in (cdr 3d-curve) by #'cdddr
    for z in (cddr 3d-curve) by #'cdddr do 
    (format plot-stream "~f ~f ~f~%" x y z))
  (format plot-stream "e~%")
  (if zstyle (loop 
	       for x in 3d-curve by #'cdddr
	       for y in (cdr 3d-curve) by #'cdddr
	       for z in (cddr 3d-curve) by #'cdddr do 
	       (format plot-stream "~f ~f ~f~%" x y z)
	       finally (format plot-stream "e~%")))
  (finish-output plot-stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quad version of NREV (the most popular Samson box reverb)
#|
(defun prime (val)
  (or (= val 2)
      (and (oddp val)
	   (do ((i 3 (+ i 2))
		(lim (sqrt val)))
	       ((or (= 0 (mod val i)) (> i lim))
		(> i lim))))))
       
(defvar dlocnrev-reverb-factor 1.09)
(defvar dlocnrev-lp-coeff 0.7)
(defvar dlocnrev-lp-out-coeff 0.85)
(defvar dlocnrev-output-scale 1.0)
(defvar dlocnrev-volume 1.0)

(definstrument dlocnrev
    (start-time duration 
		&key 
		(reverb-factor dlocnrev-reverb-factor)
		(lp-coeff dlocnrev-lp-coeff)
		(lp-out-coeff dlocnrev-lp-out-coeff)
		(output-scale dlocnrev-output-scale)
		(volume dlocnrev-volume))

  ;; reverb-factor controls the length of the decay -- it should not exceed (/ 1.0 .823)
  ;; lp-coeff controls the strength of the low pass filter inserted in the feedback loop
  ;; output-scale can be used to boost the reverb output

  (let* ((srscale (/ *srate* 25641))
	 (val 0)
	 (dly-len (make-array 15 
			      :element-type 'fixnum 
			      :initial-contents '(1433 1601 1867 2053 2251 2399 347 113 37 59 53 43 37 29 19))))
    (loop 
	for i below 15 do
	  (setf val (floor (* srscale (aref dly-len i))))
	  (if (= 0 (mod val 2)) (incf val))
	  (loop while (not (prime val)) do (incf val 2))
	  (setf (aref dly-len i) val))
    (let* ((comb1 (make-comb (* .822 reverb-factor)(aref dly-len 0)))
	   (comb2 (make-comb (* .802 reverb-factor)(aref dly-len 1)))
	   (comb3 (make-comb (* .773 reverb-factor)(aref dly-len 2)))
	   (comb4 (make-comb (* .753 reverb-factor)(aref dly-len 3)))
	   (comb5 (make-comb (* .753 reverb-factor)(aref dly-len 4)))
	   (comb6 (make-comb (* .733 reverb-factor)(aref dly-len 5)))
	   (low (make-one-pole lp-coeff (- lp-coeff 1.0)))
	   (low-a (make-one-pole lp-out-coeff (- lp-coeff 1.0)))
	   (low-b (make-one-pole lp-out-coeff (- lp-coeff 1.0)))
	   (low-c (make-one-pole lp-out-coeff (- lp-coeff 1.0)))
	   (low-d (make-one-pole lp-out-coeff (- lp-coeff 1.0)))
	   (allpass1 (make-all-pass -0.700 0.700 (aref dly-len 6)))
	   (allpass2 (make-all-pass -0.700 0.700 (aref dly-len 7)))
	   (allpass3 (make-all-pass -0.700 0.700 (aref dly-len 8)))
	   (allpass4 (make-all-pass -0.700 0.700 (aref dly-len 10))) ; 9 for stereo 
	   (allpass5 (make-all-pass -0.700 0.700 (aref dly-len 11)))
	   (allpass6 (make-all-pass -0.700 0.700 (aref dly-len 12)))
	   (allpass7 (make-all-pass -0.700 0.700 (aref dly-len 13)))
	   (allpass8 (make-all-pass -0.700 0.700 (aref dly-len 14)))
	   (amp-env (make-env :envelope '(0 1 1 1)
			      :start-time start-time
			      :duration duration))
	   (sample-a 0.0)
	   (sample-b 0.0)
	   (sample-c 0.0)
	   (sample-d 0.0)
	   (rev 0.0)
	   (outrev 0.0)
	   (beg (floor (* start-time *srate*)))
	   (end (+ beg (floor (* duration *srate*)))))
      (run
       (loop for i from beg to end do
	     (setf rev (* volume (env amp-env)(revin i)))
	     (setf outrev
	       (all-pass allpass4
			 (one-pole low
				   (all-pass allpass3
					     (all-pass allpass2
						       (all-pass allpass1
								 (+ (comb comb1 rev)
								    (comb comb2 rev)
								    (comb comb3 rev)
								    (comb comb4 rev)
								    (comb comb5 rev)
								    (comb comb6 rev))))))))
	     (setf sample-a (* output-scale (one-pole low-a (all-pass allpass5 outrev)))
		   sample-b (* output-scale (one-pole low-b (all-pass allpass6 outrev)))
		   sample-c (* output-scale (one-pole low-c (all-pass allpass7 outrev)))
		   sample-d (* output-scale (one-pole low-d (all-pass allpass8 outrev))))
	     ;; send reverb to output streams, fold back channels if stereo
	     (outa i (if (stereo) 
			 (/ (+ sample-a sample-d) 2)
		       sample-a))
	     (if (or (stereo)(quad))
		 (outb i (if (stereo)
			     (/ (+ sample-b sample-c) 2)
			   sample-b)))
	     (if (quad)
		 (progn (outc i sample-c)
			(outd i sample-d)))))
      (end-run))))
|#
