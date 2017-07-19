;; Lisp examples

;; Also look at test.lisp in the fomus directory--you will find examples of a few more features there
;; You'll probably run into bugs--e-mail me (dpsenick(at)uiuc.edu) if you have problems (and e-mail me /tmp/fomus.dbg)
;; Docs are at http://common-lisp.net/project/fomus/doc/



;; To load:
(load "/lisp/fomus/load.lisp") ; loads fomus
(use-package :fomus) ; call this to make all of fomus available (unnecessary if you're just using CM's interface)
(in-package :cm)
;; To see output:
;; Change the filename in the "events" functions to whatever filename you want (it needs to end in ".ly").
;; Example: "/my/favorite/directory/output.ly"
;; Once the file is generated, dropping and dragging the file onto the LilyPond application (in the Applications folder)
;; should make LilyPond process the file and display the results.  If it doesn't display them
;; automatically, use the Preview application on the .ps or .pdf file that is generated.

;; Examples for both CM and regular Lisp are given--in each case they should do the exact same thing (unless I made a mistake).



;; ----------------------------------------------------------------------------------------------------
;; simple drunk process in CM
(progn
  (define (cm-example beat len note1 part)
      (process
	repeat (floor (/ len beat))
	for note = (keynum note1) then (drunk note 2)
	output (new note :off (now) :dur beat :note note :partid part)
	wait beat))
  (defparameter piano (new part :partid 'piano :instr :piano))
  (events (cm-example 1/4 10 'c4 'piano) "output.ly" :parts (list piano)))



;; ----------------------------------------------------------------------------------------------------
;; simple example
;  (progn makes Lisp evaluate all the inside expressions sequentially)
(progn					; CM
  (define (cm-example dur len part)
      (process 
	output (new note
		 :partid part
		 :off (now)
		 :dur (if (< (now) len) dur (* dur 2))
		 :note (+ 48 (random 25)))
	until (>= (now) len)
	wait dur))
  (defparameter piano (new part :partid 'pianopart :name "Piano" :instr :piano)) ; <-- parts are required, and are 
  (events (cm-example 1/2 10 'pianopart) "output.ly" :parts (list piano)))       ;     referenced by their partids

(progn					; Lisp
  (fomus:fomus-init :backend '(:lilypond :view t))
  (fomus:fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (loop
   for off from 0 to 10 by 1/2 do
   (fomus:fomus-newnote 'pianopart
		  :off off
		  :dur (if (< off 10) 1/2 1)
		  :note (+ 48 (random 25))))
  (fomus:fomus-exec))

;; ----------------------------------------------------------------------------------------------------
;; add another voice
(progn					; CM
  (define (cm-example dur len voice part)
      (process
	for basepitch in '(42 54)
	sprout (process ; <-- sprouting a process for each voice
		 output (new note
			  :partid part
			  :off (now)
			  :voice voice ; <-- if voice is a list (like '(1 2)), it tells FOMUS to decide
			  :dur (if (< (now) len) dur (* dur 2))
			  :note (+ basepitch (random 25)))
		 until (>= (now) len)
		 wait dur)))
  (defparameter piano (new part :partid 'pianopart :name "Piano" :instr :piano))
  (events (cm-example 1/2 10 '(1 2) 'pianopart) "output.ly" :parts (list piano)))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t))
  (fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (loop
   for basepitch in '(42 54) do
   (loop
    for off from 0 to 10 by 1/2 do
    (fomus-newnote 'pianopart
		   :off off
		   :voice '(1 2)
		   :dur (if (< off 10) 1/2 1)
		   :note (+ basepitch (random 25)))))
  (fomus-exec))



;; ----------------------------------------------------------------------------------------------------
;; quantizing
(progn					; CM
  (define (cm-example dur len part)
      (process
	repeat len
	output (new note
		 :partid part
		 :off (now)
		 :dur (between (/ dur 2) dur) ; <-- FOMUS deals with real or floating point numbers () or any 
		 :note (+ 48 (random 25)))    ;     numbers by finding the tuplets or combinations of tuplets that
	wait (between 1.0 (1+ dur))))         ;     allow the closest possible quantization
  (defparameter piano (new part :partid 'pianopart :name "Piano" :instr :piano))
  (events (cm-example 1.0 10 'pianopart) "output.ly" :parts (list piano)))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t))
  (fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (loop
   repeat 10
   for off = (random 1.0) then (+ off 1 (random 1.0)) do
   (fomus-newnote 'pianopart
		  :off off
		  :dur (+ 0.5 (random 0.5))
		  :note (+ 48 (random 25))))
  (fomus-exec))



;; ----------------------------------------------------------------------------------------------------
;; adjusting quantizing/notation
(progn					; CM
  (define (cm-example dur len part)
      (process
	repeat len
	output (new note
		 :partid part
		 :off (now)
		 :dur (between (/ dur 2) dur)
		 :note (+ 48 (random 25)))
	wait (between 1.0 (1+ dur))))
  (defparameter piano (new part :partid 'pianopart :name "Piano" :instr :piano))
  (events (cm-example 1.0 10 'pianopart) "output.ly" :parts (list piano)
	  :max-tuplet 3 :beat-division 4)) ; <-- use these two settings to adjust quantizing

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t))
  (fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (loop
   repeat 10
   for off = (random 1.0) then (+ off 1 (random 1.0)) do
   (fomus-newnote 'pianopart
		  :off off
		  :dur (+ 0.5 (random 0.5))
		  :note (+ 48 (random 25))))
  (fomus-exec :max-tuplet 3 :beat-division 4))



;; ----------------------------------------------------------------------------------------------------
;; parts and ensemble-type
(progn					; CM
  (define (cm-example dur len)
      (process
	repeat len
	output (new note
		 :partid (random 7)
		 :off (now)
		 :dur (between (/ dur 2) dur)
		 :note (+ 48 (random 25)))
	wait (between 1.0 (1+ dur))))
  (defparameter parts
    (loop
     for (name instr) in '(("Piano" :piano) ("Flute 1" :flute) ("Flute 2" :flute)
			   ("Violin 1" :violin) ("Violin 2" :violin) ("Tuba" :tuba)
			   ("Oboe" :oboe))
     and id from 0 
     collect (make-part :partid id :name name :instr instr)))
  (events (cm-example 1.0 10) "output.ly" :parts parts
	  :max-tuplet 3 :beat-division 4 :ensemble-type :small-ensemble))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t) :ensemble-type :small-ensemble) ; <-- default ensemble types are :small-ensemble and :orchestra
  (loop
   for (name instr) in '(("Piano" :piano) ("Flute 1" :flute) ("Flute 2" :flute)
			 ("Violin 1" :violin) ("Violin 2" :violin) ("Tuba" :tuba)
			 ("Oboe" :oboe))
   and id from 0
   do (fomus-newpart id :name name :instr instr))
  (loop
   repeat 10
   for off = (random 1.0) then (+ off 1 (random 1.0)) do
   (fomus-newnote (random 7)
		  :off off
		  :dur (+ 0.5 (random 0.5))
		  :note (+ 48 (random 25))))
  (fomus-exec :max-tuplet 3 :beat-division 4))



;; ----------------------------------------------------------------------------------------------------
;; time signatures
(progn					; CM
  (define (cm-example dur len part)
      (process 
	output (new note
		 :partid part
		 :off (now)
		 :dur (if (< (now) len) dur (* dur 2))
		 :note (+ 48 (random 25)))
	until (>= (now) len)
	wait dur))
  (defparameter timesigs ; <-- FOMUS will correct the 3/4 time signature at offset 0 to fit better with the one at offset 4
    (loop for off in '(0 4) collect (new timesig :off off :time '(3 4))))
  (defparameter piano (make-part :partid 'pianopart :name "Piano" :instr :piano))
  (events (cm-example 1/2 10 'pianopart) "output.ly" :parts (list piano) :global timesigs))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t))
  (fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (fomus-newtimesig :off 0 :time '(3 4))
  (fomus-newtimesig :off 4 :time '(3 4))
  (loop
   for off from 0 to 10 by 1/2 do
   (fomus-newnote 'pianopart
		  :off off
		  :dur (if (< off 10) 1/2 1)
		  :note (+ 48 (random 25))))
  (fomus-exec))



;; ----------------------------------------------------------------------------------------------------
;; articulations
(progn					; CM
  (define (cm-example dur len part)
      (process
	with mark = (new weighting :of '((nil :weight 2) ((:accent) :weight 1)))
	output (new note
		 :partid part
		 :off (now)
		 :dur (if (< (now) len) dur (* dur 2))
		 :note (+ 48 (random 25))
		 :marks (next mark)) ; <-- this must be a list of "marks" or articulations (see the doc for a list of them)
	until (>= (now) len)
	wait dur))
  (defparameter piano (make-part :partid 'pianopart :name "Piano" :instr :piano))
  (events (cm-example 1/2 10 'pianopart) "output.ly" :parts (list piano)))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t) :ensemble-type :orchestra)
  (fomus-newpart 'pianopart :name "Piano" :instr :piano)
  (loop
   for off from 0 to 10 by 1/2 do
   (fomus-newnote 'pianopart
		  :off off
		  :dur (if (< off 10) 1/2 1)
		  :note (+ 48 (random 25))
		  :marks (when (= (random 3) 0) '(:accent))))
  (fomus-exec))



;; ----------------------------------------------------------------------------------------------------
;; percussion instruments
; you can create a "percussion" meta-instrument that specifies one or more percussion instruments that are
; notated together on the same staff.
; instead of specifying a keyword like :piano for the instrument, a list is given here telling FOMUS
; to lookup its default :percussion instrument and modify it by specifying two custom percussion instruments.
; notes are then specified by keywords identifying the percussion instrument (:woodblock and :snaredrum).
; :autodur t  tells FOMUS to automatically tweak the durations so that they don't have ties (no durations are specified here).
(progn					; CM
  (define (cm-example dur len part)
      (process
	with inst = (new weighting :of '(:woodblock :snaredrum))
	and skip = (new weighting :of '(nil t))
	when (next skip)
	output (new note
		 :partid part
		 :off (now)
		 :note (next inst)) ; <-- just use :woodblock or :snaredrum for notes
	until (>= (now) len)
	wait dur))
  (defparameter percs (new part :partid 'percspart :name "Percussion"
			   :instr (list :percussion ; <-- a list beginning with :percussion means take FOMUS's default :percussion instrument and modify it
					:percs (list (fm:make-perc :woodblock :voice 1 :note 'e4 :autodur t) ; <-- specifies a percussion instrument as part of the larger instrument definition
						     (fm:make-perc :snaredrum :voice 2 :note 'a3 :autodur t))))) ; <-- you can set them all to different voices or the same voice
  (events (cm-example 1/2 40 'percspart) "output.ly" :parts (list percs)))

(progn					; Lisp
  (fomus-init :backend '(:lilypond :view t) :ensemble-type :orchestra)
  (fomus-newpart 'percpart
		 :name "Percussion"
		 :instr `(:percussion :percs (,(make-perc :woodblock :voice 1 :note 'e4 :autodur t)
					      ,(make-perc :snaredrum :voice 2 :note 'a3 :autodur t))))
  (loop for o from 0 to 40 by 1/2 when (= (random 2) 0) do
	(fomus-newnote 'percpart
		       :off o
		       :note (case (random 2)
			       (0 :woodblock)
			       (1 :snaredrum))))
  (fomus-exec))



;; ----------------------------------------------------------------------------------------------------
;; CM interface
;; harmonics
(defun harmonics (note rhy harm1 harm2 part)
  (process with fund = (hertz note) and beat = (rhythm rhy)
           for harm from harm1 to harm2
           for knum = (keynum (* fund harm) :hz)
           output (new note :off (now) :dur beat :note knum
                       :partid part)
           wait beat))



(defparameter primes '(2 3 5 7 11))

(defparameter cellos (loop for p in primes
                           collect (new part :partid p :instr :cello)))

(events (process for p in primes
                 sprout (harmonics 'c1 (/ 1 p) p (* p 4) p))
        "/tmp/primes.ly" 
        :quartertones t 
        :max-tuplet 11
        :parts (reverse cellos))


(defun harmonics-midi (note rhy harm1 harm2 part)
  (process with fund = (hertz note) and beat = (rhythm rhy)
           for harm from harm1 to harm2
           for knum = (keynum (* fund harm) :hz)
           output (new midi :time (now) :duration beat :keynum knum)
           wait beat))

#|
(events (process for p in primes
                 sprout (harmonics-midi 'c1 (/ 1 p) p (* p 4) p))
        (new incudine-stream :channel-tuning 4))

(new incudine-stream :channel-tuning 4)

(progn
  (defun simple ()
    (process repeat 5
             output (new osc
                      :time (now)
                      :path "osc/tost"
                      :types "iiii"
                      :message '(0 1 2 3))
             output (new midi
                      :time (now)
                      :duration 0.5
                      :keynum 60)
             wait 1))
  (let ((output (new incudine-stream)))
    (events (simple) output)))
|#

(new incudine-stream)


;; ----------------------------------------------------------------------------------------------------
;; some info functions
(fm:list-fomus-settings) ; <-- all the settings and their defaults
(fm:list-fomus-instruments) ; <-- lists all the default instruments

;; .fomus file
;; You can create a .fomus file in your home directory!  Fill it with pairs of setting names (as keywords) followed by their values.
;; These will then become your personal defaults.
:backend '(:lilypond :view t)
:max-tuplet 11
:auto-cautionary-accs t
