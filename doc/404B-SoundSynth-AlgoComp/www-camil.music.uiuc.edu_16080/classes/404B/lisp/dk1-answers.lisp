(in-package :cm)

;;; Etude 1: Write a counterpoint receiver that plays back a
;;; transposed version of whatever the performer plays on the
;;; disklavier at a specified time interval in the future. (Risset)

; (load "#P"/private/Network/Servers/camilx2.music.uiuc.edu/Users/Faculty/hkt/404b/msutils.lisp")

(setq *ms* (midishare-open))

(ms:output (ms:new typeNote :pitch 60 :vel 64 :dur 1000 :port *dk*
                   :chan 0)
           *ms*)

(ms:output (ms:new typeNote :pitch 60 :vel 64 :dur 1000 :port *dk*
                   :chan 0)
           *ms*
           2000)

(defun etude1 (ev)
  (ms:pitch ev (+ (ms:pitch ev) 12))
  (ms:output ev *ms*))

(set-receiver! #'etude1 *ms*)
(remove-receiver! *ms*)

(defparameter *transp* 12)
(defparameter *future* 1000)

(defun etude1 (ev)
  (ms:pitch ev (+ (ms:pitch ev) *transp*))
  (ms:output ev *ms* *future*))

(set-receiver! #'etude1 *ms*)
(setq *transp* 13)
(setq *future* 100)
(remove-receiver! *ms*)

;;; Etude 2: Change the receiver to play a "warped" (rescaled) version
;;; of the pianists input (Risset)

(defun etude2 (ev)
  (ms:pitch ev (int (rescale (ms:pitch ev)
                        60 71 72 94)))
  (ms:output ev *ms* *future*))
(setq *future* 500)
(set-receiver! #'etude2 *ms*)
(remove-receiver! *ms*)

;;; Etude 3: Change the receiver to treat the amplitde of the incoming
;;; notes as a scaler on the time delay: the louder the notes the
;;; closer the repition interval; the softer the notes the farter the
;;; repetition interval

(defun etude3 (ev)
  (setq *future* (int (rescale (ms:vel ev)
                               0 127
                               2000 00)))
  (ms:output ev *ms* *future*))
  
(set-receiver! #'etude3 *ms*)
(remove-receiver! *ms*)
  
;;; Etude 4: Use the rescale function to also "invert" the melody
;;; played by the performer 

;;; Etude 5: Write an inverting receiver that "mirrors" whatever the
;;; performed playse around some specfiied keynumber (Risset)

(setq *ms* (midishare-open))
(defparameter *mk* 60) ; kenym around which i will invert
(defun mirror (ev)
  (cond ((> (ms:pitch ev) *mk*)
         (let* (
                (delta (- (ms:pitch ev) *mk*))
                (newkeynum (- *mk* delta))
               )
           ;; update ev with new keynum
           (ms:pitch ev newkeynum)
           (ms:output ev *ms*)))
        (t
         (ms:midiFreeEv ev)))
  )

(set-receiver! #'mirror *ms*)
(remove-receiver! *ms*)

(defparameter *tk* 108)
(defparameter *bk* 21)

(defun mirror2 (ev)
  (cond ((> (ms:pitch ev) *mk*)
         (let* (
                (delta (- (ms:pitch ev) *mk*))
                (newkeynum (- *mk* delta))
               )
           ;; update ev with new keynum
           (ms:pitch ev newkeynum)
           (ms:output ev *ms*)))
        ((= (ms:pitch ev) *mk*)
         (ms:pitch ev *tk*)
         (let ((newev (ms:midiCopyEv ev)))
           (ms:pitch newev *bk*)
           (ms:output newev *ms* )
           (ms:output ev *ms* ) ))
        (t
         (ms:midiFreeEv ev)))
  )

(set-receiver! #'mirror2 *ms*)
(remove-receiver! *ms*)

(defun dk-mirror (ev)
  (cond ((> (ms:pitch ev) *mk*)
         (let* (
                (delta (- (ms:pitch ev) *mk*))
                (newkeynum (- *mk* delta))
                )
           ;; update ev with new keynum
           (ms:pitch ev newkeynum)
           (ms:output ev *ms*)))
        ((= (ms:pitch ev) *mk*)
         (ms:pitch ev *tk*)
         (let ((newev (ms:midiCopyEv ev)))
           (ms:pitch newev *bk*)
           (ms:output newev *ms* )
           (ms:output ev *ms* ) ))
        (t
         (ms:midiFreeEv ev))))

(defun mirror3 (ev)
  (cond ((and (= (ms:port ev) *dk*)
              (keyEv? ev))
         (dk-mirror ev))
        ((= (ms:port ev) *jv*)
         (setq *mk* (ms:pitch ev))
         (ms:midiFreeEv ev)
         )))

(set-receiver! #'mirror3 *ms*)
(remove-receiver! *ms*)


;;; Etude 6: Change the receiver to also accept input from the JV.  JV
;;; keydowns will set the inversion point for the dk processing.


;;; Working with DK Pedal information. Pedal data is encoded as MIDI
;;; Controller messages. For info about MIDI Controllers see:
;;; http://www.borg.com/~jglatt/tech/midispec/ctllist.htm

(defparameter *rp* 64)  ; right pedal: 0 to 127 (if DK in half-pedal mode)
(defparameter *mp* 66)  ; middle pedal: 0 or 127
(defparameter *lp* 67)  ; left pedal: 0 TO 127

;right (down and up)
;#<MidiEv CtrlChange [4/0 3728950ms] 64 127>
;#<MidiEv CtrlChange [4/0 3729250ms] 64 0>
;middle
;#<MidiEv CtrlChange [4/0 3733624ms] 66 127>
;#<MidiEv CtrlChange [4/0 3733932ms] 66 0>
;left
;#<MidiEv CtrlChange [4/0 3736466ms] 67 127>
;#<MidiEv CtrlChange [4/0 3736753ms] 67 0>? 

;;; Use the  typeControl MidiEV for control information

(ms:output
 (ms:new typeCtrlChange :controller *rp* :change 0
         :port *dk* :chan 2)
 *ms*)

;;; Etude: 7: Write a receive hook that prints out only Pedal information
;;; sent from the disklavier.

(defun prinev (ev)
  (ms:midiPrintEv ev)
  (ms:midiFreeEv ev))

(set-receiver! #'prinev *ms*)
(remove-receiver! *ms*)

;;; Disklavier MIDI Input Settings
;;; There are two important MIDI settings on the Disklavier that
;;; affect real time performance:
;;; 1. MIDI IN needs to be in HP (Half Pedal) mode to send/receive
;;; continuous Pedal information. Otherwise sustain values 0-63=are O
;;; (off) and 64-127 are 1 (on)
;;  2. MIDI IN is either in REALTIME mode or DELAY mode (500ms).

;;; DK must be in "half-pedal mode" (HP) to receive continuous
;;; controller information for left and right pedals, otherwise values
;;; are "binary", ie 0 OR 127.  When the Disklavier is in Half pedal
;;; mode the pedal information is sent/received on channel 2.

;;; When MIDI IN is in REALTIME mode, then the keydown delay is
;;; (bascially) the same as that discussed in the Risset article. When
;;; DK is in DELAY mode then an outomatic 500ms delay is added. This
;;; kind of "built in" delay is called LATENCY and is a important
;;; basic concept in realtime audio/midi work.  Every device and/or
;;; software has latency, it it only a question of degree. The smaller
;;; the latency the more reponsize the "realtime" system is.  500ms
;;; latency is HUGE, more typical latency values are 1-50ms.

;;; To put the DK in either HP (half pedal) or REALTIME/DELAY modes:
;;; 1 [Press FUNC/EDIT] to enter Editing screen.
;;; 2 [Press ENTER]
;;; 3 [Press ENTER] to select MIDI IN. The page shows:
;;;     MIDI IN CH=HP      [Use dial to select 1-16 or HP]
;;;     MIDI IN=REALTIME   [Press -> to select, use dial to switch
;;;                         between REALTIME/DELAY modes.
;;; 4 [Press FUNC/EDIT] to quit
;;;
;;; For more information see Chapter 15 of Manual (pg 100)

;;; Etude 8: Write a function called dkped that will create any of the
;;; three pedal control values based on a symbolx pedal value passed
;;; as the 1st argument. The second argument will be the pedal value
;;; (0-127). HINT: Use CASE or COND to distinguish the three pedals
;;; The symbokc names to allow for each pedal are:
;;;  right pedal:  64 :rp :right :damper :sustain
;;;  middle pedal: 66 :mp :middle :sostenuto
;;;  middle pedal: 68 :lp :left :una-corde :soft 

(defun dkped (ped val)
  )

