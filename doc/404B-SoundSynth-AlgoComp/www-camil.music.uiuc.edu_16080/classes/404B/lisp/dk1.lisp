(in-package :cm)

;;; Etude 1: Write a counterpoint receiver that plays back a
;;; transposed version of whatever the performer plays on the
;;; disklavier at a specified time interval in the future. (Risset)

;;; Etude 2: Change the receiver to play a "warped" (rescaled) version
;;; of the pianists input (Risset)

;;; Etude 3: Change the receiver to treat the amplitde of the incoming
;;; notes as a scaler on the time delay: the louder the notes the
;;; closer the repition interval; the softer the notes the farter the
;;; repetition interval

;;; Etude 4: Use the rescale function to also "invert" the melody
;;; played by the performer (Risset)

;;; Etude 5: Write an inverting receiver that "mirrors" whatever the
;;; performed playse around some specfiied keynumber

;;; Etude 6: Change the receiver to also accept input from the JV.  JV
;;; keydowns will set the inversion point for the dk processing.


;;; Working with DK Pedal information. Pedal data is encoded as MIDI
;;; Controller messages. For info about MIDI Controllers see:
;;; http://www.borg.com/~jglatt/tech/midispec/ctllist.htm

(defparameter *rp* 64)  ; right pedal: 0 to 127 (if DK in half-pedal mode)
(defparameter *mp* 66)  ; middle pedal: 0 or 127
(defparameter *lp* 67)  ; left pedal: 0 TO 127

;;; Use the  typeControl MidiEV for control information

(ms:new typeCtrlChange :controller :change v
        :port *dk* :channel 2) ; DK Must be in half pedal mode!!

;;; Etude: 7: Write a receive hook that prints out only Pedal information
;;; sent from the disklavier.


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

