
(in-package :cm)

;; THIS FILE REQUIRES MSUTILS.LISP

(defparameter *ms* (midishare-open))

;; MS:OUTPUT is a low-level Midishare function that sends a MidiEv to
;; Midishare:  (ms:output ev to &optional ahead)

(ms:output (ms:new typeNote :port *dk* :chan 0
                   :pitch 60 :vel 90 :dur 1000)
           :to *ms*)
                   
;; TODO: Output a typeNote for keynum 30 to the JV:

(ms:output (ms:new typeNote :port *jv* :chan 0
                   :pitch 30 :vel 90 :dur 1000)
           :to *ms*)

;; TODO: write the note 2 seconds in the future

(ms:output (ms:new typeNote :port *dk* :chan 0
                   :pitch 30 :vel 90 :dur 1000)
           :to *ms* :at (+ 2000 (ms:midigettime)))

;;; TODO: Write a LOOP that plays a fast C-minor scale on the disklavier:

(loop for n in (keynum '(c4 d ef f g af bf c5))
   for j from (ms:midigettime) by 100
   do 
   (ms:output (ms:new typeNote :port *dk* :chan 0
                      :pitch n :vel 90 :dur 200)
              :to *ms* :at j))

;;
;; MIDI THRU hooks

;; You can implement an "interactive" compositions using MS:OUTPUT
;; inside a SET-RECEIVER! hook. The hook will "respond" to input by
;; sending output, possibly based on conditional tests.

;; TODO: Set JV in LOCAL OFF MODE

;; TODO: Write a thru hook that simply plays whatever key you press on
;; the JV. 

;; (defun jinho-note-on (ev)
;;   (ms:output ev *ms* 500))

(defun jinho-note-on (ev)
  (let ((ref (midishare-stream-refnum *ms*)))
    (ms:MidiSendAt ref ev (+ (ms:midigettime) 500))
    (ms:MidiFreeEv ev)))

;; (set-receiver! #'jinho-note-on *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'jinho-note-on)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))

;; (defun jinho-note-on2 (ev)
;;   (ms:port ev *dk*)
;;   (ms:output ev *ms* 500))

(defun jinho-note-on2 (ev)
  (let ((ref (midishare-stream-refnum *ms*)))
    (ms:port ev *dk*)
    (ms:MidiSendAt ref ev (+ (ms:midigettime) 500))
    (ms:MidiFreeEv ev)))

;; (set-receiver! #'jinho-note-on2 *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'jinho-note-on2)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))

;; (defun jinho-note-on3 (ev)
;;   (let ((jvev (ms:midicopyev ev)))
;;     (ms:port ev *dk*)
;;     (ms:output ev *ms* 500)
;;     (ms:output jvev *ms* 1000)))

(defun jinho-note-on3 (ev)
  (let ((ref (midishare-stream-refnum *ms*))
	(jvev (ms:MidiCopyEv ev)))
    (ms:port ev *dk*)
    (ms:MidiSendAt ref ev (+ (ms:midigettime) 500))
    (ms:MidiSendAt ref jvev (+ (ms:midigettime) 1000))
    (ms:MidiFreeEv ev)
    (ms:MidiFreeEv jvev)))

;; (set-receiver! #'jinho-note-on3 *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'jinho-note-on3)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))

;; TODO: Write a hook that will only play black keys.

;; (receiver? *ms*)

(defun keyEv? (ev)
  ;; true if type is 1 or 2 (typeKeyOn typeKeyOff)
  (< 0 (ms:evType ev) 3))

(defun blackKeyEv? (ev)
  ;; the "true" value is the black pitch class
  (and (keyEv? ev)
       (find (mod (ms:pitch ev) 12) '(1 3 6 8 10))))

(defun blackkeys (ev)
  (if (blackkeyev? ev)
      (ms:output ev :to *ms*)
      (ms:midiFreeEv ev)))

;; (set-receiver! #'blackkeys *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'blackkeys)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))

;; TODO: Write a hook that plays a parallel 5th above and below
;; whatever you play.

(defun parallel5 (ev)
  (let ((5up (ms:midiCopyEv ev))
        (5dn (ms:midiCopyEv ev)))
    (ms:pitch 5up (+ 7 (ms:pitch ev)))
    (ms:pitch 5dn (+ -7 (ms:pitch ev)))
    (ms:output ev :to *ms*)
    (ms:output 5up :to *ms*)
    (ms:output 5dn :to *ms*)
    (ms:midiFreeEv ev)
    (ms:midiFreeEv 5up)
    (ms:midiFreeEv 5dn)))

;; (set-receiver! #'parallel5 *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'parallel5)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))

;; TODO: write a harmonizer that plays a major 64 chord on the DK for
;; any keydown on the JV

(defun parallel5 (ev)
  (if (and (ke
  (let ((5up (ms:midiCopyEv ev))
        (5dn (ms:midiCopyEv ev)))
    (ms:pitch 5up (+ 7 (ms:pitch ev)))
    (ms:pitch 5dn (+ -7 (ms:pitch ev)))
    (ms:output ev *ms*)
    (ms:output 5up *ms*)
    (ms:output 5dn *ms*)))



;; TODO: CHange the harmonizer to accept keys from the JV but play
;; them on the diskavier plays a major 64 chord on the DK. The DK
;; notes should use the same velocity as the JV key actions.

;; TODO: Change the harmonizer to play the chord on the DK in a
;; staccato alberti bass figure. You will be sending typeNote events
;; to do this, so make sure that ony the true NOTEONs trigger the
;; Disklavier.

;; TODO: Change the harmonizer to play major chords on white keys
;; and minor chords on black keys.

;; TODO: Write a hook that t -- for any keydown from the disklaiver --
;; sprouts a simp2 process one octave below whose amplitude is the
;; same as the key your pressed. Dont forget to start rts before you
;; use thru5 !!

(defun simp2 (len lb ub rhy dur amp port chan)
  (process repeat len
           for k = (between lb ub k) ; dont reselect k
           ;; :date output values are ignored by MidiShare, so 
           ;; use "OUTPUT ... AT (now)" to tell CM what time
           ;; to send the C struct:
           ms:output (ms:new typeNote :pitch k :dur dur
                              :vel amp :port port  :chan chan)
	   :at (ms:midigettime) :to *ms*
           wait rhy))

(rts *ms*)

(sprout (simp2 10 40 45 .1 100 90 *dk* 0))

(rts?)

(defun keyDownEv? (ev)
  (and (= (ms:evType ev) 1)
       (> (ms:vel ev) 0)))

(defun dkwiggle (ev)
  (if (keydownev? ev)
      (sprout
       (simp2 (pick 5 6 8)
              (- (ms:pitch ev) 12)
              (- (ms:pitch ev) 5)
              .1
              100
              (ms:vel ev)
              *dk*
              0)))
  (ms:midiFreeEv ev))

;; (set-receiver! #'dkwiggle *ms*)
;; (remove-receiver! *ms*)

(ms:midishare-receive (midishare-stream-refnum *ms*) #'dkwiggle)
(ms:midishare-receive-stop (midishare-stream-refnum *ms*))
