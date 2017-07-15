(in-package :cm)

(defparameter *ms* (midishare-open))

;; MS:OUTPUT is a low-level Midishare function that sends a MidiEv to
;; Midishare:  (ms:output ev to &optional ahead)

(ms:output (ms:new typeNote :port *dk* :chan 0
                   :pitch 60 :vel 90 :dur 1000)
           :to *ms*)

;; TODO: Output a typeNote for keynum 30 to the JV:


;; ms:output has an optional third arg lets you schedule MidiEvs
;; into the future:

(ms:output (ms:new typeNote :port *dk* :chan 0
                   :pitch 60 :vel 90 :dur 1000)
           :to *ms* :at (+ (ms:midigettime) 2001))


;;; TODO: Write a LOOP that plays a fast C-minor scale on the disklavier:

;;
;; MIDI THRU hooks

;; You can implement an "interactive" compositions using MS:OUTPUT
;; inside a SET-RECEIVER! hook. The hook will "respond" to input by
;; sending output, possibly based on conditional tests.

;; TODO: Set JV in LOCAL OFF MODE

;; TODO: Write a thru hook that simply plays whatever key you press on
;; the JV. 

;; TODO: Set the hook, test it out, then remove the hook.

;; TODO: Write a hook that will only play black keys.

;; TODO: Write a hook that plays a parallel 5th above and below
;; whatever you play.

;; TODO: write a harmonizer that plays a major 64 chord on the DK for
;; any keydown on the JV


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
