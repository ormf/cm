(in-package :cm)

;; Define MidiShare ports in 5065 (Micro Express)

(defparameter *qt* 0)  ; quicktime
(defparameter *jv* 2)  ; jv-35
(defparameter *dk* 4)  ; disklavier

;; open MidiShare connection and save the stream object in the
;; variable *ms* so we can use it later.

(defparameter *ms* (midishare-open))  

;; note that the value of *ms* is an actual stream object:

*ms*

;; define a test process 

(defun simp (len lb ub rhy dur amp chan)
  (process repeat len
           for k = (between lb ub k) ; dont reselect k
           output (new midi :time (now)
                       :keynum k
                       :duration dur
                       :amplitude amp
                       :channel chan)
           wait rhy))

;; By default, CM maps 127 channels to 8 ports like this:
;; chan 0 to 15 = port 0       [quicktime]
;; chan 16 to 31 = port 1
;; chan 32 to 47 = port 2      [jv-35 in room 5065]
;; ...
;; chan 64 to 89 = port 4      [disklavier in room 5065]
;; ...
;; chan 116 to 127 = port 7

(events (simp 20 60 80 .2 .15 .7 0) "midi-port.ms")

;; To do real time work we need to use real stream objects rather than
;; their string names. So start using the value of *ms* rather than
;; its name "midi-port.ms"

(events (list (simp 50 60 90 .25 .2 .5 0)
              (simp 25 20 50 .5  .2 .5 0))
        *ms*)

;; TASK: send the output of simp to the Disklavier:

(events (list (simp 50 60 90 .25 .2 .5 64)
              (simp 25 20 50 .5  .2 .5 64))
        *ms*)

;; Real Time Scheduling

;; Real time scheduling means that things sound right when they
;; happen. This is NOT the case with the events function, which runs
;; many times FASTER than real time and then plays the result
;; afterwards.  Despite their fundamental differences, you can use RTS
;; almost "interchangeably" with events.  For example, lets pass the
;; rts function processes and the output stream...

(events (list (simp 50 60 90 .25 .2 .5 0)
              (simp 25 20 50 .5  .2 .5 0))
        *ms*)

;; but now lets add stuff "interactively", WHILE SIMPs are playing.
;; first start up rts like before but for twice as long...

(rts (list (simp 100 60 90 .25 .2 .5 0)
           (simp 50  20 50 .5  .2 .5 0))
     *ms*)

;; ...NOW EVAL THIS while rts is playing:

(let* ((k (between 20 100))
       (n (pick 3 5 7 11))
       (r (/ 1 n))
       (d (* r .9)))
  (sprout (simp (* n (pick 2 3 4))
                k
                (+ k 7)
                r
                d
                .75
                0)))

;; you can also start rts without any objects at all, in which case it
;; will run (silently) until you add stuff. To schedule events in the
;; "future" or run algorithms, start rts first and then use sprout to
;; add material to the scheduler.

(rts nil *ms*)

;; ask if rts is running

(rts? )

;; add stuff whenever you want (as long as rts is running).

(let* ((k (between 20 100))
       (n (pick 3 5 7 11))
       (r (/ 1 n))
       (d (* r .9)))
  (sprout (simp (* n (pick 2 3 4))
                k
                (+ k 7)
                r
                d
                .75
                0)))

;; stop rts

(rts-stop)

(rts? )

;; LISP DETOUR: what is the difference between LET and LET*? Why did
;; we use let* in the example above 

;; TIP: To look up Lisp functions (not CM functions) inside Emacs, put
;; the cursor on the lisp symbol (function or variable) and type:
;;
;;    M-X hyperspec-lookup       (shortcut:   M-x hy<TAB>)
;;
;; TASK: look up these symbols in the hyperspec:
;;    pi   let*    cond



;;
;; Low-level MidiShare Routines
;;
;; When you work in real-time with Midishare its best to work directly
;; with "low-level" Midishare functions. These functions are all
;; prefixed by "ms:", for example:  ms:new

;; ms:new is similar to "new" but it creates MidiShare foreign
;; objects, called MidiEvs. The most important MidiEv is called the
;; typeNote MidiEv. These have the the same basic functionality as
;; CM's midi objects:

;; midi inits       :time :keynum :duration :amplitude :channel
;; typeNote inits   :date :pitch  :dur      :vel       :port :chan


;; The most important differences between normal objects and midiEvs
;; are:
;;   1 MidiEvs are C structs, NOT lisp objects.
;;   2 Lisp does not memory manage (garbage collect) MidiEvs, you do.
;;   3 All values for midiEvs are integer. for example  :dur
;;     is in milliseconds and :vel (MIDI velocity) is 0-127
;;   4 Midishare ignores the :date value in midiEv output.

;; create a typeNote object

(defparameter myEv (ms:new typeNote :pitch 60 :dur 200 :vel 64 :port 0
                           :chan 1 :date 1001))
myEv

;; You cann use ms:prints MidiPrintEV to print any MidiEv

(ms:MidiPrintEv myEv)

;; each init has a Lisp accessor function:

(ms:pitch myEv)
(ms:dur myEv)
(ms:vel myEv)
(ms:port myEv)
(ms:chan myEv)
(ms:date myEv)
(ms:evType myEv) ; returns the "type" of midiEv

;; the optional second arg to every accessor SETS value in midiEv:

(ms:pitch myEv 90)
(ms:vel myEv 90)
(ms:dur myEv 2000)

(ms:MidiPrintEv myEv)

;; there different types of midiEvs. the most common besides typeNote
;; are typeKeyOn and typeKeyOff. Note that these "type names" are
;; really just variables holding an integer type number:

typeNote

typeKeyOn

(ms:midiPrintEv (ms:new typeKeyOn :chan 2 :pitch 50))

;; If you are not scheduling into the future or running algorithms you
;; can use ms:output to sennd individual MidiEv objects directly to
;; MidiShare at the current time. Note that Midishare automatically
;; DEALLOCATES the objects that you send to it, so perform ONLY ONE
;; ms:output per ms:new call.

(ms:output myEv *ms*) ;; only output myEv once or you will lose.

;; Lets create function SIMP2 that outputs MS:NEW typeNote rather than
;; MIDI objects. Remember that noteType uses millisecond but cm's
;; scheduler runs in seconds, ie the  (now) function

(defun simp2 (len lb ub rhy dur amp port chan)
  (process repeat len
           for k = (between lb ub k) ; dont reselect k
           ;; :date output values are ignored by MidiShare, so 
           ;; use "OUTPUT ... AT (now)" to tell CM what time
           ;; to send the C struct:
           output (ms:new typeNote :pitch k :dur dur
                       :vel amp
                       :port port
                       :chan chan)
              at (now)
           wait rhy))

(rts nil *ms*)

(rts?)

(sprout (simp2 10 60 80 .2 150 80 0 0))

(rts-stop)

;; TASK: create SIMP3. just like SIMP2 except dur will be seconds and
;; amp will be 0-1, so
;; TRY:
;;   1 write the expression that converts .5 to MIDI velocity:
;; ANSWER: (* .5 127) is the conversion but you also need to use FLOOR
;; to convert the value to an integer for midiEvs

(floor (* .5 127))

;;   2 Rewrite using CM's rescale function:
;; ANSWER:

(floor (rescale .5 0 1 1 127))

;;   3 Convert numbers in seconds to milliseconds:
;; ANSWER:

(floor (* .5 1000))


;; Remember that all MidiShare values have to be integer so you will 
;; need to use FLOOR to force integer values!

;; Now define two functions amp->vel and sec->milli that do both jobs
;; for you.
;; TASK: here is the "shell" of amp->vel, you finish it:
(defun amp->vel (amp)
  ))

;; ANSWER:

(defun amp->vel (amp)
  (floor (* amp 127)))

;; If you dont like the fact that FLOOR returns two values, use the 
;; values functon to forcee only one value to be returned.

(defun amp->vel (amp)
  (values (floor (* amp 127))))

(amp->vel .6)

;; TASK: define sec->milli:

;; ANSWER:

(defun sec->milli (sec)
  (values (floor (* sec 1000))))

(sec->milli 1)
(sec->milli .1)

;; TASK: define simp3 that uses amp->vel and sec->milli

(defun simp3 (len lb ub rhy dur amp port chan)
  (process repeat len
           for k = (between lb ub k) ; dont reselect k
           output (ms:new typeNote 
                    :pitch k
                    :dur (sec->milli dur)
                    :vel (amp->vel amp)
                    :port port
                    :chan chan)
              at (now)
           wait rhy))

;; TASK: use simp3 with rts. be sure to stop rts too.
;; ANSWER:

(rts nil *ms*)
(sprout (simp3 (pick 10 15 20)
               40
               80
               .2
               .1
               .8
               *dk*
               0))
(rts-stop)

;;
;; Receiving
;;
;; Receiving means getting data "interactively". Some streams in CM
;; (MidiShare, SuperCollider, OSC, Portmidi) allow a receiving "hook"
;; to be set. A hook is a function that is automatically called
;; whenever there is input available. Mishare hooks are always given a
;; single value: the MidiEv that MidiShare received.

;; This hook will print whaver we receive and then free (reclaim) the
;; object. Always reclaim Events that you receiive but dont send OUT
;; again. Use midiFreeEv to "garbage collect" MidiShare events:

(defun my-hook (ev)
  (ms:midiPrintEv ev)
  (ms:midiFreeEv ev))

;; use set-receiver! to set a receiving hook in a stream. when you are
;; done use remove-receiver! to remove a hook from a stream. Important:
;; when you set a hook you need to pass the actual FUNCTION not its NAME.

;; LISP DETOUR: Recall from last semeseter that #' returns a function
;; given its name, or you can use the special form FUNCTION to do
;; this:

'+           ; the name (symbol) +
#'+          ; the fuction named +
(function +) ; the function named + (same as #')

;; so lets set my-hook as the receiver for *ms*

(set-receiver! #'my-hook *ms*)

;; ...and ask if *ms* is currently receiving...

(receiver? *ms*)

;; ...now play the JV...

;; ...now play the DK...

;; ...then remove receiver to stop

(remove-receiver! *ms*)  

(receiver? *ms*)

;; Note that when you played the JV the NoteOn and NoteOff messages
;; looked like:
;;   #<MidiEv KeyOn [2/0 1678481ms] 62 72>
;;   #<MidiEv KeyOff [2/0 1678592ms]>
;; And when you played the Disklavier the pair looked like:
;;  #<MidiEv KeyOn [4/0 1735765ms] 62 93>
;;  #<MidiEv KeyOn [4/0 1738994ms] 62 0>

;; that is, the DK sends a NoteOn with zero veolcity to mean a
;; noteOff.  This is not a bug, its legal the MIDI protocol and your
;; programs will have to be careful to account for this in its tests.
;; testing is important when you only what to deal with messages
;; that are "true" for certain conditons. For example this code
;; attempts to only print the end point of messages:

(defun print-note-offs (ev)
  (when (= (ms:evType ev) typeKeyOff)
    (ms:midiPrintEv ev))
  (ms:midiFreeEv ev))

(set-receiver! #'print-note-offs *ms*)

(remove-receiver! *ms*)

;; But of course, it doestn work for the Disklavier.
;; TASK: Fix it! Allows a NoteOn with velocity=0 to mean a NoteOff (typeKeyOff)
;; rewrite print-note-off to also print in such a case

;; ANSWER: Here are three ways
;; 1. this is the lispy way:

(defun print-note-offs1 (ev)
  (when (or (= (ms:evType ev) typeKeyOff)
            (and (= (ms:evType ev) typeKeyOn)
                 (= (ms:vel ev) 0)))
    (ms:midiPrintEv ev))
  (ms:midiFreeEv ev))

;; 2 using If statements is also fine:

(defun print-note-offs2 (ev)
  (if (= (ms:evType ev) typeKeyOff)
      (ms:midiPrintEv ev)
      (if (= (ms:evType ev) typeKeyOn)
          (if (= (ms:vel ev) 0)
              (ms:midiPrintEv ev))))
  (ms:midiFreeEv ev))

;; 3. Using the COND statement (see later for more info)

(defun print-note-offs3 (ev)
  (cond ((= (ms:evType ev) typeKeyOff
         (ms:midiPrintEv ev)))
        ((and (= (ms:evType ev) typeKeyOnn)
              (= (ms:vel ev) 0))
         (ms:midiPrintev ev)))
  (ms:midiFreeEv ev))

;; here is a receiver that only prints on or off events
;; if they comme fromm a particular port (the JV port in this case)

(defun thru (ev)
  (if (= (ms:port ev) *jv*)
      (if (or (= (ms:evType ev) typeKeyOn)
              (= (ms:evType ev) typeKeyOff))
          (progn (ms:midiPrintEv ev)  ; progn is like begin
                 (ms:midiFreeEv ev))
          (ms:midiFreeEv ev))
      (ms:midiFreeEv ev)))

(set-receiver! #'thru *ms*)
(remove-receiver! *ms*)

;; LISP DETOUR: (progn ...) is like (begin ...) in scheme. Why is it
;; being used?



;; MIDI THRU HOOKS.
;; The simplist type of "interactive" receiver passes incoming
;; events to output destinations.

(defun thru (ev)
  (output ev *ms*))

;; MIDI DETOUR: Before you try thru on the JV you must first set "Local
;; Control OFF" on the JV, otherwise notes will always sound when you
;; press a key no matter what.

;; 1. Simulateously press the [Master][Midi] keys in upper right on the synth.
;; 2. Press [Parameter UP] (upper left side) until the display shows:
;;        MIDI Function
;;        Local         ON
;; 3. Press [Value DOWN] to turn ON to OFF.
;; 4. Simulateously press the [Master][Midi] keys again to stop editing.

;; now that local control is OFF, try out

(set-receiver! #'thru *ms*)

;; ... play the jv...
;; .. then clear the hook

(remove-receiver! *ms*)

;; Of course, a hook like that is pretty useless -- we want to set hooks that output things only in the case that certain conditions are true.

;; For exammple, we if we only want to allow EVEN numberded keys to be heard?
;; TASK: Write thru2 that only passes ons and offs whose keynumbers
;; are even numbered.

;; ANSWER: Here are three answers, the first works but is not correct:

(defun even-thru (ev)
  (if (evenp (ms:pitch ev))
      (ms:output ev)
      (ms:midiFreeEv ev)))

;; THis is better:

(defun even-thru (ev)
  (if (or (= (ms:type ev) typeKeyOn)
          (= (ms:type ev) typeKeyOff))
      (if (evenp (ms:pitch ev))
          (ms:output ev)
          (ms:midiFreeEv ev))
      (ms:midiFreeEv ev)))

;; This is Best: WHY??
          
(defun even-thru (ev)
  (if (and (or (= (ms:type ev) typeKeyOn)
               (= (ms:type ev) typeKeyOff))
           (evenp (ms:pitch ev)))
      (ms:output ev)
      (ms:midiFreeEv ev)))

;; TASK: Set the hook and try it.
;; ANSWER:

(set-receiver! #'even-thru *ms*)

(remove-receiver! *ms*)


;; LISP DETOUR: Predicate functions are functions that return true (or
;; false) based on testing their arguments.  COmmonLisp, unlike
;; Scheme, tends to name predicates with a "p" or "-p" at the end (the
;; p stands for predicate)

(evenp 40)    ; scheme's even? 
(evenp 41)
(symbolp 'hiho!)   ; scheme's symbol?
(integerp 20.1)    ; scheme's integer?
(floatp pi)

;; math relations are predicates too!

(> 41 5)
(< 50 100 1)
(= 1 (random 3))
(odds .3)  ; a cm function...

;; so are logical operators....here is a refresher
;; AND is a predicate that returns true if ALL args are true

(and)
(and t)
(and nil)
(and t t t t t)
(and t nil t t t t t)

;; And is interesting in that it doesnt evaluate anything past the
;; first false arg:

(setq foo 1)
(and nil (setq foo 2))
foo

;; That mean these are equivalent:

(if (= (random 2) 1)
    (setq foo 22))

(and (= (random 2) 1)
     (setq foo 22))

;; OR is a predicate that returns true if ANY args are true

(or)
(or t)
(or nil t)
(or nil nil nil nil t)
(or t nil nil nil nil t)

;; similar to and, OR only evaluate until the first T arg:

(setq foo 1)
(or t nil nil nil (setq foo 33))
foo

;; NOT return the negation or its arg. remember that ANYTHING that is
;; not NIL is true!!

(not t)
(not nil)
(not 1)
(not (not t))

;; IF does condition eval

(if t 0)
(if t 0 1)
(if nil 0)
(if nil 0 1)
(if (= (random 3) 3) 'winner 'loser)

;; WHENf and UNLESS are suger coating for IF that aloow multiple forms
;; in theiir body

(when (= (random 3) 1)
  (print 'winner!)
  (setf foo 99))

;; TASK: rewrite when as an IF (remember that only 1 expr is allowed as
;; then or else form)
;; ANSWER:
(if (= (random 3) 1)
    (progn (print 'winner!)
           (setf foo 99))
    )

(unless (= (random 3) 1)
  (print 'loser!)
  (setf foo -99))

;; TASK: rewrite UNLESS as an IF (remember that only 1 expr is allowed as
;; then or else form)
;; ANSWER:
(if (= (random 3) 1)
    nil
    (progn (print 'loser!)
           (setf foo -99)))


;; Back to predicates: Predicates are very useful in hooks, they allow
;; the hook to test incoming events and do things only in the case
;; that the predicate conditions hold true (or false).  So our
;; "interative predicate" will be testing various things about the
;; MidiEv we have just received. 
;; Recall that the accessors for midiev values are:
;;    ms:pitch ms:date ms:vel ms:dur 
;; and that the "type" of the event is accessed by
;;   ms:evType  
;; and that the most common RECEIVED MidiEvs are:
;;   typeKeyOn typeKeyOff  (typeNote are NEVER received, WHY?)
;;
;; TASK: write a predicate function (keyupEv? ev) that returns true if
;; ev is any sort of note ending, ie a note off or a note on with 0
;; velocity.

;; ANSWER: 

(defun keyupEv? (ev)
  ;; return true if ev is a noteOff 
  ;; OR if its a noteOn with 0 velocity
  (or (= (ms:evType ev) typeKeyOff)
      (and (= (ms:evType ev) typeKeyOn)
           (= (ms:vel ev) 0))))

;; TASK: write a predicate function (keyEv? ev) that returns true
;; if ev is a note on or a note off

;; ANSWER: 

(defun keyEv? (ev)
  (or (= (ms:evType ev) typeKeyOn)
      (= (ms:evType ev) typeKeyOff)))

;; TASK: write a predicate function (keydownEv? ev) that returns
;; tru if ev is a true key down

;; ANSWER:

(defun keydownEv? (ev)
  (and (= (ms:evType ev) typeKeyOn)
       (> (ms:vel ev) 0)))

;; TASK: write a predicate functino (evenkeyEv? ev) that returns true if
;; ev has an even key number

;; ANSWER:

(defun evenkeyEv? (ev)
  (and (keyev? ev)
       (evenp (ms:pitch ev))))

;; TASK: write a predicate function (whiteEv? ev) that returns true if
;; ev is a white key

;; ANWER: Here are two ways. The first way works but is absurd

(defun whiteEv? (ev)
  (and (keyEv? ev)
       (or (= (mod (ms:pitch ev) 12) 0)
           (= (mod (ms:pitch ev) 12) 2)
           (= (mod (ms:pitch ev) 12) 4)
           (= (mod (ms:pitch ev) 12) 5)
           (= (mod (ms:pitch ev) 12) 7)
           (= (mod (ms:pitch ev) 12) 9)
           (= (mod (ms:pitch ev) 12) 11))))

(defun whiteEv? (ev)
  (if (keyEv? ev)
      (let ((pc (mod (ms:pitch ev) 12)))
        (find pc '(0 2 4 5 7 9 11)))
      nil))

;; COMMON LISP DETOUR: The FIND function returns true if a value is
;; in list of possible values.

(find 1 '(0 1 2))
(find 'rick '(peter billy sue))
(find #\c  "abc")
(find 3 '((0 c4 d4 ef4) (1 loser) (2 winner) (3 fs4))
      :key #'first)

;; MIDI THRU  Hooks.

;; TASK: Write a thru hook that only plays

;; ANSWER: 

(defun thru2 (ev)
  (if (and (keyEv? ev)
           (evenp (ms:pitch ev)))  ; evenp is like even?
      (ms:output ev)
      (ms:midiFreeEv ev)))

;; TASK: Write thru3 that plays all its notes transposed down 1 octave.

;; ANSWER:

(defun thru3 (ev)
  (if (keyEv? ev)
      (progn (ms:pitch ev (- (ms:pitch ev) 12))
             (ms:output ev))
      (ms:midiFreeEv ev)))

;; TASK: Write thru4 that passes ons and offs FROM the Roland TO
;; the disklavier

;; ANSWER: 2 answers are given. both work buyt the second is better WHY?

(defun thru4 (ev)
  (if (keyEv? ev)
      (if (= (ms:port ev) *jv*)
          (progn (ms:port ev *dk*)
                 (ms:output ev))
          (ms:midiFreeEv ev))
      (ms:midiFreeEv ev)))

(defun thru4 (ev)
  (if (and (keyEv? ev)
           (= (ms:port ev) *jv*))
      (ms:port ev *dk*)
      (ms:midiFreeEv ev)))

;; TASK: write thru5 that -- for any keydown from the disklaiver --
;; sprouts a simp2 process one octave below whose amplitude is the
;; same as the key your pressed. Dont forget to start rts before you
;; use thru5 !!

(defun wiggle (ev)
  (if (keydownEv? ev)
      (let ((p (ms:pitch ev)))
        (sprout (simp (between 4 12)
                      (- p 12)
                      (- p 5)
                      .25
                      .2
                      (rescale (ms:vel ev) 0 126 0.0 1.0)
                      64))))
  (ms:midiFreeEv ev))

(set-receiver! #'wiggle *ms*)
;; ...play dislavier..
(remove-receiver *ms*)


;; LISP DETOUR:
;; If a hook needs to test for lots of differnt types of Events a Lisp
;; COND statement is useful. A cond statement consists of zero or more
;; clauses, each cluse is a list whoes first elents is a test, the
;; rest of the forms in the clauuse are evaluted if test is truwe.
;;
;; (cond (<test1> ...)
;;       (<test2> ...)
;;       (<test3> ...))

;; The first clause whose <test> expression evaluates to true is will
;; be evaluated, the last value in that clause will be returned as the
;; value of the cond statement. For example, eval the following cond a
;; number of times to see what kind of loser you are:

(let ((r (random 100)))  ; pick a number...
  (cond ((< r 25) 
         (list 'small-time-loser r))
        ((< r 50)
         (list 'regular-loser r))
        ((< r 75)
         (list 'big-time-loser r))
        (t    ;explain why this is an "else" clause
         (list 'winner r))))

