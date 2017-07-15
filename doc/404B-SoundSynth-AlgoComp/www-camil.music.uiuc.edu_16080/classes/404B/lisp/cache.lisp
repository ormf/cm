(in-package :cm)

;;; this file implements a very simple facility for assocating
;;; infomatino with keynumebers. It can be used, for example, to
;;; "remember" what keys are down on the DK. information about keys is
;;; held in an ARRAY, which is a more efficient (faster) way to
;;; organize informaton than using LISTS.  an array is simply an
;;; indexed block of memory that can hold user specified value at each
;;; index.

;;; Use MAKE-ARRAY to create an array:

(defparameter myarray (make-array 10))

;;; by default arrays are initialized to NIL at each index.

;;; use ELT to get the value at an index:

; (elt myarray 0)

;;; use SETF with ELT to set the value at an index:

; (setf (elt myarray 0) 123)
; (elt myarray 0)
; (setf (elt myarray 1) (random 10000000))
; (elt myarray 1)

;;; use LENGTH to find out the length of an array:

; (length myarray)

;; now we create an array in which to "cache" (hold) information we
;; want to associate with keynumbers.  since the "indexes" into the
;; array will be MIDI keynumbers we need a total of 128 elements in
;; our array:

(defparameter *keycache* (make-array 128))

(defun clear-keycache ()
  ;; clear all locations in array by setting them to nil
  (loop for i below (length *keycache*)
        do (setf (elt *keycache* i) nil)))

(defun keycache (knum)
  ;; return what is in the cache at knum index
  (elt *keycache* knum))

(defun keycache! (knum val)
  ;; set cache for knum to val
  (setf (elt *keycache* knum) val) )

;;; Now that we have a cache we can use it to "remember" various
;;; things about the state of the disklavier. For example we can
;;; remember which keys are down by "claiming" them on KeyDowns
;;; and then "freeing" them on keyUps

(defun key-claim! (knum)
  ;; remember keynum as down
  (keycache! knum t))

(defun key-free! (knum)
  ;; clear key down
  (keycache! knum nil))

(defun key-free? (knum)
  ;; return true if key is not claimed
  (not (keycache knum)))

;;; a hook to test it.

(defun test-hook (ev)
  (cond ((= (ms:port ev) *dk*)
         ;; code in this clause is executed if EV is from Disklavier
         (cond ((KeyDownEv? ev)
                (key-claim! (ms:pitch ev))
                )
               ((keyUpEv? ev)
                (key-free! (ms:pitch ev))
                )))
        ((= (ms:port ev) *jv*)
         ;; if ev if from JV see if key is available on DK
         (cond ((key-free? (ms:pitch ev))
                ;;code in this clause is executed if key is available
                (format t "~%key ~S is free!" (ms:pitch ev))
                )
               (t
                ;; code in this clause is executed if key is down
                (format t "~%key ~S is NOT free!" (ms:pitch ev))
                ))
         )))

; (clear-keycache)
; (set-receiver! #'test-hook *ms*)
; (remove-receiver! *ms*)

;;;
;;; this hook remembers the TIME of note ons so we can calculate
;;; note durations when noteOffs arrive.

(defun test-hook2 (ev)
  (cond ((KeyDownEv? ev)
         ;; we have a keydown, cache the DATE of the noteDown
         (keycache! (ms:pitch ev) (ms:date ev))
         )
        ((keyUpEv? ev)
         ;; we have a keyUp, calculate the duration by subtracting
         ;; the cached NoteOn time from the noteOff time
         (let ((dur (- (ms:date ev)
                       (keycash (ms:pitch ev)))))
           (print (list :key= (ms:pitch ev) :dur= dur)))
         )))

; (clear-keycache)
; (set-receiver! #'test-hook2 *ms*)
; (remove-receiver! *ms*)

;;; this is like test-hook2 but it caches a list: (key time amp) for
;;; each noteOn

(defun test-hook2 (ev)
  (cond ((KeyDownEv? ev)
         ;; cache info list: (<key> <time> <amp>) for each noteDown
         (keycache! (ms:pitch ev)
                    (list (ms:pitch ev) ; kenum is first list element
                          (ms:date ev)  ; onset is second element
                          (ms:vel ev))) ; amp is third element
         )
        ((keyUpEv? ev)
         (let* ((info (keycash (ms:pitch ev)))  ; get info list
                ;; calculate duration of note
                (dur (- (ms:date ev) (second info))))
           ;; reset the second element of info to be DUR
           (setf (second info) dur)
           (print info)))
        ))

; (clear-keycache)
; (set-receiver! #'test-hook2 *ms*)
; (remove-receiver! *ms*)
