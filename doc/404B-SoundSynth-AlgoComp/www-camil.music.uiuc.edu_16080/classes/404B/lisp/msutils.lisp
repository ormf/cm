(in-package :cm)

;; Define Midishare ports in 5065 (Micro Express)

(defparameter *qt* 0)  ; quicktime
(defparameter *jv* 2)  ; jv-35
(defparameter *dk* 4)  ; disklavier

(defparameter *ms* nil) ;global var for ms stream.

(defun int (n)
  (values (floor n)))

(defun sec->milli (s)
  (int (* s 1000)))

(defun milli->sec (m)
  (/ m 1000.0))

(defun amp->vel (a)
  (int (* a 127)))

;;
;; MidiEv Predicates 

(defun keyEv? (ev)
  ;; true if type is 1 or 2 (typeKeyOn typeKeyOff)
  (< 0 (ms:evType ev) 3)) 

(defun keyDownEv? (ev)
  (and (= (ms:evType ev) 1)
       (> (ms:vel ev) 0)))

(defun keyUpEv? (ev)
  (and (keyEv? ev)
       (not (keyDownEv? ev))))

(defun blackKeyEv? (ev)
  ;; the "true" value is the black pitch class
  (and (keyEv? ev)
       (find (mod (ms:pitch ev) 12) '(1 3 6 8 10))))

(defun whiteKeyEv? (ev)
  ;; the "true" value is the white pitch class
  (and (keyEv? ev)
       (find (mod (ms:pitch ev) 12) '(0 2 4 5 7 9 11))))












