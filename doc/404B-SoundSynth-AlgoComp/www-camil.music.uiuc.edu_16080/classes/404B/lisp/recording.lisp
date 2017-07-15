(in-package :cm)

(defparameter *memory* (list ))
(defparameter *recording* nil)

(defun start-recording ()
  ;; set recording memory to empty list and set *recording* to true so
  ;; we know we have started to record
  (setq *recording* T)
  (setq *memory* (list )))

;;; this is how to "record" information into *memory*:
;;; (these two statements are equivalent)
; (setq *memory* (cons (random 127) *memory*))
; (push (random 127) *memory*)

(defun record-midiEv (ev)
  ;; record the keynum of ev into *memory*
  ;; by pushing it onto the front of the memory list
  (push (list (ms:pitch ev)
              (ms:vel ev))
        *memory*) )

(defun stop-recording ()
  ;; reverse the memory list so that it is in the proper order, set
  ;; *recording lflag to false
  (setq *recording* nil)
  (setq *memory* (reverse *memory*)))

(defun recording? ()
  ;; return flag value if we are recording or not
  *recording*)

;; test it outt


(start-recording)
(recording?)

(push (random 127) *memory*)
(stop-recording)

(defun dk-record-hook (ev)
  (if (and (recording?)
           (= (ms:port ev) *dk*)
           (keyDownEv? ev)
           )
      (record-midiEv ev)
      (ms:midiFreeEv ev)))

(setq *ms* (midishare-open))
(start-recording)
(set-receiver! #'dk-record-hook *ms*)
(stop-recording)
(remove-receiver! *ms*)

(defun dk-record-hook2 (ev)
  (if (and (= (ms:port ev) *dk*)
           (keyDownEv? ev))
      (let (
            (knum (ms:pitch ev))
            )
        (cond ((= knum 108)
               (stop-recording)
               (print *memory*)
               )
              ((= knum 21)
               (start-recording)
               )
              ((recording?)
               (record-midiEv ev))
              (t
               (ms:midiFreeEv ev))))
      (ms:midiFreeEv ev)))

(set-receiver! #'dk-record-hook2 *ms*)