;;; -*- syntax: common-lisp; base: 10; mode: lisp -*-
;;;
;;; an example of using clm in "algorithmic composition"
;;;
;;; this is a translation of the Pla example in Douglas Keislar's article
;;; "Six Microtonal Composers" in Perspectives of New Music
;;;
;;; :ld clm-example.lisp creates tst.clm.  
;;; :ld tst.clm then produces our little tune.

(mus-set-rand-seed 5676)
(defun my-random (size) (clm-random size)) ;provide repeatable random number sequences

(setf lim 256)
(setf time 60)
(setf mode (make-array 13 :element-type 'fixnum :initial-contents '(0 0 2 4 11 11 5 6 7 0 0 0 0)))

(setf rats (make-array 13 
		       :initial-contents (map 'list #'float
					      '(1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0))))

(defun tune (x)
  (let* ((pit (mod x 12))
	 (oct (floor x 12))
	 (base (aref rats pit)))
    (* base (expt 2 oct))))

(setf bell '(0 0 
	       10 .25
	       90 1.0
	       100 1.0))

(defun rbell (x) (envelope-interp (* x 100) bell))

(setf pits (make-array (1+ lim) :element-type 'fixnum))
(setf octs (make-array (1+ lim) :element-type 'fixnum))
(setf rhys (make-array (1+ lim) :element-type 'fixnum))
(setf begs (make-array (1+ lim) :element-type 'fixnum))
(setf amps (make-array (1+ lim) :element-type 'fixnum))

(loop for i from 0 to lim do
  (setf (aref octs i) (floor (+ 4 (* 2 (rbell (my-random 1.0))))))
  (setf (aref pits i) (aref mode (floor (* 12 (my-random 1.0)))))
  (setf (aref rhys i) (floor (+ 4 (* 6 (my-random 1.0)))))
  (setf (aref begs i) (floor (+ 1 (* 3 (rbell (my-random 1.0))))))
  (setf (aref amps i) (floor (+ 1 (* 8 (rbell (my-random 1.0)))))))

(with-open-file 
    (notelist "tst.clm"
     :if-exists :supersede :if-does-not-exist :create :direction :output)
  (print (format notelist ";from agn.cl~%~A~%

(setf pna '(0 0 1 1 10 .6000 25 .3000 100 0 ))
(setf ind2 '(0 1 25 .4000 75 .6000 100 0 ))
(setf amp '(0 0 25 1 60 .7000 75 1 100 0 ))
(setf tap '(0 0 1 1 99 1 100 0 ))
(setf skwfrq '(0 -1 5 .2500 10 0 100 .1000 ))
(setf pizzf '(0 0 1 1 5 .6000 10 .3000 25 .1000 100 0 ))
(setf metalamp '(0 0 .5000 1 5 1 10 .5000 15 .2500 35 .1000 100 0 ))

(setf windy0 '(0 0 40 .1000 60 .2000 75 .4000 82 1 90 1 100 0 ))
(setf windy1 '(0 0 60 .1000 80 .2000 90 .4000 95 1 100 0 ))
(setf windy2 '(0 0 10 1 16 0 32 .1000 50 1 56 0 60 0 90 .3000 100 0 ))
(setf windy3 '(0 0 30 1 56 0 60 0 90 .3000 100 0 ))
(setf windy4 '(0 0 50 1 80 .3000 100 0 ))
(setf windy5 '(0 0 40 .1000 60 .2000 75 .4000 82 1 90 1 100 0 ))
(setf windy6 '(0 0 40 .1000 60 .2000 75 .4000 82 1 90 1 100 0 ))
(setf windy7 '(0 0 10 1 32 .1000 50 1 90 .3000 100 0 ))
(setf windy8 '(0 0 60 .1000 80 .3000 95 1 100 0 ))
(setf windy9 '(0 0 80 .1000 90 1 100 0 ))

(with-sound (:reverb jc-reverb :statistics t :channels 2) ~%" (make-default-header)))

  (loop for i from 1 to 3 do
    (let ((cellbeg 0)
          (cellsiz 4)
          (cellctr 0)
          (whichway 1)
          (base i)
          (mi (1- i))
          (winenv nil)
          (winnum 0)
          (mytempo .20)
          (nextbeg 0.0)
          (revamt 0.0)
          (ranamt 0.0)
          (beg 0.0)
          (dur 0.0)
          (freq 0.0)
          (ampl 0.0)
          (ind 0.0))
      (loop while (and (< beg time) (< cellctr lim)) do
        (incf beg nextbeg)
;        (setf nextbeg (* mytempo (aref begs cellctr)))
        (setf dur (max .25 (* mytempo (+ .9 (* .2 (my-random 1.0))) (aref rhys cellctr))))
        (setf nextbeg dur)
        (setf freq (* (/ 16.351 (expt 2 mi)) (tune (aref pits cellctr)) (expt 2 (aref octs cellctr))))
        (if (< freq 100) (incf dur dur))
        (setf ampl (max .003 (* (aref amps cellctr) (/ (* 60 base)))))
        (setf ind (* (my-random 1.0) 2 base))
        (setf revamt (* base .1))
        (setf winnum (floor (* 10 (- beg (floor beg)))))
        (setf winenv (if (= winnum 0) 'windy0
                       (if (= winnum 1) 'windy1
                         (if (= winnum 2) 'windy2
                           (if (= winnum 3) 'windy3
                             (if (= winnum 4) 'windy4
                               (if (= winnum 5) 'windy5
                                 (if (= winnum 6) 'windy6
                                   (if (= winnum 7) 'windy7
                                     (if (= winnum 8) 'windy8
                                       'windy9))))))))))
        (setf ranamt (* .00001 (expt (- (log freq 2.0) 4) 4)))
        (format notelist "  (fm-violin ~,2F ~,2F ~,3F ~,2F :fm-index ~,2F :reverb-amount ~,2F :noise-amount ~,2F :amp-env ~A)~%" 
                beg dur freq ampl ind revamt ranamt winenv)
        (incf cellctr)
        (when (> cellctr (+ cellsiz cellbeg))
          (incf cellbeg 1)
          (when (> (my-random 1.0) .5)
             (incf cellsiz whichway))
          (if (and (> cellsiz 16) (> (my-random 1.0) .99))
              (setf whichway -2)
            (if (and (> cellsiz 12) (> (my-random 1.0) .999))
                (setf whichway -1)
              (if (< cellsiz 4)
                  (setf whichway 1)))) 
          (incf cellbeg 3)
          (setf cellctr cellbeg)))))
  (format notelist ")~%"))


#| 
Here's an example of using CM and CLM written by Nicky Hind:

    (let ((my-pitches (pitches c4 d e f g))
          (my-durations (items 1 2 3 in random))
          (my-rhythms (items 1 2 3 in random))
          (number-of-notes 10)
          (start-time 0))
      (with-sound () 
         (loop for i from 1 to number-of-notes do
            (fm-violin start-time          ; start-time
                  (item my-durations) ; duration
                  (item my-pitches)   ; frequency
                  (between 0.02 0.2))  ; amplitude
            (incf start-time (item my-rhythms)))))
|#
