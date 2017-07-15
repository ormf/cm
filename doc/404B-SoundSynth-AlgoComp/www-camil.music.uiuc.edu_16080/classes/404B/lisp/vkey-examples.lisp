;; Working with VKEY

;; The Sample Databases

(load "/Classes/Music 404b/Samples/sdb.lisp")

;; Logical pathnames

(directory "sdb:Crotales;*.aiff")
(translate-logical-pathname "sdb:pp;a3.aiff")
(dac "sdb:pp;a3.aiff")

;; compiling and loading the vkey files

(cd )
(cload "fullmix.ins")
(cload "expandn.ins")
(cload "vkey.lisp")

;;; How to generate a sample database from a directory of samples:

(pprint (vkey-db "sdb:flute;ff;"))

;;; the Vkey instrument

(with-sound (:srate 44100 :channels 2)
  (vkey 0 nil 'c6 *crotales*)
  )

(with-sound (:srate 44100 :channels 2)
    (vkey 0 nil 'c6 *crotales* :expand 3)
  )

(with-sound (:srate 44100 :channels 2)
    (vkey 0 nil 'c6 *crotales* :expand 3)
    (vkey 0 nil 'd6 *crotales* :expand 3)
  )

(with-sound (:srate 44100 :channels 2)
  (loop with x = 0
        for r in (explsegs 10 8 1/36)
     do 
       (vkey x nil (vary (keynum 'a6) .02)
              (pick *marimba* *crotales*)
              :amplitude 1.5)
       (incf x r))
  (vkey 0 nil 57 *plucked-piano*)
  (vkey 1.5 nil 57.35 *plucked-piano* :amplitude .8)
  (vkey 3 nil 57.15 *plucked-piano* :amplitude .6)
  (vkey 4.5 nil 57.8 *plucked-piano* :amplitude .4)
  (vkey 6 nil 57.5 *plucked-piano* :amplitude .2))

;;;
;;; sounds from aeolian harp
;;;

(with-sound (:srate 44100 :channels 2)
  (vkey 0 nil 69 *plucked-piano*)
  (vkey 0 nil 57 *plucked-piano* :amplitude .2 :expand 2)
  (vkey 1 nil (keynum (* 220 3/2 ) :hz) *plucked-piano*
        :amplitude .6 :expand 3))

(dac "sdb:sounds;strum1.aiff")

(with-sound (:srate 44100 :channels 2)
  (dolist (rat (shuffle '(1 9/8 32/27 4/3 3/2 128/81 16/9 2)))
    (vkey 0 nil rat "sdb:sounds;strum1.aiff"
          :amplitude .35
          :expand 2)))

;;; using processes

(defparameter strum1-dur
  (clm:sound-duration 
   (namestring
    (truename "sdb:sounds;strum1.aiff"))))

(defun aeolian-strum (file)
  (process for ratio in (shuffle '(1 9/8 32/27 4/3 3/2
                                   128/81 16/9 2))
           output
           (new expandn :time (now) 
                :filename file
                :duration (* strum1-dur ratio)
                :amplitude .9
                :expand 2
                :srate ratio)
           wait (vary .5 .1)))

(events (list (aeolian-strum "sdb:sounds;strum1.aiff")
              (aeolian-strum "sdb:sounds;strum1.aiff")
              (aeolian-strum "sdb:sounds;strum1.aiff"))
        "astrum.aiff" 
        '(0 8 16)
        :srate 44100 :channels 2 :trace-output t )

;;; this one takes a while...

(defun moby-aeolian-strum (file )
  (process for ratio in (shuffle '(1 9/8 32/27 4/3 3/2
                                   128/81 16/9 2))
           output
           (new expandn :time (now) 
                :filename file
                :duration (* strum1-dur ratio 8)
                :amplitude .6
                :expand  (* strum1-dur ratio 8)
                :srate ratio)
           wait (vary 1.5 .1)))

(events (moby-aeolian-strum "sdb:sounds;strum1.aiff")
        "moby.aiff"  :srate 44100 :channels 2 )


