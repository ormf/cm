
(cd )

(load "/Volumes/Classes/Music 404b/Samples/sdb.lisp")

(dac "sdb:pp;a4.aiff")

(defparameter sf (namestring (truename "sdb:pp;a4.aiff")))

(sound-chans sf)
(sound-srate sf)
(sound-duration sf)
(sound-comment sf)
(sound-data-format sf)
(sound-format-name (sound-data-format sf))
(sound-header-type sf)
(sound-type-name (sound-header-type sf))
(sound-frames sf)
;; get max amp of both chans
(let ((da (clm:make-double-array 2))
      (ia (clm:make-integer-array 2)))
  (sound-maxamp sf 2 da ia)
  (list (elt da 0) (elt da 1)))

;;; Fullmix

(cload "fullmix.ins")

(object-parameters (new fullmix))

; infile &optional beg outdur inbeg matrix srate reverb-amount srenv

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  (fullmix sf)
  (fullmix sf .5)
  )

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  (fullmix sf)
  (fullmix sf .5 .1)
  )

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  (fullmix sf)
  (fullmix sf 6 nil 2)
  )

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  (fullmix sf)
  (fullmix sf .5 nil nil '((1 0) (0 1)))
  )

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  ;; both chans send to left
  (fullmix sf nil nil nil '((1 0) (1 0)))
  ;; both chans send to middle 
  (fullmix sf 2 nil nil '((.5 .5) (.5 .5)))
  ;; both chans send to right
  (fullmix sf 4 nil nil '((0 1) (0 1)))
  )

(with-sound (:output "test.aiff" :channels 2 :srate 44100)
  ;; left chan appears in right 1/4 through sound
  (fullmix sf nil nil nil '((1 (0 0 .25 1 1 1))
                            (0 0)
                            ))
  (fullmix sf nil nil nil '((1 (0 0 .25 1 1 1))
                            (0 0) 
                            ))
  )





