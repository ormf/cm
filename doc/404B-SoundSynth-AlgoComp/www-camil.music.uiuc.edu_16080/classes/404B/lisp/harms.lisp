(in-package :cm)

(defun hscale (h1 h2 &optional (fund 1) (mode ':normal))
  ;; calculate overtones from h1 to h2 in hertz. if fund is 1 (the
  ;; default) then freq ratios are returned.
  (if (< h1 h2)
      (ecase mode
        (:normal
         (loop for h from h1 to h2
            collect (* fund (/ h h1))))
        (:invert
         (loop for h from h2 downto h1
            collect (* fund (/ 1 (/ h h2 ))))))
      (ecase mode
        (:normal
         (loop for h from h1 downto h2
            collect (* fund (/ h h1))))
        (:invert
         (loop for h from h2 to h1
            collect (* fund (/ 1 (/ h h2))))))))

(hscale 8 16)
(hscale 8 16 1 :invert)
(hscale 8 16 440)
(hscale 16 8 440)
(hscale 8 16 440 :invert)
(hscale 16 8 440 :invert)

(defun kscale (h1 h2 &optional (fund-knum 60.0) (mode ':normal))
  ;; same as hscale, but returns keynums of fund specified as knum
  (keynum (hscale h1 h2 (hertz fund-knum) mode) :hz ))

(kscale 8 16 69)
(kscale 8 16 'a4)
(kscale 8 16 'gs3)

(defun testit (h1 h2 fund mode rate)
   (process for k in (kscale h1 h2 fund mode)
            output (new midi :time (now)
                        :keynum k
                        :duration (* rate 1.5)
                        :amplitude .75)
            wait rate))

(cd )

(events (testit 8 16 'a4 ':normal .5)
         "test.mid" :channel-tuning 2)

(defun playpat (len h1 h2 knum mode rate dur amp)
   (let ((mypat (new heap :of (kscale h1 h2 knum mode))))
     (process repeat len
              output (new midi :time (now)
                          :keynum (next mypat)
                          :duration dur
                          :amplitude amp)
              wait rate)))

(events (playpat 12 8 16 'a4 ':normal .2 .5 .7)
        "test.mid" :channel-tuning 2)

(events (playpat 40 2 32 'c2 :normal .3 .4 .8) "test.mid")

(events (playpat 80 2 32 (pickl '(c2 a2 e2)) :normal .2 .4 .8) "test2.mid")

(defparameter myscale (new tuning :ratios (hscale 8 16 )))

(keynum 61 :to myscale)

(keynum 0 :from myscale)
(keynum 8 :from myscale)
(keynum 40 :from myscale)
(keynum 41 :from myscale)

