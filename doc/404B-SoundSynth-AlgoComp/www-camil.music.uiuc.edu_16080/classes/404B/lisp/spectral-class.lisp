;;
;; spear data
;;
;; (import-spear-data file &key start end point-format freq-scaler
;;                              amp-scaler time-scaler)
;;   Imports spear frame or partial data from file
;;   :point-format   {:raw | :hertz | :keynum | :note
;;                   ({:hertz|:keynum|:note} :amplitude)
;;                   (:time {:hertz|:keynum|:note})
;;   :start          starting frame/partial to import, default 0
;;   :end            ending frame/partial to import, default all
;;   :freq-scaler    scaler for :hertz points, semitone shift for :keynum :note
;;   :amp-scaler     scaler on amp values
;;   :time-scaler    scaler on time values

;; (convert-spectrum spec fmat)
;;   Convert spectrum freqs to new fmat {:hertz|:keynum|:note}
;; (rescale-spectrum spec . minf maxf mina maxa)
;;   Rescale specturm to lie within optional new boundaries minf maxf mina maxa
;;   if a boundary is false that boundary remains unchanged. minf and maxf
;;   should agree with actual frequency format of spec
;; (invert-spectrum spec . amps?)
;;   Inverts spectral components and optionally amps
;; (plot-spectrum spec)
;;  Draws spec in plotter. spec can be list of spectra.
;; (spectrum-maxfreq spec . fmat)
;;   Return max freq of spec optionally converted to {:hertz|:keynum|:note}
;; (spectrum-minfreq spec . fmat)
;;   Return min freq of spec optionally converted to {:hertz|:keynum|:note}
;; (spectrum-maxamp spec)
;;   Return max amp of spec
;; (spectrum-minamp spec)
;;   Return min amp of spec
;; (spectrum-freqs spec . fmat)
;;   Return list of all freqs optionally converted to {:hertz|:keynum|:note}
;; (spectrum-amps spec)
;;   Return list of all amps
;; (spectrum->midi spec . slotinits)
;;   Return list of midis whose keynums & optionally amps are from spec.


;; look at:

;; 

(cd)
(probe-file "amen.txt")

(import-spear-data "amen.txt")
(setq spectra (import-spear-data  "amen.txt" :point-format '(:keynum :amplitude)))

(setq frame1 (first spectra))
(spectrum-freqs frame1 :note)
(spectrum-maxfreq frame1 :note)


(defparameter amen (import-spear-data  "amen.txt" :point-format :note))

(defun play-amen (rep dur rate amp)
  (process with pat = (new heap :of amen :for rep)
           for chord = (next pat)
           for five = (loop repeat 5 for x in (shuffle chord) collect x)
           until (eop? pat)
           each k in five output (new midi :time (now) :keynum k
                                       :duration dur  :amplitude amp)
           wait rate))

(events (play-amen 20 1.5 1 .7) "amen.mid")

(setq spectra (import-spear-data  "amen.txt" :point-format '(:keynum :amplitude)))


(plotter (apply #'append (loop for s in spectra for b from 1 by .5
                            collect (spectrum->midi s :time b :duration .1)))
         :x-axis (axis :seconds :slot '(time duration))
         :y-axis (axis :keynum))





(setq mydata '((EF3 AF3 FS4 G4 BF4 C5 D5 F5 FS5 G5 G5 BF5 B5 C6 CS6 D6)
               (BF0 AF2 G3 A3 D4 FS4 G4 AF4 BF4 B4 C5 D5 EF5 E5 F5 FS5 G5 AF5 A5 BF5 BF5 B5 C6 CS6 D6 FS6 B6)
               (B0 AF2 G3 B3 D4 G4 AF4 B4 C5 D5 D5 E5 E5 F5 FS5 AF5 A5 A5 BF5 BF5 B5 C6 CS6 D6 E6 F6 G6 BF6)
               (FS0 BF1 AF2 AF3 BF3 D4 FS4 AF4 C5 D5 D5 EF5 E5 F5 FS5 G5 AF5 A5 BF5 B5 C6 CS6 EF6 E6 F6 FS6 B6 B6)
               (CS1 AF2 G3 B3 EF4 F4 AF4 C5 D5 EF5 E5 F5 FS5 AF5 BF5 B5 C6 CS6 EF6 E6 F6 FS6 G6 AF6 B6 B6)
               (C0 A1 AF2 AF3 B3 EF4 F4 AF4 C5 D5 EF5 E5 F5 AF5 BF5 BF5 B5 CS6 EF6 F6 F6 F6 G6 G6 AF6 AF6 BF6 B6)
               (FS-1 CS1 G2 AF3 B3 EF4 FS4 AF4 C5 EF5 F5 FS5 AF5 BF5 C6 EF6 E6 F6 FS6 FS6 G6 G6 AF6 A6 BF6 BF6 B6)
               (AF-1 AF2 AF3 C4 EF4 FS4 AF4 C5 EF5 F5 FS5 AF5 BF5 C6 EF6 E6 F6 F6 FS6 FS6 G6 AF6 AF6 B6)
               (BF0 A2 AF3 BF3 C4 EF4 FS4 AF4 B4 C5 EF5 FS5 G5 BF5 C6 EF6 E6 F6 F6 FS6 FS6 G6 AF6 A6 B6)
               (F1 A2 AF3 BF3 CS4 EF4 F4 AF4 BF4 C5 D5 EF5 F5 AF5 BF5 C6 EF6 F6 F6 FS6 FS6 G6 G6 A6)
               (BF0 BF2 BF3 E4 F4 AF4 BF4 D5 EF5 F5 BF5 C6 EF6 F6 FS6 FS6 AF6)
               (A0 BF2 BF3 EF4 F4 AF4 BF4 D5 F5 BF5 EF6 F6 FS6 AF6)
               (AF0 BF2 BF3 F4 BF4 D5 F5 D6 F6 FS6))


           
           
           
       
