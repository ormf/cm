;;; CLM-3 test suite

(defvar include-notelists nil)
(defvar play-it t)

(setf clm::*clm-dac-wait-default* t)

#+openmcl (defun clm-getenv (str) (ccl::getenv str))
#+excl (defun clm-getenv (str) (system:getenv str))
#-(or excl openmcl) (defun clm-getenv (name) (declare (ignore name)) )
(defvar hostname (clm-getenv #+darwin "HOST" #-darwin "HOSTNAME"))

(defvar home-dir "/home/bil")

(if (probe-file "/export/home/bil/cl/oboe.snd")
    (setf home-dir "/export/home/bil")
    (if (probe-file "/Users/bill/cl/oboe.snd")
	(setf home-dir "/Users/bill")
	(if (probe-file "/user/b/bil/cl/oboe.snd")
	    (setf home-dir "/user/b/bil"))))

(setf *clm-header-type* #+sun mus-next #+(or openmcl sgi) mus-aifc #+(or linux windoze) mus-riff)
(setf *clm-data-format* #+(or openmcl sgi sun) mus-bshort #+(or linux windoze) mus-lshort)

(defvar already-loaded nil)
(setf already-loaded nil)

(defun setup-ins (&rest ins)
  (let ((happiness nil))
    (loop for inst in ins do
      ;; look for .o file -- if up-to-date, load it, else compile and load (ins/lisp/cl as extensions)
      ;; return nil if something went wrong (so that following test is ignored)
      (let* ((compiled-name (concatenate 'string inst "." clm::*clm-fasl-name*))
	     (ins-name (concatenate 'string inst ".ins"))
	     (happy (probe-file ins-name))
	     (compiled nil))
	(when (not happy)
	  (setf ins-name (concatenate 'string inst "." clm::*clm-lisp-name*))
	  (setf happy (probe-file ins-name))
          (when (not happy)
	     (setf ins-name (concatenate 'string inst ".cl"))
	     (setf happy (probe-file ins-name))))
        (when happy
	  (when (or (not (probe-file compiled-name))
		    (> (file-write-date (truename ins-name)) (file-write-date (truename compiled-name))))
	    #+sbcl (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
				  (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		     (format t "~%;compiling ~A" ins-name)
		     (compile-file ins-name :output-file compiled-name))
	    #-sbcl (compile-file ins-name :output-file compiled-name)
	    (setf compiled t))
	  (when (or compiled
		    (not (member inst already-loaded)))
	    (without-warnings
	     #-cmu (load compiled-name)
	     #+cmu (load (concatenate 'string inst ".cmucl"))
	     )
	    (pushnew inst already-loaded)))
	(push happy happiness)))
    (every #'identity happiness)))

(defun fneq (a b) (> (abs (- a b)) .001))

(load "ffi-test.lisp")

;;; --------------------------------------------------------------------------------
;;; fm-violin, basic with-sound, reverb

(setup-ins "v" "jcrev" "nrev")

(let* ((file (with-sound (:play play-it) (fm-violin 0 .2 440 .1)))
       (chans (mus-sound-chans file))
       (dur (mus-sound-duration file)))
  (if (not (= chans 1)) (format t "initial test chans: ~A~%" chans))
  (if (fneq dur .2) (format t "initial test dur: ~A~%" dur)))
       
(let* ((file (with-sound (:play play-it :channels 2) (fm-violin 0 .2 440 .1)))
       (chans (mus-sound-chans file)))
  (if (not (= chans 2)) (format t "initial test stereo chans: ~A~%" chans)))

(with-sound (:play play-it :channels 2 :reverb jc-reverb) (fm-violin 0 .2 440 .1 :reverb-amount .1))
(if include-notelists (load "i.clm"))


;;; --------------------------------------------------------------------------------
;;; ug2 -> all gens

(when (setup-ins "ug2")

  (with-sound (:play play-it :statistics t)
    (simple-ssb 0 .2 440 .1)
    (simple-sos .25 .2 .1)
    (simple-soc 0.5 .2 440 .1)
    (simple-osc 0.75 .2 440 .1)
    (simple-sss 1.0 .2 .1)
    (simple-asy 1.25 .2 .1)
    (simple-saw 1.5 .2 .1)
    (simple-tri 1.75 .2 .1)
    (simple-pul 2.0 .2 .1)
    (simple-sqr 2.25 .2 .1)
    (simple-sib 2.5 .2 440.0 .1)
    (simple-oz 2.75 .2 440.0 .1)
    (simple-op 3.0 .2 440.0 .1)
    (simple-tz 3.25 .2 440.0 .1)
    (simple-tp 3.5 .2 440.0 .1)
    (simple-frm 3.75 .2 440.0 .1)
    (simple-wav 4.0 .2 440.0 .1)
    (simple-poly 4.25 .2 440.0 .1)
    (simple-dly 4.75 .2 440.0 .1)
    (simple-cmb 5.0 .2 440.0 .1)
    (simple-not 5.25 .2 440.0 .1)
    (simple-alp 5.5 .2 440.0 .1)
    (simple-ave 5.75 .2 440.0 .1)
    (simple-tab 6.0 .2 440.0 .1)
    (simple-flt 6.25 .2 440.0 .1)
    (simple-fir 6.5 .2 440.0 .1)
    (simple-iir 6.5 .2 440.0 .3)
    (simple-f 6.75 .2 440.0 .1)
    (simple-ran 7.0 .2 440.0 .1)
    (simple-ri 7.25 .2 440.0 .1)
    (simple-env 7.5 .2 440.0 .1)
    (simple-amb 7.75 .2 440.0 .1)
    (simple-fof 8 1 270 .1 .001 730 .6 1090 .3 2440 .1) ;"Ahh"
    (simple-fof 9 4 270 .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
		'(0 0 .5 1 3 .5 10 .2 20 .1 50 .1 60 .2 85 1 100 0))
    (simple-fof 9 4 (* 6/5 540) .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
		'(0 0 .5 .5 3 .25 6 .1 10 .1 50 .1 60 .2 85 1 100 0))
    (simple-fof 9 4 135 .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
		'(0 0 1 3 3 1 6 .2 10 .1 50 .1 60 .2 85 1 100 0))
    (simple-osc-1 13 .5 440 1)
    (simple-rd 13.5 .45 .75 "oboe.snd")
    (simple-rd-start 13.65 .25 .75 "oboe.snd" 0 0)
    (simple-rd-start 13.8 .25 .75 "oboe.snd" 0 12345)
    (simple-rd-start 13.9 .25 .75 "oboe.snd" 0 12345678)
    (simple-cnv 14.0 .45 .75 "oboe.snd")
    (simple-cnf 14.5 .45 .75 "oboe.snd")
    (simple-lrg 15.0 .45 .75 "oboe.snd")
    (simple-cn2 15.5 .45 .4 "oboe.snd")
    (simple-src 16  .45 1.0 2.0 "oboe.snd")
    (simple-sr2 16.5 .45 1.0 2.0 "oboe.snd")
    (simple-sr2a 16.75 .45 1.0 2.0 "oboe.snd")
    (simple-rndist 17.0 .2 440.0 .1)
    (simple-ridist 17.25 .2 440.0 .1)
    (simple-sro 17.5 .45 .1 .5 440)
    (simple-grn 18 .2 .1 1.0 440)
    (simple-pvoc 18.25 .2 .4 256 "oboe.snd")
    (simple-ina 18.5 .45 1 "oboe.snd")
    (simple-rdf 19 .45 1 "oboe.snd")
    (simple-f2s 19.5 .45 1 "oboe.snd")
    (simple-loc 20 .2 440 .1)
    (simple-out 20.25 .2 440 .1)		  
    (simple-dup 20.5 .2 440 .1)
    (simple-dup 20.75 .2 440 .1)
    ))


;;; --------------------------------------------------------------------------------
;;; fm-insect

(when (setup-ins "insect")

  (let ((locust '(0 0 40 1 95 1 100 .5))
	(bug_hi '(0 1 25 .7 75 .78 100 1))
	(amp '(0 0 25 1 75 .7 100 0)))
    (with-sound (:play play-it)
      (fm-insect 0      1.699  4142.627  .015 amp 60 -16.707 locust 500.866 bug_hi  .346  .500)
      (fm-insect 0.195   .233  4126.284  .030 amp 60 -12.142 locust 649.490 bug_hi  .407  .500)
      (fm-insect 0.217  2.057  3930.258  .045 amp 60 -3.011  locust 562.087 bug_hi  .591  .500)
      (fm-insect 2.100  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .346  .500)
      (fm-insect 3.000  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .046  .500)
      (fm-insect 3.450  1.500   900.627  .09  amp 40 -16.707 locust 300.866 bug_hi  .006  .500)
      (fm-insect 3.950  1.500   900.627  .12  amp 40 -10.707 locust 300.866 bug_hi  .346  .500)
      (fm-insect 4.300  1.500   900.627  .09  amp 40 -20.707 locust 300.866 bug_hi  .246  .500))))


;;; --------------------------------------------------------------------------------
;;; birds

(when (setup-ins "bird" "bigbird")

  (with-sound (:play play-it) (scissor 0) (scissor 1))

  (if include-notelists (load "bird.clm")))


;;; --------------------------------------------------------------------------------
;;; bagpipe

(when (setup-ins "canter" "drone")

  (let ((fmt1 '(0 1200 100 1000))
	(fmt2 '(0 2250 100 1800))
	(fmt3 '(0 4500 100 4500))
	(fmt4 '(0 6750 100 8100))
	(amp1 '(0 .67 100 .7))
	(amp2 '(0 .95 100 .95))
	(amp3 '(0 .28 100 .33))
	(amp4 '(0 .14 100 .15))
	(ind1 '(0 .75 100 .65))
	(ind2 '(0 .75 100 .75))
	(ind3 '(0 1 100 1))
	(ind4 '(0 1 100 1))
	(skwf '(0 0 100 0))
	(ampf '(0 0 25 1 75 1 100 0))
	(ranf '(0 .5 100 .5))
	(index '(0 1 100 1))
	(zero_fun '(0 0 100 0))
	(atskew '(0 -1 15 .3 22 -.1 25 0 75 0 100 -.2))
	(vibfun '(0 0 .3 .3 15 .6 25 1 100 1))
	(slopefun '(0 1 75 1 100 0))
	(trap '(0 0 25 1 75 1 100 0))
	(ramp '(0 0 25 0 75 1 100 1))
	(solid '(0 0 5 1 95 1 100 0))
	(sfz '(0 0 25 1 30 .6 50 .5 75 .2 100 0))
	(mound '(0 0 10 .4 25 .8 40 1 60 1 75 .8 90 .4 100 0))
	(vio '(0 0 7 .2 25 .5 40 .6 60 .6 75 .5 90 .2 100 0))
	(bassdr2 '(.5 .06 1 .62 1.5 .07 2.0 .6 2.5 .08 3.0 .56 4.0 .24 
		      5 .98 6 .53 7 .16 8 .33 9 .62 10 .12 12 .14 14 .86
		      16 .12 23 .14 24 .17))
	(bassdrstr '(.5 .06 1.0 .63 1.5 .07 2.01 .6 2.5 .08 3.02 .56
			4.04 .24 5.05 .98 6.06 .53 7.07 .16 8.08 .33 9.09 .62
			10.1 .12 12.12 .14 13.13 .37 14.14 .86 16.16 .12 23.23 .14 24.24 .17))
	(tenordr '(.3 .04 1 .81 2 .27 3 .2 4 .21 5 .18 6 .35 7 .03 8 .07 9 .02 10 .025 11 .035))
	(tenordrstr '(.3 .04 1.03 .81 2.03 .27 3.03 .20 4.03 .21 5.03 .18
			 6.03 .35 7.03 .03 8.03 .07 9.03 .02 10.03 .03 11.03 .04)))
    (with-sound (:play play-it :reverb nrev  :play play-it)
      (drone  .000  4.000  115.000  (* .25 .500) solid bassdr2  .100  .500
	      .030  45.000 1  .010 10)
      (drone  .000  4.000  229.000  (* .25 .500) solid tenordr  .100  .500
	      .030  45.000 1  .010 11)
      (drone  .000  4.000  229.500  (* .25 .500) solid tenordr  .100  .500
	      .030  45.000 1  .010 9)
      (canter  .000  2.100 918  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  2.100  .300 688.5  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  2.400  .040 826.2  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  2.440  .560 459  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.000  .040 408  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.040  .040 619.65  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.080  .040 408  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.120  .040 688.5  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.160  .290 459  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.450  .150 516.375  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.600  .040 826.2  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.640  .040 573.75  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.680  .040 619.65  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.720  .180 573.75  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.900  .040 688.5  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
      (canter  3.940  .260 459  (* .25 .700)  45.000 1  .050 ampf ranf skwf
	       .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	       ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  ))))


;;; --------------------------------------------------------------------------------
;;; fm trumpet, reson, jcvoi

(when (setup-ins "trp" "reson" "jcvoi" "cellon" "mlbvoi" "noise" "prc96" "bell")

  (with-sound (:play play-it)
    (fm-trumpet 0 .5)
    (reson .5 1.0 440 .1 2 '(0 0 100 1) '(0 0 100 1) .1 .1 .1 5 .01 5 .01 0 1.0 0.01
	   '((0 0 100 1) 1200 .5 .1 .1 0 1.0 .1 .1)
	   '((0 1 100 0) 2400 .5 .1 .1 0 1.0 .1 .1))
    (let ((ampf '(0 0 1 1 2 1 3 0))) 
      (fm-voice 1.5 1 300 .8 3 1 ampf ampf ampf ampf ampf ampf ampf 1 0 0 .25 1 .01 0 ampf .01))
    (vox-1 2 5 600 .2 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 I 5 OW 10 I 50 AE 100 OO) .01 .1)  
    (cellon 3 1 220 .1 '(0 0 25 1 75 1 100 0) '(0 0 25 1 75 1 100 0) .75 1.0 0 0 0 0 1 0 0 220 
	    '(0 0 25 1 75 1 100 0) 0 0 0 0 '(0 0 100 0) 0 0 0 0 '(0 0 100 0))
    (fm-noise 4 2.0 500 .25 '(0 0 25 1 75 1 100 0) .1 .1 1000 
	      '(0 0 100 1) .1 .1 10 1000 '(0 0 100 1) 0 0 100 500 '(0 0 100 1) 0 0)
    (tubebell 3.5 1 440 .1)
    (wurley 4 1 440 .1)
    (Rhodey 4.5 .5 440 .1)
    (hammondoid 5 .5 440 .1)
    (metal 5.5 .5 440 .1)
    (fm-bell 6 1 220 .1 '(0 0 1 1 20 0) '(0 0 1 1 20 .5) 1.0)
    ))


;;; --------------------------------------------------------------------------------
;;; additive synthesis

(when (setup-ins "add" "badd" "lbjPiano")

  (with-sound (:play play-it)
    (test-add)
    (badd 1 1 440 .1)
    (lbj-piano 2 1 440 .1)))


;;; --------------------------------------------------------------------------------
;;; physical modeling, part 1

(when (setup-ins "prc-toolkit95" "pluck" "maraca")

  (with-sound (:play play-it)
    (plucky 0 .3 440 .2)
    (bow-1 .5 .3 220 .2)
    (brass 1 .3 440 .2)
    (clarinet 1.5 .3 440 .2)
    (flute 2 .3 440 .2)
    (pluck 2.5 1 330 .3 .96 0 0 0)
    (big-maraca 3 1 .25 0.95 0.9985 .03125 '(2300 5600 8100) '(0.96 0.995 0.995) .01)))


;;; --------------------------------------------------------------------------------
;;; ug3

#-sbcl (compile-file "/home/bil/cl/dlocsig/dlocsig.lisp")
#-sbcl (load "/home/bil/cl/dlocsig/dlocsig")

(when (setup-ins "ug3")
  (with-sound (:play play-it)
    (sample-desc 0 .2 440 .1)
    (sample-mdat .25 .2 440 .1)
    (sample-xtab .5 .2 440 .1)
    (sample-xts .75 .2 440 .1)
    (sample-srl2 1 .2 .2 .5 (* 440 2))
    (sample-srll 1.25 .2 .1 .5 (* 440 4))
    (sample-srl3 1.5 .2 .1 .5 880)
    (sample-grn2 1.75 .2 .1 .5 880)
    (sample-grn3 2 .45 1 1 "oboe.snd")
    (sample-cnv 2.5 .45 1 1 "oboe.snd")
    (sample-cnv1 3.0 .45 1 1 "oboe.snd")
    (sample-pvoc1 3.5 .45 1 512 "oboe.snd")
    (sample-pvoc2 4.0 .45 1 512 "oboe.snd")
    (sample-pvoc3 4.5 .45 1 512 "oboe.snd")
    (sample-mxf 5 .2 440 .1)
    (sample-osc 5.25 .2 440 .1)
    (sample-ardcl 5.5 .2 440 .1)
    (sample-strs 5.75 .2 440 .1)
    (sample-flt 6 .2 440 .1)
    (sample-arrintp 6.25 .2 440 .1)
    (sample-if 6.5 .2 440 .1)
    (sample-arrfile 6.75 .2 440 .15)
    #-sbcl (simple-dloc 7 1 440 .1)
    (sample-write)
    (sample-read)
    (sample-iwrite)
    (sample-iread)
    (test-reflect)
    (test-mus-method-declaration)
    (expandn 8 .5 "oboe.snd" .2)
    )

  (with-sound ()
    (test-power-env 1.0 '(0 0 .325  1 1 32  2 0 0))
    (test-power-env 1.0 '(0 0 .325  1 1 32  2 .5 1  3 1.0 .1234 4 0.0 0.0))
    (test-power-env 1.0 '(0 0 0  1 1 1  2 .5 .123  3 1.0 321 4 0.0 0.0))))


(when (setup-ins "ug4")
  (with-sound (:play play-it)
    (or1) (or2) (or3) (or4) (if1) (let1)
    (doc-1 0 0.2 440.0 0.2)
    (doc-2 .25 0.2 440.0 0.2)
    (doc-2 .5 0.2 440.0 0.2 '(0 0 1 1 2 .2 5 0))
    (doc-3 .75 .2 440 .1 2 1.0)
    (doc-3 1 .2 440 .1 3 .5 '(0 0 1 1 2 .2 5 0) '(0 0 1 1))
    (doc-4 1.25 .2 440 .4 '(0 0 50 1 75 0 86 .5 100 0))
    (doc-5 1.5 1 11025)
    (doc-6 3000)
    (doc-7 2 .05 1800 1800 .2
           '(.00 .00 .40 1.00 .60 1.00 1.00 .0)
           '(.00 .00 .25 1.00 .60 .70 .75 1.00 1.00 .0)
           '(1 .5 2 1 3 .5 4 .1 5 .01))
    (doc-8 2.25 1 "oboe" "fyow" .5 256 1.0 3 100)
    (doc-9 0 1 .2 :xcoeffs (envelope->coeffs :order 12 :envelope '(0 0.0 .125 0.5 .2 0.0 .3 1.0 .5 0.0 1.0 0.0)))
    (doc-1-0 (* 22050 3) 60000 .5 1.0 "pistol.snd")
    (doc-1-1 3.5 .5 1.0 2.0 '(0 1 1 2) "oboe.snd")
    (doc-1-2 4 .5 1.0   1 .3 20 "fyow.snd")
    (doc-1-2 4.5 .5 10.0   .01 1 10 "fyow.snd")
    (doc-1-2 5 .5 1.0   .9 .05 60 "oboe.snd")
    (doc-1-2 5.5 .5 1.0   1.0 .5 124 "oboe.snd")
    (doc-1-2 6 .5 10.0   .01 .2 8 "oboe.snd")
    (doc-1-2 6.5 .5 1.0   1 3 20 "oboe.snd")
    (doc-1-3 (* 22050 7) 5000 "oboe.snd" 10.0 .01)
    (doc-1-4 7.5 .5 1.0 2.0 '(0 1 1 2) "oboe.snd")
    (doc-1-5 8 .5 (make-double-array 4 :initial-contents (mapcar #'double (list 0.0 1.0 0.5 0.0))) "oboe.snd")
    (doc-1-6 "oboe.snd" 8.5 .5)
    (doc-1-7 (* 9 22050) 10000 2.0 "pistol.snd" 40000)
    (doc-1-9 9.5 .5 440 .1)
    (doc-2-0 10 .5 440 .1 1.0)
    (doc-2-1 10 1 "pistol.snd" .5)
    (doc-2-2  "oboe.snd" 11 '(0 1 1 .5 2 1) .1)
    (doc-2-3 12 .5 440 .1 110)
    (doc-2-4 "oboe.snd" 12)
    (doc-2-5 "oboe.snd" 12.5 1.0)
    (doc-2-6 13 .5 440 .1)
    (doc-2-7 13.5 1.0 "fyow" 1.0 3 8)
    (doc-2-8 (* 22050 14) 12000)
    (doc-2-9 (* 22050 15) 12000)
    (doc-3-0 (* 22050 16) 12000)
    (doc-3-1 (* 22050 17) 12000 2.0 1)
    (doc-3-2 14 .5 440 .1)
    (let ((oscs (make-fmins :carrier (make-oscil) :modulator (make-oscil))))
      (doc-3-3 0 1.01 440 .1 0.0 oscs)
      (doc-3-3 1.01 1 660 .1 0.0 oscs))
    (noisetone 14.5 .5 .1 '(0 0 1 1 2 0) 1000 100)
  ))


;;; --------------------------------------------------------------------------------
;;; simple filters

(when (setup-ins "resflt" "addflt" "zd")
  (with-sound (:play play-it)
    (resflt 0 1.0 0 0 0 nil .1 200 230 10 '(0 0 50 1 100 0) '(0 0 100 1) 500 .995 .1 1000 .995 .1 2000 .995 .1)
    (resflt 1 1.0 1 10000 .01 '(0 0 50 1 100 0) 0 0 0 0 nil nil 500 .995 .1 1000 .995 .1 2000 .995 .1)
    (addflts 2 1.0 2 2 '(0 0 100 1) 100 120 '(0 0 50 1 100 0) .25 30 0 0 1.0 0.01
	     '((0 0 100 1) (0 0 100 1) 1200 2400 .9 .995 .005)
	     '((0 0 100 1) (0 0 100 1) 600 1200 .95 .995 .01))
    (zc 3 3 100 .1 20 100 .5)
    (zc 3.5 3 100 .1 100 20 .95)))  


;;; --------------------------------------------------------------------------------
;;; pqw

(when (setup-ins "pqw" "pqwvox" "vox" "wavetrain")
  (with-sound (:play play-it)
    (pqw 0 .5 200 1000 .2 '(0 0 25 1 100 0) '(0 1 100 0) '(2 .1 3 .3 6 .5))
    (pqw-vox 1 2 100 414 .2 '(0 0 50 1 100 0) '(0 0 100 1) .01 '(0 OW 50 E 100 ER)
	     '(.8 .15 .05) '((1 1 2 .5 3 .1 4 .01) (1 1 4 .1) (1 1 2 .1 4 .05)) .01)
    (vox 3 2 110 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1
	 '(0 UH 25 UH 35 ER 65 ER 75 UH 100 UH) '(.8 .15 .05) '(.005 .0125 .025))
    (fofins 5 1 270 .2 .001 730 .6 1090 .3 2440 .1)
    (grain-simp-sound "oboe.snd" 6 10 :grain-interval .15 :grain-interval-env '(0 .01 1 1))))


;;; --------------------------------------------------------------------------------
;;; pins

(when (setup-ins "san")
  (with-sound (:play play-it)
    (pins 0 1 "oboe.snd" 1.0 :max-peaks 8)))


;;; --------------------------------------------------------------------------------
;;; expsrc

(when (setup-ins "expsrc")
  (with-sound (:play play-it)
    (expsrc 0 10 "fyow.snd" 6 1.5 .5)
    (expsrc1 6 10 "fyow.snd" 6 1.5 .5)
    (expsnd "fyow.snd" 10 3 1 '(0 1 1 3) nil nil '(0 2 1 .5) nil nil)
    (expsnd "oboe.snd" 12 3 1 '(0 1 1 3) nil nil '(0 2 1 .5) 0.2 nil)
    (expfil (* 22050 14) (* 22050 3) 2.0 500 10 "fyow.snd" "fyow.snd")
    (interleave-files 15 2 .2 .01 .1 "oboe.snd" "pistol.snd")))


;;; --------------------------------------------------------------------------------
;;; singer

(when (setup-ins "singer")
  (with-sound (:play play-it)
    (singer 0 .1 '((.4 ehh.shp test.glt 523.0 .8 0.0 .01) (.6 oo.shp test.glt 523.0 .7 .1 .01)))))


;;; --------------------------------------------------------------------------------
;;; formants

(when (setup-ins "fade" "tb")
  (with-sound (:play play-it)
    (cross-fade 0 4 1 "oboe" "trumpet" 1.0 .5 0 .1 256 2)
    (dissolve-fade 3 2 1 "fyow.snd" "fyow.snd" 1024 2)
    (formant-noise 5 .1 .1)))

  
;;; --------------------------------------------------------------------------------
;;; old arithmetic tests

(when (setup-ins "arith" "arith1" "arith2" "tnot" "btest")
  (with-sound (:play nil)
    (tnot)
    (arith)
    (arith1)
    (arith2)
    (btest)))


;;; --------------------------------------------------------------------------------
;;; old gen tests

(when (setup-ins "ug" "ug1")
  (with-sound (:play play-it :channels 2)
    (let ((dur 15000))
      (s1a 0 dur) (s2 (* dur 1) dur)
      (s4 (* dur 3) dur) (s5 (* dur 4) dur) (s6 (* dur 5) dur)
      (s7 (* dur 6) dur) (s8 (* dur 7) dur) (s9 (* dur 8) dur)
      (s10 (* dur 9) dur) (s11 (* dur 10) dur) (s12b (* dur 11) dur) (s12a (* dur 11) 0)
      (s13 (* dur 12) dur) (s14 (* dur 13) dur) (s15 (* dur 14) dur)
      (s16 (* dur 15) 100000)
      (echo (* dur 15) 60000 .5 1.0 (concatenate 'string "pistol.snd"))
      (f19 (* dur 16) dur)))
  (with-sound (:play play-it)
    (ha0 0 5000)
    (ha1 .5 .5))
  (with-sound (:play play-it :channels 2)
    (let ((dur 15000)) (ender)
	 (s22 0 dur) (s25 (* dur 1) dur) (s26 (* dur 2) dur)
	 (s27 (* dur 3) dur) (s28 (* dur 4) dur) (s29 (* dur 5) dur)
	 (s30 (* dur 6) dur)
	 (s34 (* dur 8) dur) (s36 (* dur 9) dur) 
	 (s38 (* dur 11) dur 440.0)
	 (arrclr)
	 (mapenv2 0 .001 440 .1 '(0 0 1 1))))
  (with-sound (:output "s8.snd" :channels 8 :play nil :srate 44100 :statistics t :verbose t)
    (loop for i from 0 to 7 do
      (simp8 (* i 10000) 40000 (* 200 (1+ i)) .05 i))
    (loop for i from 0 to 7 do
      (simp8 (* (- 7 i) 10000) 40000 300 .05 i)))
  (with-sound (:srate 44100  :play play-it)
    (loop for i from 0 to 7 do
      (simp8r (concatenate 'string "s8.snd") 0 120000 i 0))))


;;; --------------------------------------------------------------------------------
;;; mix

(defun max-amp (file)
  (let* ((chans (sound-chans file))
	 (vals (make-double-array chans))
	 (times (make-integer-array chans)))
    (sound-maxamp file chans vals times)
    (loop for i from 0 below chans maximize (abs (aref vals i)))))

(defun max-amps (file)
  (let* ((chans (sound-chans file))
	 (vals (make-double-array chans))
	 (times (make-integer-array chans)))
    (sound-maxamp file chans vals times)
    vals))

;; fullmix (in-file &optional (beg 0.0) outdur (inbeg 0.0) matrix srate reverb-amount)

(when (setup-ins "fullmix")
  (with-sound (:channels 2 :play play-it)
    (fullmix "pistol.snd")
    (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5)))))

  (let ((file (with-sound (:play nil)
			  (fullmix "oboe.snd"))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 1: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) (max-amp "oboe.snd")) (format t ";max fullmix 1: ~A ~A~%" (max-amp file) (max-amp "oboe.snd")))
    (if (not (= (sound-frames file) (sound-frames "oboe.snd"))) (format t ";frames fullmix 1: ~A ~A~%" (sound-frames file) (sound-frames "oboe.snd")))
    )
  
  (let ((file (with-sound (:play nil)
			  (fullmix "oboe.snd" 0.0 1.0 0.0 (list (list .5))))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 2: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) .07086) (format t ";max fullmix 2: ~A~%" (max-amp file)))
    (if (not (= (sound-frames file) *srate*)) (format t ";frames fullmix 2: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil)
			  (fullmix "oboe.snd" 0.0 1.0 0.0 (list (list .5)) 2.0))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 3: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) .0727) (format t ";max fullmix 3: ~A~%" (max-amp file)))
    (if (not (= (sound-frames file) *srate*)) (format t ";frames fullmix 3: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil :reverb jc-reverb)
			  (fullmix "oboe.snd" 0.0 1.0 0.0 (list (list .5)) 2.0 .1))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 4: ~A~%" (sound-chans file)))
    (if (not (= (sound-frames file) 44101)) (format t ";frames fullmix 4: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil)
			  (fullmix "oboe.snd")
			  (fullmix "pistol.snd" 1.0))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 5: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) .5134) (format t ";max fullmix 5: ~A~%" (max-amp file)))
    (if (not (= (sound-frames file) (+ *srate* (sound-frames "pistol.snd")))) (format t ";frames fullmix 5: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil)
			  (fullmix "oboe.snd")
			  (fm-violin 0.5 1.0 660 .1)
			  (fullmix "pistol.snd" 1.0))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 6: ~A~%" (sound-chans file)))
    (if (not (= (sound-frames file) (+ *srate* (sound-frames "pistol.snd")))) (format t ";frames fullmix 6: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (fullmix "oboe.snd" 1 2 0 (list (list .1 1.0))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 7: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (fneq (* 10 (aref mxs 0)) (aref mxs 1)) (format t ";maxs fullmix 7: ~A" mxs)))
    (if (not (= (sound-frames file) (* 3 *srate*))) (format t ";frames fullmix 7: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (fullmix "oboe.snd" 0 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5)))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 8: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) .0525) (format t ";max fullmix 8: ~A~%" (max-amp file)))
    (if (not (= (sound-frames file) (* 2 *srate*))) (format t ";frames fullmix 8: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (fullmix "oboe.snd" 0 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5)))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 9: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .0147)
	      (fneq (aref mxs 1) .053))
	  (format t ";maxs fullmix 9: ~A" mxs)))
    (if (not (= (sound-frames file) (* 2 *srate*))) (format t ";frames fullmix 9: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (fullmix "oboe.snd" 0 nil nil (list (list (make-env '(0 0 1 1 2 0) :duration 2.3) (make-env '(0 0 1 1) :duration 2.3)))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 10: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .147)
	      (fneq (aref mxs 1) .091))
	  (format t ";maxs fullmix 10: ~A" mxs)))
    (if (not (= (sound-frames file) (sound-frames "oboe.snd"))) (format t ";frames fullmix 10: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (mix-1 "oboe.snd" :srate 2 :matrix '((.5 .5))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans mix-1 11: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .0727)
	      (fneq (aref mxs 1) .0727))
	  (format t ";maxs fullmix 11: ~A" mxs)))
    (if (not (= (sound-frames file) (/ (sound-frames "oboe.snd") 2))) (format t ";frames fullmix 11: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil)
			  (mix-1 "oboe.snd" :srate 2 :matrix (list (list .25 (list 0 0 1 1)))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans mix-1 12: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .0363)
	      (fneq (aref mxs 1) .0897))
	  (format t ";maxs fullmix 12: ~A" mxs)))
    (if (not (= (sound-frames file) (/ (sound-frames "oboe.snd") 2))) (format t ";frames fullmix 12: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:channels 2 :play nil :reverb jc-reverb)
			  (mix-1 "oboe.snd" :srate 2 :reverb-amount .1 :matrix '((2.5 .5))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans mix-1 13: ~A~%" (sound-chans file))))
  
  (with-sound (:channels 2 :output "mixtest.snd" :play nil)
	      (fm-violin 0 1 440 .1 :degrees 0)
	      (fm-violin 0 1 440 .3 :degrees 90))
  
  (let ((file (with-sound (:play nil)
			  (fullmix "mixtest.snd"))))
    (if (not (= (sound-chans file) 1)) (format t ";chans fullmix 14: ~A~%" (sound-chans file)))
    (if (fneq (max-amp file) .1) (format t ";max fullmix 14: ~A~%" (max-amp file)))
    (if (not (= (sound-frames file) *srate*)) (format t ";frames fullmix 14: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil :channels 2)
			  (fullmix "mixtest.snd"))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 15: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .1)
	      (fneq (aref mxs 1) .3))
	  (format t ";max fullmix 15: ~A~%" mxs)))
    (if (not (= (sound-frames file) *srate*)) (format t ";frames fullmix 15: ~A~%" (sound-frames file)))
    )
  
  (let ((file (with-sound (:play nil :channels 2)
			  (fullmix "mixtest.snd" nil nil nil '((0 1.0) (1.0 0))))))
    (if (not (= (sound-chans file) 2)) (format t ";chans fullmix 16: ~A~%" (sound-chans file)))
    (let ((mxs (max-amps file)))
      (if (or (fneq (aref mxs 0) .3)
	      (fneq (aref mxs 1) .1))
	  (format t ";max fullmix 16: ~A~%" mxs)))
    (if (not (= (sound-frames file) *srate*)) (format t ";frames fullmix 16: ~A~%" (sound-frames file)))
    ))


;;; --------------------------------------------------------------------------------
;;; cnv

(when (setup-ins "cnv" "freeverb/freeverb.ins")
  (with-sound (:play play-it)
    (cnvo 0 1 '(.5 .2 .1 .05 0 0 0 0) "pistol.snd" 64))
  (with-sound (:play play-it :scaled-to .5 :srate 44100 :channels 2)
    (cnvrev "oboe.snd" "fyow.snd"))
  (with-sound (:reverb freeverb) (fm-violin 0 .1 440 .1)))


;;; --------------------------------------------------------------------------------
;;; leftovers

(load "spectr.clm")

(when (setup-ins "butterworth" "moog" "grani" "one-cut" "tc" "rmsp" "kiprev"
		 "addsnd" "zipper" "rmsp" "granular" "roomsig" "backandforth"
		 "ring-modulate" "ugex" "sndwarp" "vowel")
  (with-sound (:play play-it :channels 2)
    (backandforth 0 10 "pistol.snd" 2)
    (grani 3 1 .1 "oboe.snd")
    (one-cut 4 1 :sample-rate 3.2 :soundfile "pistol.snd")
    (sndwarp 4.5 .2 "oboe.snd" .2)
    (time-grain 100 '(0 .1 1 .01) 1)
    (let* ((ngrn 6) (last 0) (sum 0)
	   (frqns (make-env :envelope (list 0 520.0 1 520.0) :start 0 :end ngrn :base 1))
	   (posit (make-env :envelope '(0 .0 1 .0) :start 1 :end ngrn :base 1))
	   (frame (make-env :envelope '(0 .4 1 .4) :start 1 :end ngrn :base 1))
	   (hopev (make-env :envelope '(0 .2 1 .2) :start 1 :end ngrn :base 1))
	   (tline (sort (loop for i below ngrn collect (incf sum (env hopev))) '<)))
      (granular 6 ngrn (concatenate 'string "oboe.snd") :amp 1 :ampenv '(0 1 1 1) :maxsize .7
		:wnd1 '(0 0 .1 1 1 0) :wbase1 1 :wnd2 '(0 0 .1 1 1 0) :wbase2 25 :wndSiz 32
		:interp-env '(0 0 1 1) :base-intp 1 :Gs '(1) :dirct '(1) 
		:coord '(.2 .2 -.2 .9 -.2 .2 .2 .9)
		:frqns (loop for i below ngrn collect (env frqns))
		:posit (loop for i below ngrn collect (env posit))
		:frame (loop for i below ngrn collect (env frame))
		:hopev (loop for i in tline collect (- i last) do (setf last i))))
    (zipper 7 3 "oboe.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)
    (track-rms .5 "oboe.snd")
    (ring-modulate-file "oboe.snd" :freq-env '(0 440 25 440 26 220 50 220 51 3 75 5 76 100 100 0))
    (ring-modulate-file "oboe.snd" :start 2.0 :freq-env '(0 880 25 920 26 440 50 440 51 6 75 5 76 100 100 50))
    (add-sound "oboe.snd" 9)
    (gran-synth 9.5 2 100 .0189 .02 .4)
    (touch-tone-telephone '(7 2 3 4 9 7 1))
    (spectrum 10 1 440.0 .1 p-a4 '(0.0 0.0 1.0 1.0 5.0 0.9 12.0 0.5 25.0 0.25 100.0 0.0))
    (make-phoneme 11 1.0 150 0.5 'OO 'ER)
    (noi-attempt 11 2 .3 .99 1200)
    (filter-white-noise (* 22050 11) (+ (* 22050 11) 10000) .99 1000 1.0)    
    (testR (* 22050 12) (+ (* 22050 12) 10000) .9 .99 440 1.0)
    (vowel 12.5 .5 .3)
    ))


;;; --------------------------------------------------------------------------------
;;; with-mix etc

(with-sound (:reverb jc-reverb :decay-time 3  :play play-it)
  (fm-violin 0 .1 440 .1 :reverb-amount .75))

(with-sound (:reverb nrev  :play play-it :reverb-data (:reverb-factor 1.2 :lp-coeff .95 :amp-env (0 0 1 1 2 1 3 0))) 
  (fm-violin 0 .1 440 .1))

(with-sound (:output "tst.snd"  :play play-it
	     :statistics t :save-stats t
	     :header-type mus-next :data-format mus-bshort)
  (fm-violin 0 .1 440 .02 :base .03125))

(with-sound (:output "cv.snd" :play play-it)
  (fm-violin 0 1 440 .1 :amp-env '(0 1 100 1)))

(with-sound (:output "1.snd"  :play play-it
	     :header-type mus-aifc :data-format mus-bshort)
  (loop for i from 0 to 9 do
    (fm-violin i 1 (* (1+ i) 100) .1)))

(with-sound (:output "2.snd" :play play-it :channels 2 
	     :header-type mus-riff 
	     :data-format mus-lshort)
  (fm-violin 0 1 440 .1 :amp-env '(0 1 100 1) :degree 60))

(with-sound (:output "4.snd" :play nil :channels 4
	     :header-type mus-next :data-format mus-bshort)
  (fm-violin 0 1 440 .1 :amp-env '(0 1 100 1) :degree 220))

(with-sound (:output "8.snd" :play nil :channels 8)
  (fm-violin 0 1 440 .1 :amp-env '(0 1 100 1) :degree 220))

(with-sound (:play play-it)   
  (sound-let ((temp-1 () (fm-violin 0 1 440 .1))
	      (temp-2 () (fm-violin 0 2 660 .1 :base 32.0)
		         (fm-violin .125 .5 880 .1)))
    (mix temp-1) 
    (mix temp-2)))

(with-sound (:play play-it :channels 2)
  (let* ((snd1 "1.snd"))
    (mix snd1)
    (mix snd1 :frames (floor (* *srate* 1.0)))
    (mix snd1)
    (mix snd1 :frames (floor (* *srate* 1.0)) :output-frame (floor (* *srate* 1.5)) :input-frame (floor (* *srate* 3.0)))))

(with-sound (:force-recomputation t :play play-it)
  (with-mix () "sect1" 0 (fm-violin 0 1 440 .1))
  (with-mix () "sect3" 0 (fm-violin 0 1 770 .1))
  (with-mix () "sect2" 0 (fm-violin 0 1 660 .1)))

(with-sound (:play play-it)
  (fm-violin 0 .1 440 .1)
  (fm-violin 3 .1 660 .1))

(with-sound (:play play-it)
  (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
	                     (fm-violin 1 2 660 .1)))

(with-sound (:play play-it)
  (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
	                     (fm-violin 1 2 660 .1)))

(with-sound (:play play-it :reverb jc-reverb) 
  (with-mix () "section-3" 0 (fm-violin 0 1 440 .1 :reverb-amount .3)
	                     (fm-violin 1 2 660 .1)))

(with-sound (:play play-it :reverb jc-reverb) 
  (with-mix () "section-3" 0 (fm-violin 0 1 440 .1 :reverb-amount .3)
	                     (fm-violin 1 2 660 .1)))

(with-sound (:verbose t :play play-it :channels 2)
  (with-mix (:channels 2) "sect1" 0 (fm-violin 0 1 440 .1))
  (with-mix (:channels 2) "sect3" 0 (with-offset 1.0 (fm-violin 0 1 770 .1)))
  (with-mix (:channels 2) "sect2" 0 (fm-violin 0 1 660 .1)))

(with-sound (:verbose t :play play-it :channels 2)
  (fm-violin 0 1 440 .1)
  (with-offset 1.0
    (with-mix (:channels 2 :reverb jc-reverb) "sect1" 0
       (fm-violin 0 .1 880 .1 :reverb-amount .1))
    (with-mix (:channels 2) "sect3" 0
      (with-offset 1.0
        (fm-violin 0 1 770 .1))))
  (with-mix (:channels 2) "sect2" 0 (fm-violin 0 1 660 .1)))

(with-sound (:verbose t :play play-it :channels 2)
  (fm-violin 0 1 440 .1)
  (with-offset 1.0
    (with-mix (:channels 2 :reverb nrev :reverb-data (:reverb-factor 1.2 :lp-coeff .95 :amp-env (0 0 1 1 2 1 3 0)))
	      "sect1" 0
      (fm-violin 0 .1 880 .1 :reverb-amount .1))
    (with-mix (:channels 2) "sect3" 0 (with-offset 1.0 (fm-violin 0 1 770 .1))))
  (with-mix (:channels 2) "sect2" 0 (fm-violin 0 1 660 .1)))

(with-open-file 
    (file "clm-test.clm" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format file "(fm-violin 0 .1 440 .1)~%"))
(clm-load "clm-test.clm" :scaled-to .3 :statistics t :play play-it )
(delete-file "clm-test.clm")

(with-sound (:verbose t :play play-it)
  (mix (with-sound (:output "hiho.snd")
 	 (fm-violin 0 1 440 .1))))

(with-sound (:verbose t :play play-it)
  (with-mix () "s1" 0
    (sound-let ((tmp () (fm-violin 0 1 440 .1)))
      (mix tmp))))

(with-sound (:play play-it :verbose t)
  (with-mix () "s2" 0
    (sound-let ((tmp () (fm-violin 0 1 440 .1))
		(tmp1 () (mix (concatenate 'string "oboe.snd"))))
      (mix tmp1)
      (mix tmp :output-frame *srate*))
    (fm-violin .5 .1 330 .1)))
	
(with-sound (:play play-it :verbose t)
  (sound-let ((tmp ()
		(with-mix () "s3" 0
		  (sound-let ((tmp () (fm-violin 0 1 440 .1))
			      (tmp1 () (mix "oboe.snd")))
  		    (mix tmp1)
		    (mix tmp :output-frame *srate*))
		  (fm-violin .5 .1 330 .1))))
	     (mix tmp)))


;;; --------------------------------------------------------------------------------
;;; physical modeling part 2

(when (setup-ins "piano" "strad" "scanned")
  (with-sound (:play play-it)
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keyNum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
  )))

  (with-sound (:play play-it)
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keyNum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modification to do detunedness
     :detuningFactor-table '(24 5 36 7.0 48 7.5 60 12.0 72 20
				84 30 96 100 108 300)
					;scales the above detuning values
					;  so 1.0 is nominal detuning
					;  0.0 is exactly in tune (no two stage decay...)
					;  > 1.0 is out of tune...
     
     ;;modification to do stiffness
     :stiffnessFactor-table '(21 1.5 24 1.5 36 1.5 48 1.5 60 1.4
				 72 1.3 84 1.2 96 1.0 108 1.0)
					;0.0 to 1.0 is less stiff, 1.0 to 2.0 is more stiff...
     )))

  (with-sound (:play play-it)
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keyNum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modifications to do damped sounds
     :singleStringDecayRate-table '(21 -5 24.000 -5.000 36.000 -5.4
				       41.953 -5.867 48.173 -7.113 53.818 -8.016
				       59.693 -8.875 66.605 -9.434 73.056 -10.035
				       78.931 -10.293 84.000 -12.185)
     :singleStringPole-table '(21 .8 24 0.7  36.000 .6 48 .5 60 .3
				  84 .1 96 .03 108 .03)
     :stiffnessCoefficient-table '(21.000 -0.920 24.000 -0.900 36.000 -0.700
					  48.000 -0.250 60.000 -0.100 75.179 -0.040
					  82.986 -0.040 92.240 .3 96.000 .5
					  99.000 .7 108.000 .7)
					;these are the actual allpass coefficients modified here
					;to allow dampedness at hig freqs
     )))

  (with-sound (:play play-it)
  (loop for i from 5 to 5 do
    (p
     0
     :duration 10
     :keyNum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modification for long duration notes
     :singleStringDecayRateFactor 1/10
					;scales attenuation rate (1/2 means twice as long duration)
     )))

  (with-sound (:play play-it)
    (bow 0 3 400 0.5 :vb 0.15 :fb 0.1 :inharm 0.25)
    (scanned 2 1 450 .01))    
  )
  
  

;;; --------------------------------------------------------------------------------

(when include-notelists
  (load "fmviolin.clm")
  (load "singer.clm")
  (load "cream.clm")
  (load "popi.clm")
  (load "clm-example.lisp")
  )

