;;; -*- syntax: common-lisp; package: clm; base: 10; mode: lisp -*-
;;;
;;; this file contains most of the global default values that control CLM
;;; in many cases the *clm-<>* form is *<>* during a given run; that is,
;;;    *clm-channels* is the default, *channels* is the current setting

(in-package :clm)

(defvar *clm-srate* 22050)
(defvar *clm-channels* 1)

(defvar *clm-file-buffer-size* (* 64 1024))

(defvar *clm-file-name*
  #+ccrma "/zap/test.snd"
  #+(or sgi sun hpux linux) "test.snd"
  #+windoze "test.wav"
  #-(or windoze ccrma sgi sun hpux linux) "test.aiff"
  )

(defvar *clm-header-type*
  #+(or sun hpux linux) mus-next
  #+(or openmcl sgi) mus-aifc
  #+windoze mus-riff
  #-(or windoze openmcl sgi sun hpux linux) mus-aifc
  )

(defvar *clm-data-format*
  #-windoze mus-bshort
  #+windoze mus-lshort
  )

(defvar *clm-verbose* nil)		; will cause instrument names and so on to be printed out
(defvar *clm-play* t)	                ; default for play arg in with-sound
(defvar *clm-player* nil)	        ; user-supplied DAC function
(defvar *clm-table-size* 512)      	; used for table-lookup tables and others
(defvar *clm-safety* 0)			; safety setting in Run (1: check gen, 2:check array indices)
(defvar *clm-array-print-length* 10)	; how much of an array to print out during debugging
(defvar *clm-init* nil)			; name of init file used during load process
(defvar *clm-search-list* (list ""))    ; directories to search in open-input*
(defvar *clm-notehook* nil)             ; default notehook
(defvar *clm-clipped* t)                ; is out-going data clipped or wrapped-around (the latter can cause arithmetic exceptions)
(defvar *clm-delete-reverb* nil)        ; whether with-sound should delete the reverb stream
(defvar *clm-reverb-channels* 1)        ; channels in reverb stream
(defvar *clm-statistics* nil)           ; statistics arg in with-sound
