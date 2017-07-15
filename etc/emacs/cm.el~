;;; **********************************************************************
;;; Copyright (C) 2006 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

;;;
;;; Emacs/Slime support for Common Music. Most commands are not bound
;;; to keys, see entries for some suggestions. CM's documentation menu
;;; is installed under the SLIME -> Documentation submenu.
;;;
;;; M-x cm                                                       [Command]
;;;   Start up cm in a new frame, window, or buffer (see
;;;   cm-command and inferior-lisp-display below).
;;; M-x kill-cm                                                  [Command]
;;;   Kill existing *slime-repl* session.
;;; M-x enable-cm-commands                                       [Command]
;;;   Adds the following keyboard comands:
;;;   <f8>      One-stroke switching between repl and lisp buffer.
;;;   C-cC-dc   Find symbol at point in Common Music dictionary. 
;;;             (also in menu SLIME->Documentation->Common Music)
;;;   C-xC-e    Eval expr before, after, around or whole region.
;;;             On OS X this is also installed on APPLE-E.
;;;   TAB       Indent line, region or defun (if prefixed).
;;;
;;; *common-music-doc-root*                                     [Variable]
;;;   The root URL for browsing CM documentation. Defaults
;;;   to "http://commonmusic.sf.net/doc/"
;;; cm-program
;;;   The shell program to start CM. Defaults to "cm".
;;; cm-systems
;;;   A list of systems to load when CM starts.
;;; cm-scratch-mode
;;;   Emacs edit mode for *scratch* buffer, one of: lisp, sal or nil.

(unless (member 'slime features)
  (require 'slime)
  (slime-setup))

(when (member 'aquamacs features)
  (add-to-list 'obof-other-frame-regexps " \\*inferior-lisp\\*")
  (add-to-list 'obof-other-frame-regexps "\\*slime-repl\\\\*"))

;; update default value of inferior-lisp-program to "cm.sh"

(defvar cm-program
  (if (or (not (boundp 'inferior-lisp-program))
	  (not inferior-lisp-program)
	  (equal inferior-lisp-program "lisp"))
      (or (locate-library "bin/cm.sh" t)
	  "cm")))

(defvar cm-systems (list))

(defvar cm-scratch-mode 'lisp)

;; add music extensions if not already present...
(loop for mode in '(("\\.clm$" . lisp-mode)
		    ("\\.cm$"  . lisp-mode)
		    ("\\.cmn$" . lisp-mode)
		    ("\\.ins$" . lisp-mode)
		    ("\\.fms$" . lisp-mode)
		    ("\\.asd$" . lisp-mode))
      do (add-to-list 'auto-mode-alist mode))

;; add music-related ignored file types...
(loop for type in '(".midi" ".mid" ".snd" ".aiff" ".wav" ".osc"
		    ".fas" ".dfas" ".fasl" ".lib" ".ppcf" ".so"
		    ".dylib")
      do (add-to-list 'completion-ignored-extensions type))

;; set lisp buffers to slime mode...
(add-hook 'inferior-lisp-mode-hook
            (lambda ()
	      (slime-mode t)
	      (setq indent-tabs-mode nil)
	      ))

;; connect hook executes (cm) to set readtable etc and then removes
;; itself so that it doesnt interfere with other slime sessions.

(defun cm-start-hook ()
  ;;(slime-repl-send-string "(cm)")
  (slime-interactive-eval "(cm)")
  (remove-hook 'slime-connected-hook 'cm-start-hook)
  ;; aquamacs: hide inferior lisp buffer if visible after slime buffer
  ;; starts. This happens if user re-mouses original frame after doing
  ;; M-x cm before slime repl has been activated. if user then closes
  ;; the visible inferior-lisp frame the lisp session is hosed.
  ;; (when (member 'aquamacs features)
  ;;    (replace-buffer-in-windows (get-buffer "*inferior-lisp*")))
  (when (member 'aquamacs features)
    (let ((ilw (get-buffer-window " *inferior-lisp*" t)))
      (if ilw (delete-frame (window-frame ilw))))
    ))

;; Darwin: define COMMAND-E to evaluate expr a la MCL.
(if (equal system-type 'darwin)
    (global-set-key [(alt e)] 'slime-eval-expr))

;; add cm startup actions to inferior-lisp startup BEFORE repl has
;; been established

(defun cm-init-command (port coding)
  ;; get slime's inits 
  (let ((init (slime-init-command port coding)))
    ;; append system loading before repl bufer starts
    (dolist (s cm-systems)
      (setq init
	    (concat init (if (keywordp s)
			     (format "(use-system %s)\n" s)
			   (format "(use-system :%s)\n" s)))))
    init))

(defun cm (program )
  "Start CM"
  (interactive (list (if prefix-arg
			 (read-string "Command to start CM: " "cm") 
		       nil)))
  (cond ((slime-connected-p)
	 (switch-to-buffer (slime-repl-buffer)))
	(t
	 (when program (setq cm-program program))
	 (let ((parsed (split-string cm-program)))
	   (add-hook 'slime-connected-hook 'cm-start-hook)
	   (slime-start :program (first parsed) :program-args (rest parsed)
			:init 'cm-init-command
			:buffer " *inferior-lisp*"
			)
	   (claim-scratch-buffer)))))

(defun kill-cm ()
  "Kill *slime-repl* and all associated buffers."
  (interactive)
  (slime-repl-sayoonara))

(defun enable-cm-commands ()
  (interactive )
  ;; 1 stroke switching between repl and last editing buffer
  (global-set-key (kbd "<f8>") 'slime-toggle-repl)
  ;; eval before at or after point, region, or whole defun on whitespae
  (define-key slime-mode-map (kbd "\C-x\C-e") 'slime-eval-last-expression)
  ;; indent line or region
  (define-key slime-mode-map (kbd "TAB") 'slime-indent-anything)
  ;; lookup cm function at point
  (define-key slime-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
  )

(defun slime-toggle-repl ()
  "Toggle between *slime-repl* and last lisp or SAL buffer."
  (interactive)
  (if (slime-connected-p)
      (let ((repl (slime-repl-buffer)))
        (if repl
            (let ((this (current-buffer))
		  next)
              (if (eq repl this)
                  (setq next (loop for b in (buffer-list)
				   when (with-current-buffer b
					  (or (eq major-mode 'lisp-mode)
					      (eq major-mode 'sal-mode)
					      ))
				   return b))
		(setq next (slime-repl-buffer)))
	      (when next
		;;(pop-to-buffer next)
		;;(switch-to-buffer-other-frame next)
		(switch-to-buffer next)))))))

(defun claim-scratch-buffer ()
  ;; if scratch buffer is empty set to slime or SAL mode
  (let ((scratch (get-buffer "*scratch*")))
    (if scratch
	(if (not (buffer-modified-p scratch))
	    (with-current-buffer scratch
	      (cond ((equal cm-scratch-mode 'lisp)
		     (lisp-mode)
		     (setq slime-buffer-package "cm")
		     (insert (format "(in-package :cm)\n\n"))
		     (goto-char (point-max)))
		    ((equal cm-scratch-mode 'sal)
		     (sal-mode)
		     (insert (format "; Use this buffer for SAL commands.\n\n"))
		     (goto-char (point-max)))
		    (t )))))))

(when (not (featurep 'xemacs))
  (defun region-exists-p ()
    (and mark-active ; simple.el
	 (not (null (mark))))))

(defun slime-eval-expr ()
  "Evals expr before point, at point, around point, whole region."
  (interactive)
  (if (region-exists-p )
      (slime-eval-region (region-beginning) (region-end))
    (let ((wspace '(?\  ?\t ?\r ?\n))
	  (left-char (char-before))
	  (right-char (char-after))
	  left-side right-side)
      (setq left-side
	    (if (or (not left-char)
		    (member left-char wspace)
		    (member left-char '(?\( )))
		(point)
	      (save-excursion
		(backward-sexp)
		(point))))
      (setq right-side
	    (if (or (not right-char)
		    (member right-char wspace)
		    (member right-char '(?\) ))
		    ;; dont look ahead if different sexp leftward
		    (and (< left-side (point))
			 (char-equal left-char ?\))))
		(point)
	      (save-excursion
		(forward-sexp)
		(point))))
      (if (equal left-side right-side)   
	  nil
	(slime-interactive-eval
	 (buffer-substring-no-properties left-side right-side))))))

(defun slime-indent-anything ()
  "Do line indentation/symbol completion; indent region if
selected; indent whole defun if prefixed."
  (interactive)
  (if current-prefix-arg
      (slime-reindent-defun )
    (if (and (region-exists-p)
	     (> (count-lines (region-beginning) (region-end)) 1))
	(lisp-indent-region (region-beginning) (region-end))
      (slime-indent-and-complete-symbol))))

;;;
;;; CM documentation hacks, mostly cribbed from hyperspec.
;;;

(defvar *common-music-doc-root* "file:///home/orm/work/programmieren/lisp/cm-incudine/doc/"
  "The root url for visiting CM documentation.")

(defun cm-doc (url)
  (interactive "FCM document:")
  (browse-url-firefox
   (concatenate 'string *common-music-doc-root*
                url)
   nil))

(defun cm-lookup (entry)
  (interactive (list
		(let* ((it (thing-at-point 'symbol))
		       (sy (and it (downcase it))))
		  (if (and sy (intern-soft sy *common-music-symbols*))
		      sy
		    (completing-read "Lookup CM symbol: "
				     *common-music-symbols*
				     #'boundp t nil nil nil)))))
  (if entry
      (let ((sym (intern-soft (downcase entry) *common-music-symbols*)))
	(if (and sym (boundp sym))
	    (cm-doc (car (symbol-value sym)))))))

(defvar *common-music-doc-menu*
  `("Common Music"
    [ "Home Page" (cm-doc "cm.html")]
    [ "Installing" (cm-doc "install.html")]
    [ "Working in Emacs" (cm-doc "emacs.html")]
    "--"
    [ "Tutorials" (cm-doc "../etc/tutorials/")]
    [ "Examples"  (cm-doc "../etc/examples/")]
    "--"
    [ "Dictionary" (cm-doc "dict/index.html") ]
    [ "Lookup..." cm-lookup ]
    ))

;; add Common Music documentation menu to Slime...
;; last argument changed to make it workable under xemacs 21.4.19
;; Robert Matovinovi,c 20.07.2006, robert.matovinovic@web.de

(easy-menu-add-item menubar-slime
		    '("Documentation")
		    *common-music-doc-menu*
		    ;(easy-menu-create-menu "Common Music" )
		    )

(defvar *common-music-symbols* (make-vector 66 0))

(mapcar
 (lambda (entry)
   (let ((symbol (intern (car entry)
			 *common-music-symbols*)))
     (if (boundp symbol)
	 (push (cadr entry) (symbol-value symbol))
       (set symbol (cdr entry)))))
 ;; *** generate by loading "/Lisp/cm/doc/dict/index.lisp"
 '(
; warning: /Lisp/cm/doc/dict/index.html#osc-stream-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/dict/index.html#sc-file-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/dict/index.html#sc-stream-cls.html has no <title> tag.
   ("*beat*" "dict/index.html#beat-var.html")
   ("*chromatic-scale*" "dict/index.html#chromatic-scale-var.html")
   ("*loudest*" "dict/index.html#loudest-var.html")
   ("*midi-channel-map*" "dict/index.html#midi-channel-map-var.html")
   ("*midi-connections*" "dict/index.html#midi-connections-var.html")
   ("*portmidi-default-filter*" "dict/index.html#portmidi-topic.html#*portmidi-default-filter*")
   ("*portmidi-default-inbuf-size*" "dict/index.html#portmidi-topic.html#*portmidi-default-inbuf-size*")
   ("*portmidi-default-input*" "dict/index.html#portmidi-topic.html#*portmidi-default-input*")
   ("*portmidi-default-latency*" "dict/index.html#portmidi-topic.html#*portmidi-default-latency*")
   ("*portmidi-default-mask*" "dict/index.html#portmidi-topic.html#*portmidi-default-mask*")
   ("*portmidi-default-outbuf-size*" "dict/index.html#portmidi-topic.html#*portmidi-default-outbuf-size*")
   ("*portmidi-default-output*" "dict/index.html#portmidi-topic.html#*portmidi-default-output*")
   ("*power*" "dict/index.html#power-var.html")
   ("*scale*" "dict/index.html#scale-var.html")
   ("*softest*" "dict/index.html#softest-var.html")
   ("*tempo*" "dict/index.html#tempo-var.html")
   ("accumulation" "dict/index.html#accumulation-cls.html")
   ("active-sensing-p" "dict/index.html#midi-topic.html#active-sensing-p")
   ("active-sensing-route" "dict/index.html#midi-topic.html#active-sensing-route")
   ("amplitude" "dict/index.html#amplitude-fn.html")
   ("append-object" "dict/index.html#append-object-fn.html")
   ("audio-file" "dict/index.html#audio-file-cls.html")
   ("axis" "dict/index.html#axis-fn.html")
   ("axis" "dict/index.html#axis-cls.html")
   ("between" "dict/index.html#between-fn.html")
   ("cable-select-cable" "dict/index.html#midi-topic.html#cable-select-cable")
   ("cable-select-p" "dict/index.html#midi-topic.html#cable-select-p")
   ("cable-select-route" "dict/index.html#midi-topic.html#cable-select-route")
   ("cd" "dict/index.html#cd-fn.html")
   ("cents->scaler" "dict/index.html#cents-gtscaler-fn.html")
   ("channel-message-channel" "dict/index.html#midi-topic.html#channel-message-channel")
   ("channel-message-data1" "dict/index.html#midi-topic.html#channel-message-data1")
   ("channel-message-data2" "dict/index.html#midi-topic.html#channel-message-data2")
   ("channel-message-opcode" "dict/index.html#midi-topic.html#channel-message-opcode")
   ("channel-message-p" "dict/index.html#midi-topic.html#channel-message-p")
   ("channel-pressure-channel" "dict/index.html#midi-topic.html#channel-pressure-channel")
   ("channel-pressure-p" "dict/index.html#midi-topic.html#channel-pressure-p")
   ("channel-pressure-pressure" "dict/index.html#midi-topic.html#channel-pressure-pressure")
   ("chord" "dict/index.html#chord-cls.html")
   ("clm-file" "dict/index.html#clm-file-cls.html")
   ("cm-version" "dict/index.html#cm-version-fn.html")
   ("cm.sh" "dict/index.html#cm-sh.html")
   ("cmio" "dict/index.html#cmio-fn.html")
   ("cmn" "dict/index.html#cmn-cls.html")
   ("cmn-file" "dict/index.html#cmn-file-cls.html")
   ("continue-p" "dict/index.html#midi-topic.html#continue-p")
   ("continue-route" "dict/index.html#midi-topic.html#continue-route")
   ("control-change-channel" "dict/index.html#midi-topic.html#control-change-channel")
   ("control-change-controller" "dict/index.html#midi-topic.html#control-change-controller")
   ("control-change-p" "dict/index.html#midi-topic.html#control-change-p")
   ("control-change-value" "dict/index.html#midi-topic.html#control-change-value")
   ("copier" "dict/index.html#copier-cls.html")
   ("copy-object" "dict/index.html#copy-object-fn.html")
   ("copyright-note-p" "dict/index.html#midi-topic.html#copyright-note-p")
   ("cue-point-p" "dict/index.html#midi-topic.html#cue-point-p")
   ("cycle" "dict/index.html#cycle-cls.html")
   ("date-and-time" "dict/index.html#date-and-time-fn.html")
   ("decimals" "dict/index.html#decimals-fn.html")
   ("decode-interval" "dict/index.html#decode-interval-fn.html")
   ("defaxis" "dict/index.html#defaxis-mac.html")
   ("defobject" "dict/index.html#defobject-mac.html")
   ("doeach" "dict/index.html#doeach-mac.html")
   ("drunk" "dict/index.html#drunk-fn.html")
   ("dumposc" "dict/index.html#dumposc-fn.html")
   ("eod?" "dict/index.html#eodqmk-fn.html")
   ("eop?" "dict/index.html#eopqmk-fn.html")
   ("eot-p" "dict/index.html#midi-topic.html#eot-p")
   ("eox-p" "dict/index.html#midi-topic.html#eox-p")
   ("eox-route" "dict/index.html#midi-topic.html#eox-route")
   ("events" "dict/index.html#events-fn.html")
   ("expl" "dict/index.html#expl-fn.html")
   ("explseg" "dict/index.html#explseg-fn.html")
   ("explsegs" "dict/index.html#explsegs-fn.html")
   ("f" "dict/index.html#f-cls.html")
   ("false" "dict/index.html#false-var.html")
   ("find-object" "dict/index.html#find-object-fn.html")
   ("fit" "dict/index.html#fit-fn.html")
   ("fm-spectrum" "dict/index.html#fm-spectrum-fn.html")
   ("fold-objects" "dict/index.html#fold-objects-fn.html")
   ("fomus-file" "dict/index.html#fomus-file-cls.html")
   ("graph" "dict/index.html#graph-cls.html")
   ("harmonics" "dict/index.html#harmonics-fn.html")
   ("heap" "dict/index.html#heap-cls.html")
   ("hertz" "dict/index.html#hertz-fn.html")
   ("histogram" "dict/index.html#histogram-fn.html")
   ("i" "dict/index.html#i-cls.html")
   ("import-events" "dict/index.html#import-events-fn.html")
   ("input" "dict/index.html#input-fn.html")
   ("insert-object" "dict/index.html#insert-object-fn.html")
   ("instrument-name-p" "dict/index.html#midi-topic.html#instrument-name-p")
   ("interp" "dict/index.html#interp-fn.html")
   ("interpl" "dict/index.html#interpl-fn.html")
   ("interval" "dict/index.html#interval-fn.html")
   ("invert" "dict/index.html#invert-fn.html")
   ("io" "dict/index.html#io-mac.html")
   ("join" "dict/index.html#join-cls.html")
   ("key-pressure-channel" "dict/index.html#midi-topic.html#key-pressure-channel")
   ("key-pressure-key" "dict/index.html#midi-topic.html#key-pressure-key")
   ("key-pressure-p" "dict/index.html#midi-topic.html#key-pressure-p")
   ("key-pressure-pressure" "dict/index.html#midi-topic.html#key-pressure-pressure")
   ("key-signature-p" "dict/index.html#midi-topic.html#key-signature-p")
   ("keynum" "dict/index.html#keynum-fn.html")
   ("line" "dict/index.html#line-cls.html")
   ("list-named-objects" "dict/index.html#list-named-objects-fn.html")
   ("list-objects" "dict/index.html#list-objects-fn.html")
   ("log-axis" "dict/index.html#log-axis-cls.html")
   ("lookup" "dict/index.html#lookup-fn.html")
   ("lyric-p" "dict/index.html#midi-topic.html#lyric-p")
   ("make-active-sensing" "dict/index.html#midi-topic.html#make-active-sensing")
   ("make-cable-select" "dict/index.html#midi-topic.html#make-cable-select")
   ("make-channel-message" "dict/index.html#midi-topic.html#make-channel-message")
   ("make-channel-pressure" "dict/index.html#midi-topic.html#make-channel-pressure")
   ("make-cm" "dict/index.html#make-cm-fn.html")
   ("make-continue" "dict/index.html#midi-topic.html#make-continue")
   ("make-control-change" "dict/index.html#midi-topic.html#make-control-change")
   ("make-copyright-note" "dict/index.html#midi-topic.html#make-copyright-note")
   ("make-cue-point" "dict/index.html#midi-topic.html#make-cue-point")
   ("make-eot" "dict/index.html#midi-topic.html#make-eot")
   ("make-eox" "dict/index.html#midi-topic.html#make-eox")
   ("make-instrument-name" "dict/index.html#midi-topic.html#make-instrument-name")
   ("make-key-pressure" "dict/index.html#midi-topic.html#make-key-pressure")
   ("make-key-signature" "dict/index.html#midi-topic.html#make-key-signature")
   ("make-lyric" "dict/index.html#midi-topic.html#make-lyric")
   ("make-marker" "dict/index.html#midi-topic.html#make-marker")
   ("make-meta-message" "dict/index.html#midi-topic.html#make-meta-message")
   ("make-midi-channel" "dict/index.html#midi-topic.html#make-midi-channel")
   ("make-midi-port" "dict/index.html#midi-topic.html#make-midi-port")
   ("make-mtc-quarter-frame" "dict/index.html#midi-topic.html#make-mtc-quarter-frame")
   ("make-note-off" "dict/index.html#midi-topic.html#make-note-off")
   ("make-note-on" "dict/index.html#midi-topic.html#make-note-on")
   ("make-pitch-bend" "dict/index.html#midi-topic.html#make-pitch-bend")
   ("make-program-change" "dict/index.html#midi-topic.html#make-program-change")
   ("make-sequence-number" "dict/index.html#midi-topic.html#make-sequence-number")
   ("make-sequencer-event" "dict/index.html#midi-topic.html#make-sequencer-event")
   ("make-sequence_track-name" "dict/index.html#midi-topic.html#make-sequence_track-name")
   ("make-smpte-offset" "dict/index.html#midi-topic.html#make-smpte-offset")
   ("make-song-position" "dict/index.html#midi-topic.html#make-song-position")
   ("make-song-select" "dict/index.html#midi-topic.html#make-song-select")
   ("make-start" "dict/index.html#midi-topic.html#make-start")
   ("make-stop" "dict/index.html#midi-topic.html#make-stop")
   ("make-sysex" "dict/index.html#midi-topic.html#make-sysex")
   ("make-system-message" "dict/index.html#midi-topic.html#make-system-message")
   ("make-system-reset" "dict/index.html#midi-topic.html#make-system-reset")
   ("make-tempo-change" "dict/index.html#midi-topic.html#make-tempo-change")
   ("make-text-event" "dict/index.html#midi-topic.html#make-text-event")
   ("make-time-signature" "dict/index.html#midi-topic.html#make-time-signature")
   ("make-timing-clock" "dict/index.html#midi-topic.html#make-timing-clock")
   ("make-timing-tick" "dict/index.html#midi-topic.html#make-timing-tick")
   ("make-tune-request" "dict/index.html#midi-topic.html#make-tune-request")
   ("map-objects" "dict/index.html#map-objects-fn.html")
   ("map-pattern-data" "dict/index.html#map-pattern-data-fn.html")
   ("map-subcontainers" "dict/index.html#map-subcontainers-fn.html")
   ("map-subobjects" "dict/index.html#map-subobjects-fn.html")
   ("marker-p" "dict/index.html#midi-topic.html#marker-p")
   ("markov" "dict/index.html#markov-cls.html")
   ("markov-analyze" "dict/index.html#markov-analyze-fn.html")
   ("meta-message-p" "dict/index.html#midi-topic.html#meta-message-p")
   ("meta-message-type" "dict/index.html#midi-topic.html#meta-message-type")
   ("midi" "dict/index.html#midi-cls.html")
   ("midi-chan-event" "dict/index.html#midi-chan-event-cls.html")
   ("midi-channel-p" "dict/index.html#midi-topic.html#midi-channel-p")
   ("midi-channel-pressure" "dict/index.html#midi-channel-pressure-cls.html")
   ("midi-control-change" "dict/index.html#midi-control-change-cls.html")
   ("midi-copy-message" "dict/index.html#midi-topic.html#midi-copy-message")
   ("midi-eot" "dict/index.html#midi-eot-cls.html")
   ("midi-file" "dict/index.html#midi-file-cls.html")
   ("midi-file-print" "dict/index.html#midi-file-print-fn.html")
   ("midi-key-pressure" "dict/index.html#midi-key-pressure-cls.html")
   ("midi-key-signature" "dict/index.html#midi-key-signature-cls.html")
   ("midi-note-off" "dict/index.html#midi-note-off-cls.html")
   ("midi-note-on" "dict/index.html#midi-note-on-cls.html")
   ("midi-pitch-bend" "dict/index.html#midi-pitch-bend-cls.html")
   ("midi-port-event" "dict/index.html#midi-port-event-cls.html")
   ("midi-port-p" "dict/index.html#midi-topic.html#midi-port-p")
   ("midi-print-message" "dict/index.html#midi-topic.html#midi-print-message")
   ("midi-program-change" "dict/index.html#midi-program-change-cls.html")
   ("midi-sequence-number" "dict/index.html#midi-sequence-number-cls.html")
   ("midi-sequencer-event" "dict/index.html#midi-sequencer-event-cls.html")
   ("midi-smpte-offset" "dict/index.html#midi-smpte-offset-cls.html")
   ("midi-stream" "dict/index.html#midi-stream-cls.html")
   ("midi-system-event" "dict/index.html#midi-system-event-cls.html")
   ("midi-tempo-change" "dict/index.html#midi-tempo-change-cls.html")
   ("midi-text-event" "dict/index.html#midi-text-event-cls.html")
   ("midi-time-signature" "dict/index.html#midi-time-signature-cls.html")
   ("midishare-open" "dict/index.html#midishare-topic.html#midishare-open")
   ("midishare-open?" "dict/index.html#midishare-topic.html#midishare-open?")
   ("mode" "dict/index.html#mode-cls.html")
   ("ms:midiprintev" "dict/index.html#midishare-topic.html#ms:midiprintev")
   ("ms:new" "dict/index.html#midishare-topic.html#ms:new")
   ("mtc-quarter-frame-nibble" "dict/index.html#midi-topic.html#mtc-quarter-frame-nibble")
   ("mtc-quarter-frame-p" "dict/index.html#midi-topic.html#mtc-quarter-frame-p")
   ("mtc-quarter-frame-route" "dict/index.html#midi-topic.html#mtc-quarter-frame-route")
   ("mtc-quarter-frame-tag" "dict/index.html#midi-topic.html#mtc-quarter-frame-tag")
   ("new" "dict/index.html#new-mac.html")
   ("next" "dict/index.html#next-fn.html")
   ("note" "dict/index.html#note-fn.html")
   ("note-accidental" "dict/index.html#note-accidental-fn.html")
   ("note-name" "dict/index.html#note-name-fn.html")
   ("note-off-channel" "dict/index.html#midi-topic.html#note-off-channel")
   ("note-off-key" "dict/index.html#midi-topic.html#note-off-key")
   ("note-off-p" "dict/index.html#midi-topic.html#note-off-p")
   ("note-off-velocity" "dict/index.html#midi-topic.html#note-off-velocity")
   ("note-on-channel" "dict/index.html#midi-topic.html#note-on-channel")
   ("note-on-key" "dict/index.html#midi-topic.html#note-on-key")
   ("note-on-p" "dict/index.html#midi-topic.html#note-on-p")
   ("note-on-velocity" "dict/index.html#midi-topic.html#note-on-velocity")
   ("now" "dict/index.html#now-fn.html")
   ("object->cmn" "dict/index.html#object-gtcmn-fn.html")
   ("object-name" "dict/index.html#object-name-fn.html")
   ("object-parameters" "dict/index.html#object-parameters-fn.html")
   ("object-time" "dict/index.html#object-time-fn.html")
   ("octave-number" "dict/index.html#octave-number-fn.html")
   ("odds" "dict/index.html#odds-fn.html")
   ("output" "dict/index.html#output-fn.html")
   ("palindrome" "dict/index.html#palindrome-cls.html")
   ("pattern-state" "dict/index.html#pattern-state-fn.html")
   ("pattern-value" "dict/index.html#pattern-value-fn.html")
   ("pattern?" "dict/index.html#patternqmk-fn.html")
   ("pick" "dict/index.html#pick-fn.html")
   ("pickl" "dict/index.html#pickl-fn.html")
   ("pitch-bend-channel" "dict/index.html#midi-topic.html#pitch-bend-channel")
   ("pitch-bend-lsb" "dict/index.html#midi-topic.html#pitch-bend-lsb")
   ("pitch-bend-msb" "dict/index.html#midi-topic.html#pitch-bend-msb")
   ("pitch-bend-p" "dict/index.html#midi-topic.html#pitch-bend-p")
   ("pitch-class" "dict/index.html#pitch-class-fn.html")
   ("play" "dict/index.html#play-fn.html")
   ("plotter" "dict/index.html#plotter-fn.html")
   ("plotter-add-layer" "dict/index.html#plotter-add-layer-fn.html")
   ("plotter-data" "dict/index.html#plotter-data-fn.html")
   ("plotter-front-styling" "dict/index.html#plotter-front-styling-fn.html")
   ("plotter-property" "dict/index.html#plotter-property-fn.html")
   ("plotter-redraw" "dict/index.html#plotter-redraw-fn.html")
   ("plotter-redraw" "dict/index.html#plotter-close-fn.html")
   ("plotter-scroll" "dict/index.html#plotter-scroll-fn.html")
   ("plotter-zoom" "dict/index.html#plotter-zoom-fn.html")
   ("pm:countdevices" "dict/index.html#portmidi-topic.html#pm:countdevices")
   ("pm:getdefaultinputdeviceid" "dict/index.html#portmidi-topic.html#pm:getdefaultinputdeviceid")
   ("pm:getdefaultoutputdeviceid" "dict/index.html#portmidi-topic.html#pm:getdefaultoutputdeviceid")
   ("pm:getdeviceinfo" "dict/index.html#portmidi-topic.html#pm:getdeviceinfo")
   ("pm:time" "dict/index.html#portmidi-topic.html#pm:time")
   ("point" "dict/index.html#point-cls.html")
   ("portmidi-close" "dict/index.html#portmidi-topic.html#portmidi-close")
   ("portmidi-open" "dict/index.html#portmidi-topic.html#portmidi-open")
   ("portmidi-open?" "dict/index.html#portmidi-topic.html#portmidi-open?")
   ("portmidi-record!" "dict/index.html#portmidi-topic.html#portmidi-record")
   ("portmidi-stream" "dict/index.html#portmidi-topic.html#portmidi-stream")
   ("prime-form" "dict/index.html#prime-form-fn.html")
   ("process" "dict/index.html#process-mac.html")
   ("program-change-channel" "dict/index.html#midi-topic.html#program-change-channel")
   ("program-change-p" "dict/index.html#midi-topic.html#program-change-p")
   ("program-change-program" "dict/index.html#midi-topic.html#program-change-program")
   ("pval" "dict/index.html#pval-mac.html")
   ("pval" "dict/index.html#pval-cls.html")
   ("pwd" "dict/index.html#pwd-fn.html")
   ("quantize" "dict/index.html#quantize-fn.html")
   ("ran" "dict/index.html#ran-fn.html")
   ("range" "dict/index.html#range-cls.html")
   ("ransegs" "dict/index.html#ransegs-fn.html")
   ("receive" "dict/index.html#receive-fn.html")
   ("receiver?" "dict/index.html#receiverqmk-fn.html")
   ("remove-object" "dict/index.html#remove-object-fn.html")
   ("remove-receiver!" "dict/index.html#remove-receiver-fn.html")
   ("remove-subobjects" "dict/index.html#remove-subobjects-fn.html")
   ("rescale" "dict/index.html#rescale-fn.html")
   ("rescale-envelope" "dict/index.html#rescale-envelope-fn.html")
   ("rewrite" "dict/index.html#rewrite-cls.html")
   ("rewrite-generation" "dict/index.html#rewrite-generation-fn.html")
   ("rhythm" "dict/index.html#rhythm-fn.html")
   ("rm-spectrum" "dict/index.html#rm-spectrum-fn.html")
   ("rotation" "dict/index.html#rotation-cls.html")
   ("rts" "dict/index.html#rts-fn.html")
   ("rts-continue" "dict/index.html#rts-continue-fn.html")
   ("rts-pause" "dict/index.html#rts-pause-fn.html")
   ("rts-stop" "dict/index.html#rts-stop-fn.html")
   ("rts?" "dict/index.html#rtsqmk-fn.html")
   ("save-object" "dict/index.html#save-object-fn.html")
   ("sc-clearsched" "dict/index.html#supercollider-topic.html#sc-clearsched")
   ("sc-close" "dict/index.html#supercollider-topic.html#sc-close")
   ("sc-dumposc" "dict/index.html#supercollider-topic.html#sc-dumposc")
   ("sc-flush" "dict/index.html#supercollider-topic.html#sc-flush")
   ("sc-notify" "dict/index.html#supercollider-topic.html#sc-notify")
   ("sc-open" "dict/index.html#supercollider-topic.html#sc-open")
   ("sc-open?" "dict/index.html#supercollider-topic.html#sc-open?")
   ("sc-quit" "dict/index.html#supercollider-topic.html#sc-quit")
   ("scale-max" "dict/index.html#scale-max-fn.html")
   ("scale-min" "dict/index.html#scale-min-fn.html")
   ("scale-mod" "dict/index.html#scale-mod-fn.html")
   ("scale-order" "dict/index.html#scale-order-fn.html")
   ("scale<" "dict/index.html#scalelt-fn.html")
   ("scale<=" "dict/index.html#scalelteql-fn.html")
   ("scale=" "dict/index.html#scaleeql-fn.html")
   ("scale>" "dict/index.html#scalegt-fn.html")
   ("scale>=" "dict/index.html#scalegteql-fn.html")
   ("scaler->cents" "dict/index.html#scaler-gtcents-fn.html")
   ("sco-file" "dict/index.html#sco-file-cls.html")
   ("seq" "dict/index.html#seq-cls.html")
   ("sequence-number-p" "dict/index.html#midi-topic.html#sequence-number-p")
   ("sequencer-event-p" "dict/index.html#midi-topic.html#sequencer-event-p")
   ("sequence_track-name-p" "dict/index.html#midi-topic.html#sequence_track-name-p")
   ("set-clm-output-hook!" "dict/index.html#set-clm-output-hook-fn.html")
   ("set-midi-output-hook!" "dict/index.html#set-midi-output-hook-fn.html")
   ("set-receiver!" "dict/index.html#set-receiver-fn.html")
   ("set-sco-output-hook!" "dict/index.html#set-sco-output-hook-fn.html")
   ("shell" "dict/index.html#shell-fn.html")
   ("shuffle" "dict/index.html#shuffle-fn.html")
   ("smpte-offset-p" "dict/index.html#midi-topic.html#smpte-offset-p")
   ("song-position-lsb" "dict/index.html#midi-topic.html#song-position-lsb")
   ("song-position-msb" "dict/index.html#midi-topic.html#song-position-msb")
   ("song-position-p" "dict/index.html#midi-topic.html#song-position-p")
   ("song-position-route" "dict/index.html#midi-topic.html#song-position-route")
   ("song-select-p" "dict/index.html#midi-topic.html#song-select-p")
   ("song-select-route" "dict/index.html#midi-topic.html#song-select-route")
   ("song-select-song" "dict/index.html#midi-topic.html#song-select-song")
   ("sprout" "dict/index.html#sprout-fn.html")
   ("start-p" "dict/index.html#midi-topic.html#start-p")
   ("start-route" "dict/index.html#midi-topic.html#start-route")
   ("stop" "dict/index.html#stop-fn.html")
   ("stop-p" "dict/index.html#midi-topic.html#stop-p")
   ("stop-route" "dict/index.html#midi-topic.html#stop-route")
   ("subcontainers" "dict/index.html#subcontainers-fn.html")
   ("subobjects" "dict/index.html#subobjects-fn.html")
   ("sv" "dict/index.html#sv-mac.html")
   ("sv*" "dict/index.html#svstar-mac.html")
   ("sv+" "dict/index.html#svplus-mac.html")
   ("sysex-p" "dict/index.html#midi-topic.html#sysex-p")
   ("sysex-route" "dict/index.html#midi-topic.html#sysex-route")
   ("system-message-data1" "dict/index.html#midi-topic.html#system-message-data1")
   ("system-message-data2" "dict/index.html#midi-topic.html#system-message-data2")
   ("system-message-p" "dict/index.html#midi-topic.html#system-message-p")
   ("system-message-route" "dict/index.html#midi-topic.html#system-message-route")
   ("system-message-status" "dict/index.html#midi-topic.html#system-message-status")
   ("system-reset-p" "dict/index.html#midi-topic.html#system-reset-p")
   ("system-reset-route" "dict/index.html#midi-topic.html#system-reset-route")
   ("tempo-change-p" "dict/index.html#midi-topic.html#tempo-change-p")
   ("tendency" "dict/index.html#tendency-fn.html")
   ("text-event-p" "dict/index.html#midi-topic.html#text-event-p")
   ("thunk" "dict/index.html#thunk-cls.html")
   ("time-signature-p" "dict/index.html#midi-topic.html#time-signature-p")
   ("timing-clock-p" "dict/index.html#midi-topic.html#timing-clock-p")
   ("timing-clock-route" "dict/index.html#midi-topic.html#timing-clock-route")
   ("timing-tick-p" "dict/index.html#midi-topic.html#timing-tick-p")
   ("timing-tick-route" "dict/index.html#midi-topic.html#timing-tick-route")
   ("transpose" "dict/index.html#transpose-fn.html")
   ("transposer" "dict/index.html#transposer-cls.html")
   ("true" "dict/index.html#true-var.html")
   ("tune-request-p" "dict/index.html#midi-topic.html#tune-request-p")
   ("tune-request-route" "dict/index.html#midi-topic.html#tune-request-route")
   ("tuning" "dict/index.html#tuning-cls.html")
   ("use-system" "dict/index.html#use-system-fn.html")
   ("vary" "dict/index.html#vary-fn.html")
   ("wait" "dict/index.html#wait-fn.html")
   ("wait-until" "dict/index.html#wait-until-fn.html")
   ("weighting" "dict/index.html#weighting-cls.html"))
 )

;; eof
(enable-cm-commands)
(provide 'cm)
