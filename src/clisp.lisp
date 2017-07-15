;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

(in-package :cm)

(pushnew :metaclasses *features*)

(import '(clos::class-direct-subclasses
          clos::class-direct-superclasses
          clos::class-direct-slots
          clos:slot-definition-initargs
          clos:slot-definition-initform
          clos:slot-definition-name
          clos:class-slots
          clos:generic-function-name
          ))

(defun exit () (ext::exit))
(defun quit () (exit))
(defun lisp-version ()
  (let* ((s (lisp-implementation-version))
         (p (position-if #'digit-char-p s)))
    (concatenate 'string "CLISP "
                 (subseq s p (position #\space s :start p)))))

;;;(setf clos::*allow-mixing-metaclasses* T)

;(defun slot-definition-initargs (slot)
;  (clos::slotdef-initargs slot))
;
;(defun slot-definition-initform (slot)
;  (cdr (clos::slotdef-initer slot)))
;
;(defun slot-definition-name (slot)
;  (clos::slotdef-name slot))
;
;(defun slod-definition-reader (slot)
;  slot
;  nil)
;
;(defun class-slots (class)
;  (clos::class-slots class))
;
;(defun generic-function-name (class)
;  (error "generic-function-name not implmented in clisp."))
  
(defun finalize-class (class) class (values))

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defmethod make-load-form (obj)
  (error "no make-load-form method for ~s." obj))

;;(setf clos::*warn-if-gf-already-called* NIL)
;;(setf custom:*warn-on-floating-point-contagion* nil)

;;;
;;;  misc. stuff
;;;

;(defun object-address (x) (sys::address-of x))

#-win32
(defun shell (cmd &key (output t) (wait t))
  (ext:run-shell-command cmd :output (if output :terminal nil)
                         :wait wait))
(defun shell-quoted-pathname-for-dos (path)
  (let ((dev (pathname-device path))
	(dir (apply #'concatenate
		    'string
		    (loop for d in (cdr (pathname-directory path)) 
			  if (find #\space d)
			  collect (concatenate 'string "\"" d "\"") 
			  else collect d
			  collect "\\")))
	(nam (pathname-name path))
	(ext (pathname-type path)))
    (concatenate 'string dev ":\\" dir nam "." ext)))

#+win32
(defun shell (cmd &key (wait nil) (output t))
  (let ((pos (or (search ".exe" cmd)(search ".bat" cmd))))
    (if pos
	(let* ((exe (probe-file (subseq cmd 0 (+ pos 4)))))
	  (if (null exe)
	      nil
	      (let ((arg (substitute #\\ #\/ (subseq cmd (+ pos 4)))))
		;; convert to Windows format pathname
		(setq exe (namestring exe))
		;; explicity quote if path contains spaces
		(when (find #\space exe)
		  (setq exe (shell-quoted-pathname-for-dos exe)))
		(ext:run-shell-command (concatenate 'string
						    exe arg)
				       :output output :wait wait))))
	(ext:run-shell-command cmd :output output :wait wait))))

; (setq foo (namestring (probe-file "/program files/windows media player/mplayer2.exe")))
; (shell-quoted-pathname-for-dos foo)

(defconstant directory-delimiter #\/)

;(defun cd (&optional dir)
;  (if dir (setf (ext:default-directory) dir)
;      (ext:default-directory)))
;

(defun cd (&optional (dir (user-homedir-pathname )))
  (setf (ext:default-directory) dir))

(defun pwd ()
  (namestring (ext:default-directory)))

(defun env-var (var)
  (ext::getenv var))

(defun set-env-var (var val)
  (system::setenv var val))

(defun cm-image-dir ()
  ;; clisp's ext:argv only appears in 2.32
  (let* ((v (ext:argv))
         (l (length v))
         (i (position "-M" v :test #'string-equal))
         )
    (if (and i (< i (- l 1)))
      (let ((img (elt v (+ i 1)) ))
        (enough-namestring img
                           (concatenate 'string (pathname-name img)
                                        "." (pathname-type img))))
      nil)))

(defun save-cm (path &rest args)
  (declare (ignore args))
  (ext:saveinitmem path :quiet t
                   :init-function
                   #'(lambda ()
                       (declare (special *cm-readtable*))
                       (setf *package* (find-package :cm))
                       (setf *readtable* *cm-readtable*)
                       (load-cminit)
                       (cm-logo)
                       )))

;;;
;;; thread and osc stubs
;;;

(defun nothreads ()
  (error "Threads are not supported in CLISP."))

(defun current-thread ()
  (nothreads))

(defun thread? (obj)
  obj
  (nothreads))

(defun make-thread (thunk &optional name)
  thunk name
 (nothreads))

(defun thread-name (thread) 
  thread
  (nothreads))

;(thread-specific thread)
;(thread-specific-set! thread obj)

(defun thread-start! (thread)
  thread
  (nothreads))

(defun thread-yield! ()
  (nothreads))

(defun thread-sleep! (timeout)
  timeout
  (nothreads))

(defun thread-terminate! (thread)
  thread
  (nothreads))

;(thread-join! thread [timeout [timeout-val]])

(defun mutex? (obj)
  obj
  (nothreads))

(defun make-mutex (&optional name)
  name
  (nothreads))

(defun mutex-name (mutex)
  mutex
  (nothreads))

;(mutex-specific mutex)
;(mutex-specific-set! mutex obj)
;(mutex-state mutex)
;(mutex-lock! mutex [timeout [thread]])
;(mutex-unlock! mutex [condition-variable [timeout]])
;(condition-variable? obj)
;(make-condition-variable [name])
;(condition-variable-name condition-variable)
;(condition-variable-specific condition-variable)
;(condition-variable-specific-set! condition-variable obj)
;(condition-variable-signal! condition-variable)
;(condition-variable-broadcast! condition-variable)

(defun current-time ()
  (nothreads))

(defun time? (obj)
  obj
  (nothreads))

(defun time->seconds (time) 
  time
  (nothreads))

(defun seconds->time (sec)
  sec
  (nothreads))

(defun thread-current-time ()
  (nothreads))

(defmacro without-interrupts (&body body)
  `(progn ,@body))

(defmacro with-mutex-grabbed ((mutex) &body body)
  mutex
  `(progn ,@body))

;(current-exception-handler)
;(with-exception-handler handler thunk)
;(raise obj)
;(join-timeout-exception? obj)
;(abandoned-mutex-exception? obj)
;(terminated-thread-exception? obj)
;(uncaught-exception? obj)
;(uncaught-exception-reason exc)

;;;
;;; periodic task support
;;;

(defparameter *periodic-tasks* nil)

(defun add-periodic-task! (&rest args)
  args
  (error "set-periodic-task! not implemented in this lisp/os"))

(defun remove-periodic-task! (&rest args)
  args
  (error "remove-periodic-task! not implemented in this lisp/os"))

(defun set-periodic-task-rate! (&rest args)
  args
  (error "set-periodic-task-rate! not implemented in this lisp/os"))

(defun periodic-task-rate (&rest args)
  args
  (error "periodic-task-rate not implemented in this lisp/os"))

(defun periodic-task-running? (&rest args)
  args
  (error "periodic-task-running? not implemented in this lisp/os"))

;;; stub out other rt stuff

(defun rts-run-specific (object ahead end)
  object ahead end 
  (error "rts-run-specific not implemented in this lisp"))

(defun osc-vector->osc-message (arr) 
  arr
  (error "osc-vector->osc-message not implemented in this lisp/os"))

(defun udp-socket-recv (sock bytes &optional flags)
  sock bytes flags
  (error "udp-socket-recv not implemented in this lisp/os"))

(defun make-udp-socket (host port local-port)
  host port local-port
  (error "make-udp-socket not implemented in this lisp/os"))

(defun udp-socket-close (sock)
  sock
  (error "udp-socket-close not implemented in this lisp/os"))

(defun make-osc-timetag (offset out)
  offset out
  (error "make-osc-timetag not implemented in this lisp/os"))

(defun send-osc (mess oscstr len)
  mess oscstr len
  (error "send-osc not implemented in this lisp/os"))

