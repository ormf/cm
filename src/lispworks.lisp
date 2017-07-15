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

(pushnew ':metaclasses *features*)

(import '(clos:slot-definition-name 
          clos:slot-definition-initargs 
          clos:slot-definition-initform 
          clos:class-direct-superclasses
          clos:class-direct-subclasses
          hcl:class-slots
          hcl:class-direct-slots
          hcl:validate-superclass
          mp:without-interrupts
          ))

;; pkg.lisp blocked these symbols so CM does not inherit Lispworks
;; classes RANDOM and FUNCALL. THere must be a better way to do
;; this...

(defun random (ub &optional (state *random-state*))
  (cl:random ub state))

(defun funcall (fn &rest args) (apply #'cl:funcall fn args))

(defun finalize-class (class) class (values))

(defun shell (cmd &key (output t) (wait t))
  (if output
    (sys:call-system-showing-output cmd :wait wait)
    (sys:call-system cmd :wait wait)))

(defun quit () (lw:quit))

(defun exit () (quit))

(defun cd (&optional (dir (user-homedir-pathname )))
  (hcl:change-directory dir))

(defun pwd ()
  (hcl:get-working-directory))

(defun env-var (var)
  (hcl:getenv var))

(defun set-env-var (var)
  (hcl:setenv var))

(defun cm-image-dir ()
  ;; system:*line-arguments-list*
  (pathname-directory (lw:lisp-image-name)))

#+lispworks-personal-edition
(defun save-image (&rest args)
  args
  (error "Lispworks Personal Edition does not support save-image."))

#-lispworks-personal-edition
(defun save-cm (path &rest args)
  path args
  (hcl:save-image path 
                  :restart-function
                  #'(lambda ()
                      (declare (special *cm-readtable*))
                      (setf *package* (find-package :cm))
                      (setf *readtable* *cm-readtable*)
                      (load-cminit)
                      (cm-logo))))

;;;
;;; thread support
;;;

(defun current-thread () 
  mp:*current-process*)

(defun thread? (obj)
  (mp:process-p obj))

;(defun make-thread (thunk &optional name)
;  ;; this has to return a function that thead-start! calls
;  (let ((nam (or name (format nil "cm-~a" (gentemp 'thread)))))
;    (lambda () (mp:process-run-function name '() thunk))))

(defun make-thread (thunk &optional name)
  ;; this has to return a function that thead-start! calls
  (let* ((n (or name (format nil "CM-~A" (gentemp "THREAD"))))
         (p (mp:process-run-function n '() thunk)))
    (mp:process-disable p)
    p))

(defun thread-start! (thread)
  (mp:process-enable thread))

(defun thread-name (thread) 
  (mp:process-name thread))

;(thread-specific thread)
;(thread-specific-set! thread obj)

(defun thread-yield! ()
  (mp:process-allow-scheduling))

(defun thread-sleep! (timeout)
  (mp:sleep-for-time timeout))

(defun thread-terminate! (thread)
  (mp:process-kill thread))

;(thread-join! thread [timeout [timeout-val]])

(defun mutex? (obj)
  (typep obj 'mp:lock))

(defun make-mutex (&optional name)
  (mp:make-lock :name name))

(defun mutex-name (mutex)
  (mp:lock-name mutex))

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
  (get-internal-real-time))

(defun time? (obj)
  (and (integerp obj) (> obj 0)))

(defun time->seconds (time) 
  (/ time #.(coerce internal-time-units-per-second 'float)))

(defun seconds->time (sec)
  (truncate (* sec internal-time-units-per-second)))

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

(defun set-periodic-task! (&rest args)
  args
  (error "set-periodic-task!: no periodic tasks in LispWorks."))

