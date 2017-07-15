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

(import '(ext::load-foreign
          pcl:slot-definition-initargs
          pcl:slot-definition-initform
          pcl:slot-definition-name
          pcl:class-direct-slots
          pcl:class-slots
          ;pcl::class-direct-subclasses
          pcl::class-direct-superclasses
          pcl:generic-function-name
          system:without-interrupts
          )
	:cm)

(defun quit () (ext::quit))
(defun exit () (quit))

#+cmu19
(defun class-subclasses (c)
  (let ((subs (pcl::class-direct-subclasses c)))
    (if (null subs)
	'()
      (loop for s in subs
	append (cons s (class-subclasses s))))))
	  
#+cmu18
(defun class-subclasses (class)
  (let ((tbl (kernel:class-subclasses class))
        (sub '()))
    (maphash (lambda (k v) v (push k sub)) tbl)
    (nreverse sub)))

#-(or cmu19 cmu18)
(error "Fix class-subclasses for this version of cmu.")

;(defun make-load-form (obj)
;  (pcl::make-load-form obj))

(defun finalize-class (class) 
  class
  )

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defun slot-defintion-reader (slot) slot nil)

;;;
;;; misc. stuff
;;;

(defun object-address (thing)
  (kernel:get-lisp-obj-address thing))

(defconstant directory-delimiter #\/)

;(defun cd (&optional dir)
;  (if dir
;    (progn (setf (ext:default-directory) dir)
;	   (namestring (ext:default-directory)))
;    (namestring (ext:default-directory))))

(defun cd (&optional (dir (user-homedir-pathname )))
  (setf (ext:default-directory) dir)
  (namestring (ext:default-directory)))

(defun pwd ()
  (namestring (ext:default-directory)))

;;(defun shell (format &rest strings) 
;;  (let ((str (apply #'format nil format strings)))
;;    (extensions:run-program "/bin/csh" (list "-fc" str) :output t)))

(defun shell (cmd &key (wait t) (output t))
  (extensions:run-program "/bin/csh" (list "-fc" cmd)
                          :output output :wait wait))

(defun env-var (var)
  (let ((x (assoc var ext:*environment-list*
                  :test #'string=)))
    (and x (cdr x) )))

(defun set-env-var (name value)
  (let ((cell (assoc (string name) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string value))
      (push (cons (intern (string name) "KEYWORD") (string value))
	    ext:*environment-list*))))

(defun cm-image-dir ()
  (let ((img (member "-core" ext:*command-line-strings*)))
    (if img
	(namestring
	 (make-pathname
	  :directory (pathname-directory (cadr img))))
      nil)))

(defun save-cm (path &rest args)
  (declare (ignore args))
  #+:linkage-table
  (pushnew 'sys::reinitialize-global-table ext:*after-save-initializations*)       
  (extensions:save-lisp path :print-herald NIL
                        :init-function
                        #'(lambda ()
                            (declare (special *cm-readtable*))
                            (setf *readtable* *cm-readtable*)
                            (setf *package* (find-package :cm))
                            (load-cminit)                            
                            (cm-logo)
                            (lisp::%top-level))))

;;; attempt thread support on x86 using cmucl's "multiprocessing"
;;; package. mp is NOT based on native threads so this is unlikely to
;;; work well. decisecond time precision is really bad too.

(defun nothreads ()
  (error "CMUCL user threads are not support in this OS."))

(defun thread-current-time ()
  (get-internal-real-time))

(defun current-thread ()
  #+x86 mp:*current-process*
  #-x86 (nothreads))

(defun thread? (obj)
  obj
  #+x86 (mp:processp obj)
  #-x86 (nothreads))

(defun make-thread (thunk &optional name)
  thunk name
  #+x86
  (progn
    (mp::init-multi-processing)
    (unless mp::*idle-process*
      (setq mp::*idle-process* mp::*initial-process*))
    (let* ((n (or name (format nil "CM-~A" (gentemp "THREAD"))))
           (p (mp:make-process thunk :name n)))
      (mp:disable-process p)
      p))
  #-x86 (nothreads))

(defun thread-name (thread) 
  thread
  #+x86 (mp:process-name thread)
  #-x86 (nothreads))

;(thread-specific thread)
;(thread-specific-set! thread obj)

(defun thread-start! (thread)
  thread
  #+x86 (mp:enable-process thread)
  #-x86 (nothreads))

(defun thread-yield! ()
  #+x86 (mp:process-yield)
  #-x86 (nothreads))

(defun thread-sleep! (timeout)
  timeout
  #+x86 (sleep timeout)
  #-x86 (nothreads))

(defun thread-terminate! (thread)
  thread
  #+x86 (mp:destroy-process thread)
  #-x86 (nothreads))

;(thread-join! thread [timeout [timeout-val]])

(defun mutex? (obj)
  obj
  #+x86 (typep obj 'mp:lock)
  #-x86 (nothreads))

(defun make-mutex (&optional name)
  name
  #+x86 (if name (mp:make-lock name) (mp:make-lock))
  #-x86 (nothreads))

(defun mutex-name (mutex)
  mutex
  #+x86 (slot-value mutex 'mp::name)
  #-x86 (nothreads))

(defmacro with-mutex-grabbed ((var) &body body)
  `(mp:with-lock-held (,var "rts lock wait" :wait t) ,@body))

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
;;; period task support
;;;

(defvar *periodic-tasks* (list ))

(defun set-periodic-task-rate! (rate meas)
  ;; set periodic time but only if tasks are not running
  (if *periodic-tasks*
      (error "set-periodic-task-rate!: Periodic tasks currently running.")
      (let (divs)
        (ecase meas
          ((:second :seconds :sec :s)
           (setf divs 1))
          ((:millisecond :milliseconds :ms :m)
           (setf divs 1000))
          ((:nanosecond :nanoseconds :usec :usecs :u)
           (setf divs 1000000)))
        (multiple-value-bind (sec rem)
            (floor rate divs)
          ;;(print (list sec (* rem (/ 1000000 divs))))
          (setf lisp::*max-event-to-sec* sec)
          (setf lisp::*max-event-to-usec*
                (floor (* rem (/ 1000000 divs)))))
        (values))))

(defun periodic-task-rate ()
  ;; always return in usec
  (+ (* lisp::*max-event-to-sec* 1000000)
     lisp::*max-event-to-usec*))

;; (defun set-periodic-task-rate! (milli)
;;   ;; set periodic time but only if tasks are not running
;;   (if *periodic-tasks*
;;       (error "set-periodic-task-rate!: Periodic tasks currently running.")
;;       (let (sec mil)
;;         (if (< milli 1000)
;;             (setf sec 0 mil milli)
;;             (multiple-value-setq (sec mil)
;;               (floor milli 1000)))
;;         (setf lisp::*max-event-to-sec* sec)
;;         (setf lisp::*max-event-to-usec* (* mil 1000))
;;         milli)))

(defun run-periodic-tasks ()
  ;; this is the polling function, it just funcalls thunks on the list
  (dolist (e *periodic-tasks*) (funcall (cdr e)))
  (values))

(defun periodic-task-running? (&optional owner)
  (if *periodic-tasks*
      (if owner 
          (and (assoc owner *periodic-tasks* :test #'eq) t)
          t)
      nil))

(defun add-periodic-task! (owner task)
  (cond ((null *periodic-tasks*)
         (push (cons owner task) *periodic-tasks*)
         (setf lisp::*periodic-polling-function* #'run-periodic-tasks))
        ((assoc owner *periodic-tasks* :test #'eq)
         (error "add-periodic-task!: task already running for ~s."
                owner))
        (t
         (push (cons owner task) *periodic-tasks*)))
  (values))

(defun remove-periodic-task! (owner)
  (if (eq owner t)
      (setf lisp::*periodic-polling-function* nil
            *periodic-tasks* (list))
      (let ((e (assoc owner *periodic-tasks* :test #'eq)))
        (cond ((null e)
               (error "remove-periodic-task!: No task for owner ~s."
                      owner))
              (t
               (setf *periodic-tasks* (delete e *periodic-tasks*))
               (if (null *periodic-tasks*)
                   (setf lisp::*periodic-polling-function* nil))))))
  (values))

#|
(periodic-task-running?)
(set-periodic-task-rate! 1000) ; milliseconds
(defun moe () (print :you-moron))
(defun larry () (print :ow-ow-ow))
(defun curly () (print :nyuk-nyuk))
(periodic-task-running? :moe)
(add-periodic-task! :moe #'moe)
(add-periodic-task! :larry #'larry)
(remove-periodic-task! :moe)
(add-periodic-task! :curly #'curly)
(add-periodic-task! :moe #'moe)
(remove-periodic-task! :larry)
*periodic-tasks*
(remove-periodic-task! t)
|#
