;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package :cl-user)

(defvar *cm-directory* 
  (namestring
   (truename (make-pathname :name nil :type nil
                            :defaults *load-truename*))))

#-(or allegro clisp cmu lispworks openmcl sbcl ecl)
(error "Sorry, Common Music does not run in this Lisp.")

(defpackage :common-music-system (:use :cl :asdf))
(in-package :common-music-system)

(defun cm-directory (&rest subs)
  (namestring
   (make-pathname :name nil :type nil
                  :directory (append (pathname-directory cl-user::*cm-directory*)
                                     subs)
                  :defaults cl-user::*cm-directory*)))

(defmethod perform :after ((op load-op) cm)
  ;; add cm feature before loading other systems...
  (pushnew :cm *features*)
  ;; load site init file if it exists
  (load (merge-pathnames "cminit.lisp" (cm-directory "etc"))
        :if-does-not-exist nil)
  ;; load user init file if it exists
  (load (merge-pathnames ".cminit.lisp" (user-homedir-pathname))
        :if-does-not-exist nil))

;;;
;;; system definition
;;;

(defsystem :cm
    :description "Common Music"
    :version "2.12.0"
    :author "Rick Taube <taube (at) uiuc.edu>"
    :licence "LLGPL"
    :depends-on (:alexandria)
    :components
    ((:module "src"
      :serial t
      :components ((:file "pkg")
                   (:file #+allegro "acl"
                          #+clisp "clisp"
                          #+cmu "cmu"
                          #+ecl "ecl"
                          #+lispworks "lispworks"
                          #+(and mcl (not openmcl)) "mcl"
                          #+openmcl "openmcl"
                          #+sbcl "sbcl")
                   (:file "iter")
                   (:file "level1")
                   (:file "clos")
                   (:file "scheme")
                   (:file "utils")
                   (:file "mop")
                   (:file "objects")
                   (:file "data")
                   (:file "scales")
                   (:file "spectral")
                   (:file "patterns")
                   (:file "io")
                   (:file "scheduler")
                   (:file "gnuplot")
                   (:file "plt")
                   (:file "sco")
                   (:file "clm")
                   (:file "midi1")
                   (:file "midi2")
                   (:file "midi3")
                   (:file "cmn")
;;;                   (:file "fomus")
                   (:file "sc")
                   (:file "rt")
                   (:file "parse")))))

;;;
;;; main functions
;;;

(in-package :cl-user)

(defun use-system (sys &key directory bin-directory
                   (verbose t) warnings symbols)
  (declare (ignore directory bin-directory warnings symbols))
  (asdf:load-system sys :verbose verbose))

(defun cm (&rest systems)
  (flet ((cmcall (fn &rest args)
           (apply (find-symbol (string fn) :cm) args))
         (cmvar (var)
           (symbol-value (find-symbol (string var) :cm))))
    (setf *package* (find-package :cm))
    (setf *readtable* (cmvar :*cm-readtable*))
    ;; add slime readtable mapping...
    (let ((swank-pkg (find-package :swank)))
      (when swank-pkg
        (let ((sym (intern (symbol-name :*readtable-alist*) swank-pkg)))
          (setf (symbol-value sym)
                (cons (cons (symbol-name :cm) (cmvar :*cm-readtable*))
                      (symbol-value sym))))))
    (let (#-sbcl (*trace-output* nil))
      (dolist (s systems) (use-system s :verbose nil)))
    (cmcall :cm-logo)))

(export '(cm use-system) :cl-user)
