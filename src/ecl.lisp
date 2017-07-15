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

(import '(clos:slot-definition-name 
          clos:slot-definition-initargs 
          clos:slot-definition-initform 
          clos:class-direct-superclasses
          clos:class-direct-subclasses
          clos:class-slots
          clos:class-direct-slots
          clos:validate-superclass
          ))

;; fix when i know what xp's *feature*  is.
(defconstant directory-delimiter #\/)

(defmacro without-interrupts (&body body)
  `(progn ,@body))

(defun shell (cmd &key (output t) (wait t))
  output wait
  (si:system cmd))

(defun finalize-class (class) 
  class
  ;(clos:finalize-inheritance class)
  (values))

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  class superclass
  t)

(defun quit () (si:quit))

(defun exit () (si:quit))

(defun pwd ()
  (si:getcwd))

(defun cd (&optional (dir (user-homedir-pathname )))
  (si:chdir (pathname dir))
  (si:getcwd))

(defun env-var (var)
  (si:getenv var))

(defun set-env-var (var val)
  (si:setenv var val))

(defun cm-image-dir ()
  ;; system:*line-arguments-list*
  nil)

(defun save-cm (path &rest args)
  args
  (c:build-program path :epilogue-code '(progn
					 (setf *package* (find-package :cm))
					 (setf *readtable* cm::*cm-readtable*)
					 (cm::load-cminit)
					 (cm::cm-logo))))

