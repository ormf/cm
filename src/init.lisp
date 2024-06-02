;;; 
;;; init.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-user)

(defun cm-directory (&rest subs)
  (namestring
   (make-pathname :name nil :type nil
                  :directory (append (pathname-directory cl-user::*cm-directory*)
                                     subs)
                  :defaults cl-user::*cm-directory*)))

(pushnew :cm *features*)
  ;; load site init file if it exists
(load (merge-pathnames "cminit.lisp" (cm-directory "etc"))
      :if-does-not-exist nil)
  ;; load user init file if it exists
(load (merge-pathnames ".cminit.lisp" (user-homedir-pathname))
      :if-does-not-exist nil)
