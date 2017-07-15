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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "pascal-strings"))

(pushnew :metaclasses *features*)

(import '(ccl:slot-definition-name 
          ccl:slot-definition-initargs 
          ccl:slot-definition-initform 
          ccl:class-direct-superclasses
          ccl:class-direct-subclasses
          ccl:open-shared-library ; needed if loading clm into cm.
          ccl:without-interrupts ; for testing realtime
          #+:openmcl-partial-mop
          ccl:class-slots
          #+:openmcl-partial-mop
          ccl:class-direct-slots
          #+:openmcl-partial-mop
          ccl:validate-superclass))

#-:openmcl-partial-mop
(progn
  (defun class-slots (class) 
    (ccl::class-instance-slots class))
  (defun class-direct-slots (class)
    (ccl:class-direct-instance-slots class))
  (defmethod validate-class ((class t) (superclass t))
    ;; this is a no-op except in OpenMCL 014
    t))

(defun finalize-class (class) class t)

(defmethod make-load-form (obj) (cl:make-load-form obj))

(defun slot-definition-reader (slot) slot nil)

;;;
;;; misc stuff
;;;

(defun quit () (ccl:quit))

(defun exit () (quit))

(defun object-address (x)
  (ccl:%address-of x))

(defun generic-function-name (fn)
  (ccl::function-name fn))

(defun cd (&optional (dir (user-homedir-pathname )))
  (ccl::cwd dir))

(defun pwd ()
  (namestring (ccl::mac-default-directory)))

(defun explode-string (str)
  ;; parse str into a list of tokens
  ;; delimited by whitespace
  (let ((white '(#\space #\tab))
	(slen (length str))
	(args '()))
    (loop with i = 0 and b and s and l
	  while (< i slen)
	  do
	  ;; flush whitespace
          (loop while (and (< i slen)
			   (member (elt str i) white))
                do (incf i))
	  (unless (< i slen)
	    (return))
	  (setf b i)
	  (setf s nil)
	  (setf l #\null)
	  ;; read until next undelimited whitspace
	  (loop while (and (< i slen)
			   (or (not (member (elt str i) white))
			       (char= l #\\)
			       s))
                do (if (char= (elt str i) #\")
                       (setf s (not s)))
                   (setf l (elt str i))
                   (incf i))
          (push (subseq str b i) args))
    (nreverse args)))

(defun shell (cmd &key (output t) (wait t))
  (ccl:run-program "/bin/csh" (list "-fc" cmd)
                   :output output :wait wait))

(defparameter *browser* nil)

(defun open-url (url &key (browser *browser*))
  (ccl:run-program "open" (list "-a" browser url))
  (values))

(defconstant directory-delimiter #\/)

(defun env-var (var)
  (ccl::getenv (string var)))

(defun set-env-var (var val)
  (ccl::setenv (string var) val))

;;;
;;; cm application classes
;;; 

(defclass cm-application (ccl::lisp-development-system) ())
(defclass cm-carbon-application (cm-application) ())

(defparameter *cm-application-class* (find-class 'cm-application))

(defmethod initialize-instance :after ((obj cm-application) &rest args)
  args
  (setf (slot-value obj 'ccl::command-line-arguments)
	(list ccl::*standard-help-argument*
	      (ccl::make-command-line-argument
	       :option-char #\I
	       :long-name "image-name"
	       :keyword :image-name
	       :help-string "image-name <file>"
	       :may-take-operand t
	       :allow-multiple nil)
	      (ccl::make-command-line-argument
	       :option-char #\l
	       :long-name "load"
	       :keyword :load
	       :help-string "load <file>"
	       :may-take-operand t
	       :allow-multiple t)
	      (ccl::make-command-line-argument
	       :option-char #\e
	       :long-name "eval"
	       :keyword :eval
	       :help-string "evaluate <form> (may need to quote <form> in shell)"
	       :may-take-operand t
	       :allow-multiple t))))

(defmethod ccl:application-name ((app cm-application)) "Common Music")

(defmethod ccl::application-version-string ((a cm-application))
  (cm-version))

(defmethod ccl:toplevel-function ((app cm-application) init-file)
  (declare (ignore init-file))
  (call-next-method))

(defparameter *cm-swank-port* nil)

(defmethod ccl:toplevel-function ((app cm-carbon-application) init-file)
  (declare (ignore init-file) (special *cm-readtable*))
  (when (and *cm-swank-port* (find-package ':swank))
    (funcall (find-symbol "CREATE-SERVER" ':swank)
             :port *cm-swank-port*))
  (ccl:quit))

(defun cm-image-dir ()
  (namestring
   (make-pathname
    :directory (pathname-directory ccl::*heap-image-name*))))

(defun save-cm (path &rest args)
  (declare (ignore args) (special *cm-readtable*))
  (setf ccl::*inhibit-greeting* t)
  (setf ccl:*lisp-startup-functions*
        (append ccl:*lisp-startup-functions*
                (list #'(lambda ()
                          (declare (special *cm-readtable*))
                          (setf *package* (find-package :cm))
                          (setf *readtable* *cm-readtable*)
                          (load-cminit)
                          (cm-logo)
                          ))))
  (ccl:save-application path :application-class *cm-application-class*))

(defun normal-setup ()
  (setf ccl::*inhibit-greeting* t)
    (setf ccl:*lisp-startup-functions*
        (append ccl:*lisp-startup-functions*
                (list #'(lambda ()
                          (declare (special *cm-readtable*))
                          (setf *package* (find-package :cm))
                          (setf *readtable* *cm-readtable*)
                          (load-cminit)
                          (cm-logo) )))))

(defun carbon-setup (slime-directory swank-port)
  (setf *cm-application-class* (find-class 'cm-carbon-application))
  (when slime-directory
    (if (ccl:directoryp slime-directory) ; ensure / at end
        (setf slime-directory (probe-file slime-directory))
	(error "save-cm: :slime-directory ~s is not a directory."
	       slime-directory))
    (let ((files '("swank-backend" "nregex" "metering" "swank-openmcl" "swank-gray"
                   "swank")))
      (dolist (f files)
        (load (merge-pathnames f slime-directory)))))
  (when swank-port
    (unless (find-package :swank)
      (error "save-cm: :swank-port specified but swank is not loaded. Specify :slime-directory to load it."))
    (setf *cm-swank-port* swank-port)))

(defun create-info.plist (path carbon?)
  (with-open-file (fil path :direction :output :if-does-not-exist :create
                       :if-exists :supersede)
    (format fil
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>English</string>
	<key>CFBundleExecutable</key>
	<string>~A</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>CFBundleVersion</key>
	<string>0.2</string>
	<key>NSMainNibFile</key>
	<string>MainMenu</string>
</dict>
</plist>
" (if carbon? "cm" "cm.sh"))))

(defun create-cm.sh (path)
  (with-open-file (fil path :direction :output :if-does-not-exist :create
                       :if-exists :supersede)
    (format fil
"#!/bin/sh 
CWD=\"`(cd \\\"\\`dirname \\\\\\\"$0\\\\\\\"\\`\\\"; echo $PWD)`\"
RES=\"`dirname \\\"$CWD\\\"`/Resources\"
EDITOR=`defaults read -app CM editor 2>/dev/null`
export \"DISPLAY=:0.0\"
if [[ ! $EDITOR || $EDITOR == Emacs ]] ; then
    EDITOR=\"/Applications/Emacs.app/Contents/MacOS/Emacs\"
fi

if [ -f \"$EDITOR\" ] ; then
    \"$EDITOR\" -l \"${RES}/etc/listener.el\" -l \"${RES}/etc/cm.el\" --eval \"(lisp-listener \\\"${CWD}/cm --image-name ${CWD}/cm.image\\\" )\"
else
    open -a Terminal \"${CWD}/cm\"
fi

#EOF
"))
  (shell (format nil "chmod a+x ~A" (namestring path))))
