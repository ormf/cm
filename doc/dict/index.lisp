;;; **********************************************************************
;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

(in-package :cm)

(flet ((grovel-cm-dict ()
	 (let* ((here (make-pathname :name nil :type nil
				     :defaults *load-pathname*))
		(root (make-pathname :defaults here
				     :directory
				     (butlast (pathname-directory here))))
		(enuf (enough-namestring here root))
		(htmls (merge-pathnames "*.html" here))
		(ignore '("terms" "index" "hshamp-mac" "hshi-mac"
			  "syntax"))
		(files (list)))
	   (loop with line for file in (directory htmls)
	      for topic? = (search "-topic" (pathname-name file))
	      unless (member (pathname-name file) ignore :test #'equal)
	      do
	      (if (not topic?)
		  (with-open-file (f file)
		    (setq line nil)
		    (loop for l = (read-line f nil nil)
		       until (null l)
		       do (if (> (length l) 7)
			      (if (string-equal l "<title>" :end1 7 )
				  (return (setq line l)))))
		    (unless line
		      (format t "~%; warning: ~a has no <title> tag." 
			      (namestring file)))
		    (when line
		      (let ((pos (search "</title>" line)))
			(if pos
			    (let* ((name (string-trim '(#\space)
						      (subseq line 7 pos)))
				   (ampr (position #\& name))
				   ampe symb)
			      (when ampr
				(cond ((string= name "&lt;" :start1 ampr
						:end1 (+ ampr 4))
				       (setq symb "<" ampe (+ ampr 4)))
				      ((string= name "&gt;" :start1 ampr
						:end1 (+ ampr 4))
				       (setq symb ">" ampe (+ ampr 4)))
				      ((string= name "&amp;" :start1 ampr
						:end1 (+ ampr 4))
				       (setq symb "&" ampe (+ ampr 5)))
				      (t
				       (format t "~%; warning: & token ~S"
					       name)))
				(when symb
				  (setq name (concatenate
					      'string
					      (subseq name 0 ampr)
					      symb (subseq name
							   ampe)))))
			      (push (list (string-downcase name)
					  (concatenate
					   'string
					   enuf
					   (pathname-name file)
					   ".html"))
				    files))))))
		  (with-open-file (f file)
		    ;; grovel through file for name entries
		    (loop for l = (read-line f nil nil)
		       for p = (search "<dl class=\"dictsyntax\" id=" l)
		       while l
		       when p
		       do
		       (let ((s (read-from-string l nil nil 
						  :start (+ p 26))))
			 (when s
			   (setq s (string-downcase s))
			   (push (list s
				       (concatenate 'string
						    enuf
						    (pathname-name file)
						    ".html#" s))
				 files)))))))
	   files )))
  (format
   t
   "~%(defvar *common-music-doc-root* \"http://commonmusic.sf.net/doc/\")")
  (format t "~% '(")
  (loop for e in (sort (grovel-cm-dict) #'string-lessp :key #'car)
     do (format t "~%   (~S ~S)" (car e) (cadr e)))
  (format t ")~%")
  (values))

	      