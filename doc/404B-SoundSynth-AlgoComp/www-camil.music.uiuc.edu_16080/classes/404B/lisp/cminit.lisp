(in-package :cm)

;; Put this .cminit.lisp file in your home directory. Add forms you
;; want to evalute when CM starts create.

(defparameter *homedir* (user-homedir-pathname))

(defparameter *404bdir* (merge-pathnames "404b/" *homedir*))

;; load  ~/404b/msutils.lisp file it exists else warn

(let ((utils (merge-pathnames *404bdir* "msutils.lisp")))
  (if (probe-file utils)
    (load utils)
    (format t "; Warning: Can't load ~S: file does not exist." utils)))

;;
;; Add your forms here:
;;


