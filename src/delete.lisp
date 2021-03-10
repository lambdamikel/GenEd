;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod delete-it ((object basic-thing))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (delete-polyrep-from-cache object)	
      (setf liste (delete object liste :test #'equal)))))

(defmethod delete-it ((object composite-thing))
  (dolist (elem (liste object))
    (delete-it elem))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (setf liste (delete object liste :test #'equal)))))

(defmethod delete-object ((object thing) &key (undo-object t))
  (if undo-object (make-undo-object object 'delete))
  (delete-it object)
  (free-all-handles object) 
  (free-my-handles object)
  (do-incremental-updates))

(define-gened-command (com-delete-object)
    ((object 'thing :gesture :delete))
  (delete-object object))

(define-gened-command (com-gened-delete-object :name "Delete Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(delete-object source-object))))

