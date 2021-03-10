;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod translate-object ((object thing) newx newy)
  (let ((dx (- newx (xtrans object)))
	(dy (- newy (ytrans object))))
    (translate-relative-object object dx dy)))

(defmethod translate-object ((object basic-thing) newx newy)
  (declare (ignore newx newy))
  (call-next-method))

(defmethod translate-relative-object ((object basic-thing) deltax deltay)
  (incf (xtrans object) deltax)
  (incf	(ytrans object) deltay)
  (translate-fixed-handles-relative object deltax deltay))

(defmethod translate-relative-object ((object composite-thing) deltax deltay)
  (with-slots (liste) object
    (dolist (elem liste)  
      (translate-relative-object elem deltax deltay)) ; weil beliebige Cluster moeglich!
    (incf (xtrans object) deltax)
    (incf (ytrans object) deltay)
    (translate-fixed-handles-relative object deltax deltay)))

(defmethod set-object-scaling ((object scaleable-thing) newleft newtop newright newbottom) 
  (let* ((xext (init-x-extend object))
	 (yext (init-y-extend object)))    
    (setf (xscale object)
      (/ (- newright newleft) xext))
    (setf (yscale object)
      (/ (- newbottom newtop) yext))))

;;;
;;;
;;;

(defmethod rotate-object ((object rotateable-thing) newangle)
  (setf (rotangle object) newangle))

(defmethod move-object ((object thing))
  (save-object-list)
  (make-undo-object object 'move)
  
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))

  (interactive-master-creator object 
			      #'(lambda (x y)
				  (translate-object object x y))
			      :drawer-function #'(lambda (object stream)
						   (draw object stream
							 :handling-active t)))
  (free-my-handles object)
  (adjust-object-origin object)
  (adjust-all-attached-objects object)
  (tick-object object)
  (do-incremental-updates))

(define-gened-command (com-move-object)
    ((object 'thing :gesture :move))
  (move-object object))

(define-gened-command (com-gened-move-object :name "Move Object") 
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(move-object source-object))))

;;;
;;;
;;;

(defmethod change-object-scaling ((object scaleable-thing))  
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))
 
  (make-undo-object object 'change-object-scaling)
 
  (let ((*concept-labels* nil)
	(xorg (xtrans object))
	(yorg (ytrans object)))
    
    (interactive-master-creator object 
				#'(lambda (x y) 
				    
				    (set-object-scaling object	
							(- (* 2 xorg) x)
							(- (* 2 yorg) y)
							x
							y)))

    (adjust-object-origin object)
    (free-my-handles object)
    (tick-object object)
    (set-init-extend object)
    (do-incremental-updates)))

(define-gened-command (com-scale-object)
    ((object 'scaleable-thing :gesture :scale))
  (change-object-scaling object))

(define-gened-command (com-gened-scale-object :name "Scale Object")
    ()
  (if (any-scaleable-objects)
      (let* ((source-object (accept 'scaleable-thing)))
	(terpri)
	(change-object-scaling source-object))))
;;;
;;;
;;;

(defmethod rotate-int-object ((object rotateable-thing))
  (make-undo-object object 'rotate-object)
  
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))
  
  (let* ((organgle (rotangle object))
	 (orgx (xtrans object))
	 (*concept-labels* nil))
    
    (interactive-master-creator object 
				#'(lambda (nowx nowy) 
				    (declare (ignore nowy))
				    (with-slots (rotangle) object 
				      (setf rotangle
					(+ organgle
					   (* (/ pi 150)
					      (round (- nowx orgx)))))))))
  
  (adjust-object-origin object)
  (free-my-handles object)
  (tick-object object)
  (set-init-extend object)
  (do-incremental-updates))

(define-gened-command (com-rotate-object)
    ((object 'rotateable-thing :gesture :rotate))
  (rotate-int-object object))

(define-gened-command (com-gened-rotate-object :name "Rotate Object")
    ()
   (if (any-rotateable-objects)
       (let* ((source-object (accept 'rotateable-thing)))
	 (terpri)
	 (rotate-int-object source-object))))

  
