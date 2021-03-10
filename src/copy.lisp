;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod copy-values ((source-object t) (destination-object t))
  ())

(defmethod copy-values ((source-object thing) (destination-object thing))
  (with-slots (xtrans ytrans hidden
	       parent-concepts id-number
	       associated-classic-ind) destination-object
    (setf parent-concepts (parent-concepts source-object))
    (setf xtrans (xtrans source-object))
    (setf ytrans (ytrans source-object))
    (setf hidden (hiddenp source-object))
    (setf associated-classic-ind 
      (associated-classic-ind source-object))
    (setf id-number (get-id-number)))
  (call-next-method))
     
(defmethod copy-values ((source-object composite-thing) (destination-object composite-thing))
  (with-slots (liste xtrans ytrans parent-concepts) source-object
    (dolist (elem (reverse liste))
      (let ((object (make-instance (type-of elem) :initialize nil)))
	(copy-values elem object)
	(push object
	      (liste destination-object))))
   (call-next-method)))


(defmethod copy-values ((source-object directed-info-element) (destination-object directed-info-element))
  (with-slots (start-info-point end-info-point) source-object 
    (let ((ps (make-instance 'info-point :initialize nil))
	  (pe (make-instance 'info-point :initialize nil)))
      (copy-values start-info-point ps)
      (copy-values end-info-point pe)
      (setf (id-number ps) (id-number start-info-point))
      (setf (id-number pe) (id-number end-info-point))
      (setf (start-info-point destination-object) ps)
      (setf (end-info-point destination-object) pe)))
  (call-next-method))

(defmethod copy-values ((source-object ink-mixin) (destination-object ink-mixin))
  (with-slots (ink) destination-object
    (setf ink (ink source-object)))
  (call-next-method))

(defmethod copy-values ((source-object linestyle-mixin) (destination-object linestyle-mixin))
  (with-slots (linestyle) destination-object
    (setf linestyle (linestyle source-object)))
  (call-next-method))

(defmethod copy-values ((source-object linethickness-mixin) (destination-object linethickness-mixin))
  (with-slots (linethickness) destination-object
    (setf linethickness (linethickness source-object)))
  (call-next-method))

(defmethod copy-values ((source-object filled-mixin) (destination-object filled-mixin))
  (with-slots (filledp filled-ink) destination-object
    (setf filledp (filledp source-object))
    (setf filled-ink (filled-ink source-object))
    (call-next-method)))

(defmethod copy-values ((source-object g-rectangle) (destination-object g-rectangle))
  (with-slots (xextend yextend) destination-object
    (setf xextend (xextend source-object))
    (setf yextend (yextend source-object))
    (call-next-method)))

(defmethod copy-values ((source-object g-arrow) (destination-object g-arrow))
  (call-next-method)
  (with-slots (startpoint endpoint) destination-object
    (make-object-handles destination-object (list startpoint endpoint)))) ;unschoen !!!

(defmethod copy-values ((source-object linesegment) (destination-object linesegment))
  (with-slots (startpoint endpoint) destination-object
    (setf startpoint (copy-list (startpoint source-object)))
    (setf endpoint   (copy-list (endpoint source-object)))
    (call-next-method)))

(defmethod copy-values ((source-object object-with-head) (destination-object object-with-head))
  (setf (head destination-object) 
    (head source-object))
  (call-next-method))

(defmethod copy-values ((source-object pointlist-object) (destination-object pointlist-object))
  (with-slots (pointlist) destination-object
    (setf pointlist (copy-list (pointlist source-object)))
    (make-object-handles destination-object (generate-brackets pointlist))
    (call-next-method)))

(defmethod copy-values ((source-object g-polygon) (destination-object g-polygon))
  (call-next-method))

(defmethod copy-values ((source-object g-chain) (destination-object g-chain))
  (call-next-method))

(defmethod copy-values ((source-object spline-object) (destination-object spline-object))
  (with-slots (spline-points) destination-object
    (setf spline-points (copy-list (spline-points source-object))))
  (call-next-method))

(defmethod copy-values ((source-object g-spline-chain) (destination-object g-spline-chain))
  (call-next-method))

(defmethod copy-values ((source-object g-spline-polygon) (destination-object g-spline-polygon))
  (call-next-method))

(defmethod copy-values ((source-object g-circle) (destination-object g-circle))
  (with-slots (radius) destination-object
    (setf radius (radius source-object))
    (call-next-method)))
          
(defmethod copy-values ((source-object point) (destination-object point))
  (call-next-method))

(defmethod copy-values ((source-object info-point) (destination-object info-point))
  (setf (startpoint-of destination-object)
    (startpoint-of source-object))
  (setf (endpoint-of destination-object)
    (endpoint-of source-object))   
  (call-next-method))

(defmethod copy-values ((source-object object-with-handles) 
			(destination-object object-with-handles))
  (call-next-method))

(defmethod copy-values ((source-object g-text)
			(destination-object g-text))
  (with-slots (text-string face size family) destination-object
    (setf text-string (text-string source-object))
    (setf face (face source-object))
    (setf family (family source-object))
    (setf size (size source-object))
    (call-next-method)))
	  
      
(defmethod copy-values ((source-object scaleable-thing)
			(destination-object scaleable-thing))
  (with-slots (xscale yscale init-x-extend init-y-extend) destination-object
    (setf xscale (xscale source-object))
    (setf yscale (yscale source-object))
    (setf init-x-extend (init-x-extend source-object))
    (setf init-y-extend (init-y-extend source-object))
    (call-next-method)))
    
(defmethod copy-values ((source-object rotateable-thing) 
			(destination-object rotateable-thing))
  (with-slots (rotangle) destination-object
    (setf rotangle (rotangle source-object))
    (call-next-method)))

;;;
;;;
;;;

(defmethod copy-object ((source-object thing))			 
  (with-application-frame (gened-frame)
    (with-slots (liste mode) gened-frame
      (let ((destination-object (make-instance (type-of source-object)
				  :initialize nil)))
	(copy-values source-object destination-object)
	(translate-relative-object destination-object 10 10)
	(push destination-object liste)
	(make-undo-object destination-object 'copy)
	(tick-object destination-object)
	(do-incremental-updates)))))
    
(define-gened-command (com-copy-object)
    ((source-object 'thing :gesture :copy))
  (copy-object source-object))

(define-gened-command (com-gened-copy-object :name "Copy Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(copy-object source-object))))
