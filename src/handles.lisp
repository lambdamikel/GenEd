;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod move-object-handle ((object object-handle))    
  (make-undo-object (parent-object object) 'move-object-handle)
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))

  (let ((*concept-labels* nil)
	(parent (parent-object object)))
    (loop 
      (interactive-master-creator object 
				  #'(lambda (x y)
				      (change-object-handle 
				       object x y))
				  :drawer-function #'(lambda (object stream)
						       (draw parent stream
							     :handle-handling-active t
							     :handling-active t)))
      (if (object-ok-p parent)
	  (progn 
	    (tick-object parent)
	    (adjust-object-origin parent)
	    (do-incremental-updates)
	    (return))
	(beep)))))
							
(define-gened-command (com-move-object-handle)
    ((object 'object-handle :gesture :move))
  (move-object-handle object))

(define-gened-command (com-gened-move-object-handle :name "Move Object-Handle") 
    ()
  (let ((source-object (accept-handle 'object-handle '(start end fixed unfixed normal))))
    (move-object-handle source-object))
  (only-tick-all-objects)
  (redraw))
    
;;;
;;;
;;;

(defun accept-handle (handle-type display-options)
  (let ((*handles-symbol-list* display-options))
    (only-tick-all-objects)
    (redraw)
    (prog1 
	(accept handle-type)
      (terpri))))
  
(defmethod fix-handle ((object object-handle))
  (let ((x (x object))
	(y (y object))
	(parent-object (parent-object object)))
    
    (multiple-value-bind (x y)
	(transform-point parent-object x y)
      (multiple-value-bind (x y)
	  (scale-mouse-position x y)
      
      (with-application-frame (gened-frame)     
	(with-slots (liste) gened-frame
	  (or 
	   (some #'(lambda (elem)
		     (unless (eq parent-object elem)
		       (multiple-value-bind (xf yf xt yt)
			   (get-bounding-rect elem)
			 (let ((rect (make-rectangle* (- xf +br-fix-inc+)
						      (- yf +br-fix-inc+)
						      (+ xt +br-fix-inc+)
						      (+ yt +br-fix-inc+))))
			   (when (region-contains-position-p
				  rect x y)
			     (change-class object 
					   (cond ((typep object 'start-handle)
						  'fixed-start-handle)
						 ((typep object 'end-handle)
						  'fixed-end-handle)
						 ((typep object 'object-handle)
						  'fixed-object-handle)))
			     (only-tick-object parent-object)
			     (setf (fixed-at-object object) elem)				    
			     (push object
				   (attached-handles elem)))))))
		 liste)
	   (notify-user gened-frame
			"Not in Any Bounding-Rectangle!"))))))))

(define-gened-command (com-gened-fix-handle :name "Fix Object-Handle")
    ()
  (if (any-unfixed-handles)
      (let* ((source-object (accept-handle 'unfixed-object-handle '(normal start end))))
	(fix-handle source-object)
	(only-tick-all-objects)
	(redraw))))

(define-gened-command (com-fix-object-handle)
    ((object 'unfixed-object-handle :gesture :fix/free))
  (fix-handle object))

;;;
;;;
;;;

(defmethod free-handle ((object fixed-object-handle))
  (let* ((fixed-at-object (fixed-at-object object))
	 (attached-handles (attached-handles fixed-at-object)))
    (setf (attached-handles fixed-at-object)
      (delete object attached-handles))
    (setf (fixed-at-object object) nil)
    (only-tick-object (parent-object object))
    (change-class object 
		  (cond ((typep object 'fixed-start-handle) 'unfixed-start-handle)
			((typep object 'fixed-end-handle) 'unfixed-end-handle)
			((typep object 'fixed-object-handle) 'unfixed-object-handle)))))

(defmethod free-all-handles ((object t))
  ())

(defmethod free-all-handles ((object thing))
  (dolist (handle (attached-handles object))
    (free-handle handle)))

(defmethod free-my-handles ((object t))
  ())

(defmethod free-my-handles ((object object-with-handles))
  (dolist (handle (handles object))
    (if (and (typep handle 'fixed-object-handle)
	     (fixed-at-object handle))
	(free-handle handle))))

;;;
;;;
;;;

(define-gened-command (com-gened-free-handle :name "Free Object-Handle")
    ()  
  (if (any-fixed-handles)
      (let* ((source-object (accept-handle 'fixed-object-handle '(fixed))))
	(free-handle source-object))))

(define-gened-command (com-free-object-handle)
    ((object 'fixed-object-handle :gesture :fix/free))
  (free-handle object)
  (only-tick-all-objects)
  (redraw))

;;;
;;;
;;;

(defmethod translate-fixed-handles-relative ((object thing) deltax deltay)
  (when (attached-handles object)
    (let* ((handles (attached-handles object)))
      (dolist (elem handles)
	(change-object-handle-relative elem
				       deltax deltay)))))
	 	 
(defmethod change-object-handle ((object object-handle) x y) ; x, y : screen-coordinates
  (multiple-value-bind (newx newy)
      (transform-point (parent-object object) x y :inverse t)
    (let ((oldx (x object))
	  (oldy (y object)))
      (setf (x object) newx)
      (setf (y object) newy)
      (search-and-change (parent-object object) newx newy oldx oldy))))

 
(defmethod change-object-handle-relative ((object object-handle) deltax deltay)
  (let ((oldx (x object))
	(oldy (y object)))
    (incf (x object) deltax)
    (incf (y object) deltay)
    (search-and-change (parent-object object) (x object) (y object) oldx oldy)))


;;;
;;;
;;;

(defmethod make-object-handles ((object t) pointlist)
  (declare (ignore pointlist))
  ())

(defmethod make-object-handles ((object object-with-handles) pointlist)
  (setf
      (handles object)
    (loop for elem in pointlist
	collect
	  (make-instance 
	      (cond ((equal elem (first pointlist)) 'unfixed-start-handle)
		    ((equal elem (first (last pointlist))) 'unfixed-end-handle)
		    (t 'unfixed-object-handle))
	    :x (first elem) 
	    :y (second elem) 
	    :parent-object object))))
 
