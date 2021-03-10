
;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod create ((object g-point) (orgx integer) (orgy integer))
  (init-object-properties object))


(defmethod create ((object g-rectangle) (orgx integer) (orgy integer))
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))
  (loop
    (interactive-master-creator object 
				#'(lambda (nowx nowy) 
				    (with-slots (xtrans ytrans
						 xextend yextend) object
				      (let ((dx  (/ (- nowx orgx) 2))
					    (dy  (/ (- nowy orgy) 2)))
									      
					(setf xtrans (/ (+ nowx orgx) 2)
					      ytrans (/ (+ nowy orgy) 2)
					      xextend dx
					      yextend dy)))))
    (if (object-ok-p object)
	(return)
      (beep)))
  (init-object-properties object))


(defmethod create ((object linesegment) (orgx integer) (orgy integer))
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))
  (loop 
    (interactive-master-creator object 
				#'(lambda (nowx nowy)
				    (with-slots (xtrans ytrans startpoint endpoint) object
				      
				      (let ((dx  (/ (- nowx orgx) 2))
					    (dy  (/ (- nowy orgy) 2)))
					
					(setf xtrans   (/ (+ nowx orgx) 2)
					      ytrans   (/ (+ nowy orgy) 2)
					      startpoint (list (- dx) (- dy))
					      endpoint (list dx dy))))))
    (if (object-ok-p object)
	(return)
      (beep)))
  (init-object-properties object))


(defmethod create ((object g-text) (orgx integer) (orgy integer))
  (let ((stream (get-frame-pane *application-frame* 'display)))
    (loop
      (setf (text-string object)
	(accepting-values (stream :own-window t :label "Enter Text:")
	  (accept 'string :stream stream :view 'gadget-dialog-view
		  :default "Text" :default-type 'string)))
      (if (object-ok-p object)
	  (return)
	(beep)))
    (init-object-properties object)))


(defmethod create ((object g-circle) (orgx integer)  (orgy integer))
  (when *info*
    (format t "~%Press Left Mouse-Button To Release!~%"))
  (loop 
    (interactive-master-creator object 
				#'(lambda (nowx nowy)
				    (with-slots (xtrans ytrans radius) object
				      
				      (let* ((dx (- nowx orgx))
					     (dy (- nowy orgy))
					     (d (round
						 (sqrt 
						  (+ (* dx dx) (* dy dy))))))
					
					(setf xtrans orgx
					      ytrans orgy
					      radius d)))))
    (if (object-ok-p object)
	(return)
      (beep)))
  (init-object-properties object))

(defmethod create ((object pointlist-object) (orgx integer) (orgy integer))
  (when *info*
    (format t "~%Press Left Mouse-Button To Fix Point,~%         Middle Button To Delete Last Point,~%         Right Mouse-Button To Finish!~%"))
  
  (setf (pointlist object) '(0 0))
  (let ((templist (list (list orgx orgy) (list orgx orgy)))
	(stream (get-frame-pane *application-frame* 'display)))
	
    (with-output-recording-options (stream :draw t :record nil)	
      
      (flet ((draw-it ()
	       (draw object stream 
		     :handling-active t)
	       (transform-and-draw object stream
				   #'(lambda ()
				       (draw-marker* stream
						     (pointlist object) 
						     2
						     :ink +flipping-ink+
						     :line-thickness +mr-line-thickness+)))))
    
	  
	(draw-it)
	  
	(block track-pointer
	  
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			     (multiple-value-bind (x y)
				 (scale-mouse-position x y :inverse t)
		
			       (with-slots (pointlist xtrans ytrans) object

				 (draw-it)
			       	 
				 (setf (first templist)
				   (list x y))
			       
				 (multiple-value-bind (newtemplist newxtrans newytrans)
				     (normalize-pointlist templist)
				 
				   (setf xtrans newxtrans)
				   (setf ytrans newytrans)
				   (setf pointlist newtemplist)
				 
				   (draw-it)))))
	      
	    (:pointer-button-press (x y event)
				   (multiple-value-bind (x y)
				       (scale-mouse-position x y :inverse t)
		
				     (let ((button-event (pointer-event-button event)))	
				       
				       (if (or 
					    (and (not (member (list x y) (rest templist)
							      :test #'equal))
						 (object-ok-p object))					 
					    (eql button-event +pointer-middle-button+))
					   (cond
					    ((eql button-event +pointer-right-button+)
					     (if (pointlist-ok-p object)
						 (progn
						   (draw-it)
						   (init-object-properties object)
						   (return-from track-pointer))
					       (beep)))
					
					    ((and (eql button-event +pointer-middle-button+)
						  (> (length templist) 1))
					     (pop templist))
				       
					    ((eql button-event +pointer-left-button+)       
					     (push (list x y) templist))
					
					    (t
					     (beep)))
					 (beep)))))))))))

;;;
;;;
;;;

(defun create-object (x y)
  (let ((*global-border* t))
    (multiple-value-bind (x y)
	(scale-mouse-position x y :inverse t)
      (with-application-frame (gened-frame)
	(with-slots (liste mode concept-type) gened-frame
	  (if (not (eq mode 'g-concept))
	      (let ((object (make-instance 
				mode
			      :xtrans x :ytrans y :initialize t)))
		(push object liste)
		(create object x y)				
		(make-undo-object object 'create))
	    
	    (if (null concept-type)
		(notify-user gened-frame  
			     "Select a concept first!"
			     :style :error)		 
	      (let* ((object 
		      (get-nth-visualisation-of-concept (first concept-type)
							(second concept-type)))
		     (object2 
		      (make-instance (type-of object) :initialize nil)))
		
		(copy-values object object2)
		
		(translate-object object2 x y)				

		(push object2 liste)
		(make-poly-representation object2)		
		(make-undo-object object2 'create))))))))
  
  (do-incremental-updates))


(define-gened-command (com-create-object :name "Create Object")
    ((x 'integer) (y 'integer))
  (create-object x y))

(define-presentation-to-command-translator create
    (blank-area com-create-object gened
		:gesture :create)		   
  (x y)
  (list x y))
