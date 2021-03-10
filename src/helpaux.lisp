;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defun calc-right-tres ()
  (/ +tres-touch+ *global-scaling*))

;;;
;;;
;;;

(defun get-id-number ()
  (incf *id-number*))

(defun min-max (liste)
  (values
   (apply #'min liste)
   (apply #'max liste)))

(defun find-it (liste fn)
  (mapcan #'(lambda (x)
	      (if (funcall fn x)
		  (list x)))
	  liste))

;;;
;;;
;;;

(defun interactive-master-creator (object updater-function 
				   &key (drawer-function #'(lambda (object stream) 
							     (draw object stream
								   :handling-active t))))
							    
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display)))
      (with-output-recording-options (stream :draw t :record nil)	
      
	(funcall drawer-function object stream)
	
	(block track-pointer
	  
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			     
			     (multiple-value-bind (x y)
				 (scale-mouse-position x y :inverse t)
			       
			       (funcall drawer-function object stream)
			       (funcall updater-function x y)
			       (funcall drawer-function object stream)))
	    
	      
	    (:pointer-button-press (event)
				   (funcall drawer-function object stream)
				   (return-from track-pointer event))))))))
				
;;;
;;;
;;;

(defun generate-brackets (liste &optional (akku '()))
  (cond ((null liste) (reverse akku))
	(t (generate-brackets 
	    (cddr liste)
	    (cons 
	     (list (first liste) (second liste))
	     akku)))))

(defun normalize-pointlist (pointlist)
  (let ((xlist)
	(ylist))
  
    (dolist (elem pointlist) 
      (push (first elem) xlist)
      (push (second elem) ylist))

    (multiple-value-bind (xf xt) (min-max xlist)
      (multiple-value-bind (yf yt) (min-max ylist)
	
	(let* ((xtrans  (/ (+ xf xt) 2))
	       (ytrans  (/ (+ yf yt) 2))
	       (translist	  
		(mapcan #'(lambda (x y)
			    (list 
			     (- x xtrans) 
			     (- y ytrans)))
			xlist ylist)))
	  
	  (values translist xtrans ytrans))))))

(defun draw-marker* (stream pointlist size &key (ink +foreground-ink+) (line-thickness 1)
						(line-dashes nil))
  (loop
    (let ((x (first pointlist))
	  (y (second pointlist)))
      (draw-line* stream (- x size)
		  (- y size)
		  (+ x size)
		  (+ y size)
		  :ink ink
		  :line-dashes line-dashes
		  :line-thickness line-thickness)
      (draw-line* stream (- x size)
		  (+ y size)
		  (+ x size)
		  (- y size)
		  :ink ink
		  :line-dashes line-dashes
		  :line-thickness line-thickness)
      
      (setf pointlist (cddr pointlist))
      (if (null pointlist) (return)))))
      
;;;
;;;
;;;

(defun any-?-objects (function text-string)
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (let ((test (some function
			liste)))
	(if (not test) 
	    (notify-user gened-frame
			 text-string
			 :style :warning))
	test))))


(defun any-fixed-handles ()
  (any-?-objects #'attached-handles "No fixed handles at all!"))


(defun any-unfixed-handles ()
  (any-?-objects #'(lambda (elem)
		     (if (typep elem 'object-with-handles)		    		  
			 (not (every #'(lambda (handle)
					 (typep handle 'fixed-object-handle))			  
				     (handles elem)))))
		 "No unfixed handles at all!"))

(defun any-visible-objects ()
  (any-?-objects #'(lambda (x) 
		     (not (hiddenp x)))
		 "No visible objects at all!"))

(defun any-visible-directed-objects ()
  (any-?-objects #'(lambda (x) 
		     (and (not (hiddenp x))
			  (typep x 'directed-info-element)
			  (head x)))
		 "No visible directed objects at all!"))

(defun any-invisible-objects ()
  (any-?-objects #'hiddenp
		 "No invisible objects at all!"))

(defun any-visible-clusters ()
  (any-?-objects #'(lambda (x)
		     (and (typep x 'composite-thing)
			  (not (hiddenp x))))
		 "No visible clusters at all!"))

(defun any-invisible-clusters ()
  (any-?-objects #'(lambda (x)
		     (and (typep x 'composite-thing)
			  (hiddenp x)))
		 "No invisible clusters at all!"))

(defun any-scaleable-objects ()
  (any-?-objects #'(lambda (x)
		     (and (typep x 'scaleable-thing)
			  (or (and (hiddenp x) (member 'hidden *global-display-options*))
			      (and (not (hiddenp x)) (member 'visible *global-display-options*)))))
		 "No scaleable objects at all!"))

(defun any-rotateable-objects ()
  (any-?-objects #'(lambda (x)
		     (and (typep x 'rotateable-thing)
			  (or (and (hiddenp x) (member 'hidden *global-display-options*))
			      (and (not (hiddenp x)) (member 'visible *global-display-options*)))))
		 "No rotateable objects at all!"))

(defun any-visible-2d-objects ()
  (any-?-objects #'(lambda (x)
		     (and (typep x '2d)
			  (not (hiddenp x))))
		 "No visible 2d-objects at all!"))
		     
