;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod only-tick-object ((object thing))
  (incf (tickval object)))

(defun only-tick-all-objects ()
  (with-application-frame (gened-frame) 
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(only-tick-object elem)))))

(defmethod tick-object ((object thing))
  (incf (tickval object))  
  (make-poly-representation object))

(defun tick-all-objects ()
  (with-application-frame (gened-frame) 
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(tick-object elem)))))

(defun tick-all-text-objects ()
  (with-application-frame (gened-frame) 
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(when (typep elem 'g-text)
	  (tick-object elem))))))

;;;
;;;
;;;

(defmethod search-and-change ((object linesegment) newx newy oldx oldy)
  (let ((startpoint (startpoint object)))
    (if (and (= (first startpoint) oldx)
	     (= (second startpoint) oldy))
	(setf (startpoint object) (list newx newy))
      (setf (endpoint object) (list newx newy)))))

(defmethod search-and-change ((object pointlist-object) newx newy oldx oldy)
  (let ((pointlist (pointlist object)))	
    (setf (pointlist object)
      (loop for x in pointlist by #'cddr
	  for y in (rest pointlist) by #'cddr append
	    (if (and (= x oldx)
		     (= y oldy))
		(list newx newy)
	      (list x y))))))

;;;
;;;
;;;

(defmethod adjust-object-origin ((object t))
  ())

(defmethod adjust-object-origin ((object linesegment))
  (let ((startpoint (startpoint object))
	(endpoint (endpoint object)))
    
    (only-tick-object object)
    (multiple-value-bind (newstartx newstarty)
	(transform-point object (first startpoint) (second startpoint))
      (multiple-value-bind (newendx newendy)
	  (transform-point object (first endpoint) (second endpoint)) 
	(multiple-value-bind (newlist orgx orgy)
	    
	    (normalize-pointlist 
	     (list (list newstartx newstarty)
		   (list newendx newendy)))
	  
	  (let ((newlist (generate-brackets newlist)))
	    (setf (xtrans object) orgx
		  (ytrans object) orgy
		  (startpoint object) (second newlist)
		  (endpoint object) (first newlist)
		  (rotangle object) 0)
	    
	    (setf (x (first (handles object)))
	      (- newstartx orgx))
	    (setf (y (first (handles object)))
	      (- newstarty orgy))
	    
	    (setf (x (second (handles object)))
	      (- newendx orgx))
	    (setf (y (second (handles object)))
	      (- newendy orgy))))))))
	    

(defmethod adjust-object-origin ((object pointlist-object))
  (let ((pointlist (generate-brackets (pointlist object)))
	(collect ()))
    
    (only-tick-object object)
    (dolist (elem pointlist)
      (multiple-value-bind (newx newy)
	  (transform-point object (first elem) (second elem))
	(push (list newx newy) collect)))
    
    (multiple-value-bind (newlist orgx orgy)
	(normalize-pointlist collect)
      (let ((nested-newlist (generate-brackets newlist)))
	(setf (xtrans object) orgx
	      (ytrans object) orgy
	      (pointlist object) newlist)
	
	(if (typep object 'rotateable-thing)
	    (setf (rotangle object) 0))
	(if (typep object 'scaleable-thing)
	    (setf
		(xscale object) 1.0
		(yscale object) 1.0))
	
	(mapc #'(lambda (newpoint oldhandle)		
		  (setf (x oldhandle)
		    (first newpoint)) 
		  (setf (y oldhandle)
		    (second newpoint)))
	      nested-newlist 
	      (handles object))))))

(defmethod adjust-object-origin ((object spline-object))
  (only-tick-object object)
  (call-next-method)
  (init-object-properties object))


;;;
;;;
;;;

(defmethod adjust-all-attached-objects ((object t))
  (dolist (handle (attached-handles object))
    (adjust-object-origin (parent-object handle))
    (tick-object (parent-object handle))))

;;;
;;;
;;;

(defmethod adjust-parent-concept ((object g-arrow))
  (substitute-concept object 'g-arrow 'g-line))

(defmethod adjust-parent-concept ((object g-chain))
  (substitute-concept object 'g-directed-chain 'g-chain))

(defmethod adjust-parent-concept ((object g-spline-chain))
  (substitute-concept object 'g-directed-spline-chain 'g-spline-chain))

(defun substitute-concept (object from-concept to-concept)
  #+:classic
  (update-classic-ind-concepts object :remove t :clos-update nil)
  (if (not (head object))
      (progn
	(setf (parent-concepts object)
	  (subst to-concept from-concept
		  (parent-concepts object)))
	(setf (ancestor-concepts object)
	  (subst to-concept from-concept
		  (ancestor-concepts object))))	
    (progn
      (setf (parent-concepts object)
	(subst from-concept to-concept
		(parent-concepts object)))
      (setf (ancestor-concepts object)
	(subst from-concept to-concept
		(ancestor-concepts object)))))     
  #+:classic
  (update-classic-ind-concepts object :clos-update t)
  (only-tick-object object))


;;;
;;;
;;;
  
(defmethod get-right-inks ((object t) &key
				      (handling-active nil)
				      &allow-other-keys)
  (if handling-active 
      (values +flipping-ink+ +flipping-ink+
	      +dash-pattern+ 1)

    (values (make-gray-color 0.0)
	    (make-gray-color 0.0)
	    nil 1)))


(defmethod get-right-inks ((object fixed-object-handle) &key
							(handling-active nil)
							&allow-other-keys)
  (if handling-active 
      (values +flipping-ink+ +flipping-ink+
	      +dash-pattern+ 1)

    (values (make-gray-color 0.0)
	    (make-gray-color 1.0)
	    nil 1)))

;;;
;;;
;;;

(defmethod highlight-object ((object t))
  ())

(defmethod highlight-object ((object thing))
  (only-tick-object object)
  (setf (highlightedp object) t))

(defmethod unhighlight-object ((object t))
  ())

(defmethod unhighlight-object ((object thing))
  (only-tick-object object)
  (setf (highlightedp object) nil))

(defun unhighlight-all-objects ()
  (with-application-frame (gened-frame)    
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(unhighlight-object elem)))))

;;;
;;;
;;;

(defmethod get-bounding-rect ((object thing) &key (handling-active nil))
  (let ((stream (get-frame-pane *application-frame* 'display))
	(*bounding-boxes* nil)
	(*handles-symbol-list* nil)
	(*origins* nil)
	(*concept-labels* nil)
	(*global-border* t)
	(highlighted (highlightedp object)))
    
    (if highlighted (unhighlight-object object))
    
    (multiple-value-bind (xf yf xt yt)	
	(bounding-rectangle*
	 (with-output-to-output-record (stream)
	   (draw object stream :handling-active handling-active)))	  
      (with-slots (br-left br-right br-top br-bottom) object
	(setf br-left xf
	      br-right xt
	      br-top yf
	      br-bottom yt))
      (if highlighted (highlight-object object))
      
      (values xf yf xt yt))))

(defmethod get-bounding-rect-center ((object thing) &key (handling-active nil))
  (multiple-value-bind (xf yf xt yt)
      (get-bounding-rect object :handling-active handling-active)
    (values (/ (+ xf xt) 2) (/ (+ yf yt) 2))))

;;;
;;;
;;;

(defun refresh ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display)))
      (window-refresh stream))))
			
(defun redraw ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display)))
   (redisplay-frame-pane gened-frame stream))))

;;;
;;;
;;;

(defun new ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (clear-hashtable)
      (setf liste nil)
      (init-undo-buffer)
      (do-incremental-updates)
      (setf *current-file* "NONAME"))))

(define-gened-command (com-gened-new :name "New Scene")
    ()
  (with-application-frame (gened-frame)
    (let ((yes-or-no (notify-user gened-frame
				  "New Scene selected! Are you sure?"
				  :style :question)))
      (if yes-or-no
	  (new)))))

;;;
;;;
;;;

(define-gened-command (com-gened-exit :name "Exit Gened")
    ()
  (with-application-frame (gened-frame)
    (let ((yes-or-no (notify-user gened-frame
				  "Exit Gened selected! Are you sure?"
				  :style :question)))
      (if yes-or-no
	  (frame-exit gened-frame)))))

;;;
;;;
;;;

(defmethod to-front ((object thing))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (only-tick-all-objects)
      (setf liste (delete object liste))
      (push object liste))))	    

(define-gened-command (com-gened-to-front :name "Bring Object To Front")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(to-front source-object))))

(define-gened-command (com-bring-object-to-front)
    ((object 'thing :gesture nil))
  (to-front object))

;;;
;;;
;;;

(define-gened-command (com-gened-find-object :name "Find Object")
    ()
  (with-application-frame (gened-frame)    
    (with-slots (liste) gened-frame
      (let* ((stream (get-frame-pane gened-frame 'display))
	     (id-number
	      (accepting-values (stream :own-window t :label "Enter Object's ID-Number:")
		(accept 'integer :stream stream :view 'gadget-dialog-view
			:default *id-number*))))
	(find-object id-number)))))

(defun find-object (id-number)
  (let ((object (get-object id-number)))
    (highlight-object object)))

(defun get-object (id-number &key (saved-object-list nil))
  (with-application-frame (gened-frame)
    (with-slots (prev-op-liste liste) gened-frame
      (find-if #'(lambda (elem)
		   (when (= (id-number elem)
			    id-number)
		     elem))
	       (if saved-object-list
		   prev-op-liste
		 liste)))))

;;;
;;;
;;;

(define-gened-command (com-gened-unmark-all-objects :name "Unmark All Objects")
    ()
  (unhighlight-all-objects))


;;;
;;;
;;;

(defmethod to-back ((object thing))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (only-tick-all-objects)
      (setf liste (delete object liste)) 
      (setf liste (nconc liste (list object)))))) 	     

(define-gened-command (com-gened-to-back :name "Bring Object To Back")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(to-back source-object))))

(define-gened-command (com-bring-object-to-back)
    ((object 'thing :gesture nil))
  (to-back object))

;;;
;;;
;;;

(define-gened-command (com-gened-hide-object :name "Hide Object")
    ()
  (if (any-visible-objects)
      (let ((*global-border* t)
	    (*global-filled* t)
	    (*global-display-options* '(visible composite primitive)))
	(only-tick-all-objects)
	(redraw)
	(let* ((source-object (accept 'thing)))
	  (terpri)
	  (setf (hiddenp source-object) t)))))


(define-gened-command (com-hide-object)
    ((object 'thing :gesture nil))
  (setf (hiddenp object) t))

(define-gened-command (com-gened-show-object :name "Show Object")
    ()
  (if (any-invisible-objects)
      (let ((*global-border* t)
	    (*global-filled* t)
	    (*global-display-options* '(hidden composite primitive)))
	(only-tick-all-objects)
	(redraw)
	(let* ((source-object (accept 'thing)))
	  (terpri)
	  (setf (hiddenp source-object) nil)))))

(define-gened-command (com-show-object)
    ((object 'thing :gesture nil))
  (setf (hiddenp object) nil))


;;;
;;;
;;;

(defun save-object-list ()
  (with-application-frame (gened-frame)
    (with-slots (liste prev-op-liste) gened-frame
      (setf prev-op-liste (copy-list liste)))))

     

	  
	  

  
