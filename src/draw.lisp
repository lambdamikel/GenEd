;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;; 


(defmethod transform-and-draw ((object basic-thing) stream drawer-function)
  (let* ((comptrans (get-transformation object))
	 (gstrans (make-scaling-transformation  *global-scaling*
					        *global-scaling*))
	 (trans (compose-transformations gstrans comptrans)))
    (with-drawing-options (stream :transformation trans)
      (funcall drawer-function))))

(defmethod transform-and-draw ((object composite-thing) stream drawer-function)
  (declare (ignore stream))
  (funcall drawer-function))

(defmethod transform-point ((object basic-thing) x y &key (inverse nil))    
  (let ((comptrans (get-transformation object))) 
    (multiple-value-bind (x y)
	(if inverse 
	    (untransform-position comptrans x y)
	  (transform-position comptrans x y))
      (values x y))))

(defun scale-mouse-position (x y &key (inverse nil))
  (if inverse
      (values (round (/ x *global-scaling*)) (round (/ y *global-scaling*)))
    (values (round (* x *global-scaling*)) (round (* y *global-scaling*)))))

(defmethod get-transformation ((object basic-thing))
  (let* ((scaletrans (if (typep object 'scaleable-thing)
			 (make-scaling-transformation (xscale object)
						      (yscale object))
		       +identity-transformation+))
	 (transtrans (make-translation-transformation
		      (xtrans object)
		      (ytrans object)))
	 
	 (rottrans (if (typep object 'rotateable-thing)
		       (make-rotation-transformation 
			(rotangle object))
		     +identity-transformation+))
	 
	 (comptrans 
	  (compose-transformations 
	   transtrans
	   (compose-transformations
	    rottrans
	    scaletrans))))
    comptrans))

(defmethod get-transformation ((object composite-thing))  
  +identity-transformation+)
    
;;;
;;;
;;;


(defmethod output-draw-object-handles ((object object-with-handles) stream &key
				       &allow-other-keys)
  (transform-and-draw   
   object stream 
   #'(lambda () 		       
       (let ((handles (handles object)))
	 (dolist (elem handles)
	   (with-output-as-presentation
	       (stream elem (type-of elem) :single-box :position) 
	     (draw elem stream)))))))

	
(defmethod draw-object-handles ((object object-with-handles) stream &key 
								    (handling-active nil)
								    (printing-active nil)
				&allow-other-keys)
  (transform-and-draw
   object stream 
   #'(lambda () 		       			
       (let ((handles (handles object)))
	 (dolist (elem handles)
	   (draw elem stream 
		 :handling-active handling-active
		 :printing-active printing-active))))))


(defmethod draw ((object object-handle) stream &key 
					       (handling-active nil)
					       (printing-active nil)
		 &allow-other-keys)

    (if (or (member 'normal *handles-symbol-list*)
	    (and (member 'unfixed *handles-symbol-list*)
		 (typep object 'unfixed-object-handle))
	    (and (member 'fixed *handles-symbol-list*)
		 (typep object 'fixed-object-handle)))

	(let ((x (x object))
		(y (y object)))
	    (multiple-value-bind (ink1 ink2 pattern)
		(get-right-inks object 
				:handling-active handling-active
				:printing-active printing-active)
	      (declare (ignore ink2))
	    
	      (draw-rectangle* stream
			     (- x *handle-space*)
			     (- y *handle-space*)
			     (+ x *handle-space*)
			     (+ y *handle-space*)
			     :ink ink1
			     :filled (typep object 'fixed-object-handle)
			     :line-dashes pattern)))))
			     

(defmethod draw ((object start-handle) stream &key 
					      (handling-active nil)
					      (printing-active nil)
		 &allow-other-keys)
  
  (if (or (member 'start *handles-symbol-list*)
	      (and (member 'fixed *handles-symbol-list*)
		   (typep object 'fixed-object-handle))
	      (and (member 'unfixed *handles-symbol-list*)
		   (typep object 'unfixed-object-handle)))
	  
	  (let ((x (x object))
		(y (y object)))
	    (multiple-value-bind (ink1 ink2 pattern)
		(get-right-inks object 
				:handling-active handling-active
				:printing-active printing-active)
	      
	      (draw-circle* stream
			    x y
			    (+ 2 *handle-space*)
			    :ink ink1 
			    :filled (typep object 'fixed-object-handle)
			    :line-dashes pattern)
	    
	      (draw-marker* stream (list x y) *handle-space*
			    :ink ink2
			    :line-dashes pattern)))))
    

(defmethod draw ((object end-handle) stream &key 
					    (handling-active nil)
					    (printing-active nil)
		 &allow-other-keys)
 
  (if (or (member 'end *handles-symbol-list*)
	  (and (member 'fixed *handles-symbol-list*)
	       (typep object 'fixed-object-handle))
	  (and (member 'unfixed *handles-symbol-list*)
	       (typep object 'unfixed-object-handle)))
	  	
      (let ((x (x object))
	    (y (y object)))
	(multiple-value-bind (ink1 ink2 pattern)
	    (get-right-inks object 
			    :handling-active handling-active
			    :printing-active printing-active)
	    
	  (draw-rectangle* stream
			   (- x *handle-space* 1)
			   (- y *handle-space* 1)
			   (+ x *handle-space* 1)
			   (+ y *handle-space* 1)
			   :ink ink1
			   :filled (typep object 'fixed-object-handle)
			   :line-dashes pattern)
	      
	  (draw-marker* stream (list x y) *handle-space*
			:ink ink2 
			:line-dashes pattern)))))


;;;
;;;
;;;

(defmethod output-draw ((object composite-thing) stream 
			&key
			&allow-other-keys)

    (with-output-as-presentation
	(stream object 'composite-thing :single-box t)
      (draw object stream)))
	  

(defmethod output-draw ((object basic-thing) stream 
			&key 
			&allow-other-keys)
  (let ((*handles-symbol-list* nil))
    (with-output-as-presentation 
	(stream object (type-of object) :single-box :position)
      (draw object stream))))  

(defmethod output-draw :after ((object object-with-handles) stream
			       &key 
			       &allow-other-keys)
  (output-draw-object-handles object stream))

;;;
;;;
;;;

(defmethod draw :after ((object object-with-handles) stream  &key 
							     (handling-active nil)
							     (printing-active nil)
			&allow-other-keys)
						
  (if (not (null *handles-symbol-list*))
      (draw-object-handles object stream 
			   :handling-active handling-active
			   :printing-active printing-active)))


(defmethod draw :after ((object thing) stream &key
					      (handling-active nil)
					      (printing-active nil)
			&allow-other-keys)
  
  (let ((xtrans (xtrans object))
	(ytrans (ytrans object)))
    
    (multiple-value-bind (ink1)
	(get-right-inks object 
			:handling-active handling-active
			:printing-active printing-active)

      (if (or *bounding-boxes* (highlightedp object))
	  (multiple-value-bind (left top right bottom)
	      (get-bounding-rect object :handling-active handling-active)  
	    
	    (draw-rectangle* stream
			     (- left +br-space+) (- top +br-space+)
			     (+ right +br-space+) (+ bottom +br-space+)
			     :filled nil
			     :line-dashes 
			     (if (highlightedp object)
				 nil
			       (if handling-active +br-line-dashes2+ +br-line-dashes+))
			     :line-thickness (if (highlightedp object) 
						 +br-highlight-thickness+
						 +br-line-thickness+)
			     :ink ink1)))       

      (if *concept-labels*
	  
	  (with-scaling (stream *global-scaling* *global-scaling*)
	    
	    (let ((count 0)
		  (ancestor-concepts (cons '* (ancestor-concepts object)))
		  (parent-concepts (cons '** (parent-concepts object))))
		
	      (dolist (concepts (append (when (and (not (null parent-concepts))
						   (or
						    (member 'parents *concept-labels*)
						    (equal '(ids) *concept-labels*)))
					  (list parent-concepts))
					(when (and (member 'ancestors *concept-labels*)
						   (not (null ancestor-concepts)))
					  (list ancestor-concepts))))
		
		(with-text-style (stream (if (eq (first concepts) '*)
					     +labels-text-style2+
					   +labels-text-style+))
		  
		  (let ((concepts 	       
			 (if (equal *concept-labels* '(ids))
			     '(ID)
			   (rest concepts))))
		    
		    (dolist (concept concepts)
		      
		      (draw-text* 
		       stream 
		       (if (and (zerop count)
				(member 'ids *concept-labels*))
			   (concatenate 'string 
			     (generate-name concept)
			     " "
			     (write-to-string (id-number object)))
			 (generate-name concept))
		       (- xtrans *org-space* 3)
		       (- (- ytrans *org-space* 3) 
			  (/ (* (incf count) +labels-text-height+) *global-scaling*))
		       
		       :ink ink1))))))))
      
      (if *origins*
	    
	  (with-scaling (stream *global-scaling* *global-scaling*)
	      
	    (draw-rectangle* stream
			     (- xtrans *org-space*)
			     (- ytrans *org-space*)
			     (+ xtrans *org-space*)
			     (+ ytrans *org-space*)
			     :ink ink1			     
			     :filled nil
			     :line-dashes 
			     (if handling-active +origin-dashes+))
	      
	    (draw-marker* stream (list xtrans ytrans) *org-space* 
			  :ink ink1
			  :line-dashes 
			  (if handling-active +origin-dashes+)))))))
  


;;;
;;;
;;;

(defmacro draw-good-arrow* (xs ys xe ye)
  `(draw-arrow*
    stream
    ,xs ,ys
    ,xe ,ye
    
    :ink (if handling-active 
	     +flipping-ink+
	   (make-gray-color (ink object)))
    
    :line-thickness
    (if handling-active 
	1 
      (+ (linethickness object)
	 (if printing-active +printing-inc+ 0)))
    
    :line-dashes (if handling-active
		     +dash-pattern+
		   (linestyle object))
    
    :head-width (if (head object)
		    (nth (1- (linethickness object))
			 '(6 9 13 18))
		  0)
    :head-length (if (head object) 
		     (nth (1- (linethickness object)) 
			  '(6 9 15 22))
		   0)))


(defmacro draw-good-line* (xs ys xe ye)
  `(draw-line*
    stream
    ,xs ,ys
    ,xe ,ye
    
    :ink (if handling-active 
	     +flipping-ink+
	   (make-gray-color (ink object)))
    
    :line-thickness
    (if handling-active 
	1 
      (+ (linethickness object)
	 (if printing-active +printing-inc+ 0)))
    
    :line-dashes (if handling-active
		     +dash-pattern+
		   (linestyle object))))

;;;
;;;
;;;

(defun draw-pointlist (object pointlist stream &key (printing-active nil)
						    (handling-active nil))
  (if *pretty-hl*
      (transform-and-draw object
			  stream 
			  #'(lambda ()
			      (when
				  *global-border* 				   
				(let* ((points (butlast pointlist 2))
				       (end (last pointlist 2))
				       (lastx)
				       (lasty))
				  
				  (loop for x in points by #'cddr 
				      for y in (cdr points) by #'cddr do
				    
					(if (and lastx lasty)
					    
					    (draw-line*
					     stream 
					     lastx lasty
					     x y
				       
					     :ink (if handling-active 
						      +flipping-ink+
						    (make-gray-color (ink object)))
					     :line-thickness
					     (if handling-active 
						 1 
					       (+ (linethickness object)
						  (if printing-active +printing-inc+ 0)))
					 
					     :line-dashes (if handling-active
							      +dash-pattern+
							    (linestyle object))))
				    
					(setf lastx x
					      lasty y)
				    
				      finally
				    
					(if (head object)
					    (draw-good-arrow* 
					     (or lastx (first pointlist))
					     (or lasty (second pointlist))
					     (first end) (second end))
					  (draw-good-line* 
					   (or lastx (first pointlist))
					   (or lasty (second pointlist))
					   (first end) (second end)))
					)))))


    (transform-and-draw object
			stream 
			#'(lambda ()
			    (when
				*global-border* 				   
			      (let* ((points (butlast pointlist 2))
				     (end (last pointlist 4)))
			      
				(if points
				    (draw-polygon*
				     stream 
				     points				       
				     :ink (if handling-active 
					      +flipping-ink+
					    (make-gray-color (ink object)))
				     :line-thickness
				     (if handling-active 
					 1 
				       (+ (linethickness object)
					  (if printing-active +printing-inc+ 0)))
				   
				     :line-dashes (if handling-active
						      +dash-pattern+
						    (linestyle object))
				     :filled nil
				     :closed nil))				    
				(when (= (length end) 4)
				  (if (head object)
				      (draw-good-arrow* 
				       (first end)
				       (second end)
				       (third end)
				       (fourth end))
				    (draw-good-line* 
				     (first end)
				     (second end)
				     (third end)
				     (fourth end))))))))))

;;;
;;;
;;;

(defmethod draw ((object composite-thing) stream &key
						 (handling-active nil)
						 (printing-active nil)
		 &allow-other-keys)

    (transform-and-draw object stream 
			#'(lambda ()
			    (with-slots (liste) object
			      (let ((*bounding-boxes* nil)
				    (*concept-labels* nil)
				    (*origins* nil)
				    (*handles-symbol-list* nil))
				(dolist (elem liste)
				  (draw elem stream 
					:handling-active handling-active
					:printing-active printing-active)))))))

;;;
;;;
;;;

(defmethod draw ((object g-circle) stream &key 
					(handling-active nil)
					(printing-active nil)
		 &allow-other-keys)
  
  (if (and (not handling-active) (polygonized-representation object))
      (let ((points (get-points (polygonized-representation object))))

	(if (and (filledp object) *global-filled*)
	    (draw-polygon* stream
			   points
			   :ink (make-gray-color (filled-ink object))
			   :filled t))
	
	(if *global-border* 
	    (draw-polygon* stream
			   points
			   :ink (make-gray-color (ink object))
			   :filled nil
			   :line-thickness 
			   (+ (linethickness object)
			      (if printing-active +printing-inc+ 0))
			   :line-dashes (linestyle object))))
    
    (transform-and-draw object stream 
			#'(lambda ()
			    
			    (if (and (not handling-active) (filledp object) *global-filled*)
				(draw-circle* stream 0 0 (radius object) 
					      :ink (make-gray-color (filled-ink object))
					      :filled t))
			    
			    (if *global-border* 
				(draw-circle* stream 0 0 (radius object) 
					      :ink (if handling-active 
						       +flipping-ink+
						     (make-gray-color (ink object)))
					      :filled nil
					      :line-thickness 
					      (if handling-active 
						  1 
						(+ (linethickness object)
						   (if printing-active +printing-inc+ 0)))
					      :line-dashes (if handling-active
							       +dash-pattern+
							     (linestyle object))))))))


(defmethod draw ((object g-rectangle) stream &key 
					(handling-active nil)
					(printing-active nil)
		 &allow-other-keys)
  
  (if (and (not handling-active) (polygonized-representation object))
      (let ((points (get-points (polygonized-representation object))))

	  (if (and (filledp object) *global-filled*)
	      (draw-polygon* stream
			     points
			     :ink (make-gray-color (filled-ink object))
			     :filled t))
	  
	  (if *global-border* 
	      (draw-polygon* stream
			     points
			     :ink (make-gray-color (ink object))
			     :filled nil
			     :line-thickness 
			     (+ (linethickness object)
				(if printing-active +printing-inc+ 0))
			     :line-dashes (linestyle object))))

    
    (transform-and-draw object stream 
			#'(lambda ()
			  
			    (if *global-border*
				(draw-polygon* stream 
					       (append
						(list (- (xextend object)) (yextend object))
						(list (xextend object) (yextend object))
						(list (xextend object) (- (yextend object)))
						(list (- (xextend object)) (- (yextend object))))
					       :ink (if handling-active 
							+flipping-ink+
							  (make-gray-color (ink object)))
					       :filled nil
					       :closed t
					       :line-thickness 
					       (if handling-active 
						   1 
						 (+ (linethickness object)
							(if printing-active +printing-inc+ 0)))
					       :line-dashes (if handling-active
								+dash-pattern+
							      (linestyle object))))))))

(defmethod draw ((object g-text) stream &key
				      (handling-active nil)
		 &allow-other-keys)

    (let ((text-style
	   (parse-text-style (list
			      (family object)
			      (face object)
			      (size object)))))
	
      (transform-and-draw object stream 
			  #'(lambda ()			    
			      (with-text-style (stream text-style)
				(draw-text* 
				 stream 
				 (text-string object)
				 0 0 
				 :ink (if handling-active
					  +flipping-ink+
					(make-gray-color (ink object)))))))))
				   

(defmethod draw ((object g-point) stream &key
					  (handling-active nil)
					  (printing-active nil)
		 &allow-other-keys)
  

    (transform-and-draw object stream
			#'(lambda ()
			    (draw-circle* stream 0 0 
					  (* 2 (linethickness object))
					  :ink (if handling-active 
						   +flipping-ink+
						 (make-gray-color (ink object)))
					  :filled t
					  :line-thickness 
					  (if handling-active 
					      1 
					    (+ (linethickness object)
					       (if printing-active +printing-inc+ 0)))))))

(defmethod draw ((object g-arrow) stream &key
				       (handling-active nil)
				       (printing-active nil)
		 &allow-other-keys)

    (let ((startpoint (startpoint object))
	  (endpoint (endpoint object)))
      (transform-and-draw object stream
			  #'(lambda ()
			      (when *global-border* 
				(if (head object)
				    (draw-good-arrow* (first startpoint) (second startpoint)
						      (first endpoint) (second endpoint))
				  (draw-line*
				   stream
				   (first startpoint) (second startpoint)
				   (first endpoint) (second endpoint) 
				   
				   :ink (if handling-active 
					    +flipping-ink+
					  (make-gray-color (ink object)))
				   
				   :line-thickness
				   (if handling-active 
				       1 
				     (+ (linethickness object)
					(if printing-active +printing-inc+ 0)))
				     
				   :line-dashes (if handling-active
						    +dash-pattern+
						  (linestyle object)))))))))	

(defmethod draw ((object g-chain) stream &key (handling-active nil)
					      (printing-active nil)
		 &allow-other-keys) 
  (draw-pointlist 
   object (pointlist object) stream 
   :printing-active printing-active
   :handling-active handling-active))

(defmethod draw ((object g-spline-chain) stream &key (handling-active nil)
						     (handle-handling-active nil)
						     (printing-active nil)
		 &allow-other-keys) 
  (draw-pointlist 
   object (if (or handle-handling-active
		  (null (spline-points object)))
	      (pointlist object)
	    (spline-points object))
   stream 
   :printing-active printing-active
   :handling-active handling-active))

(defmethod draw ((object g-spline-polygon) stream &key (handling-active nil)
						       (printing-active nil)
						       (handle-handling-active nil)
		 &allow-other-keys) 

  (transform-and-draw object
		      stream 
		      #'(lambda ()
			  (if (and (not handling-active) (filledp object) *global-filled*)
			      (draw-polygon*
			       stream 
			       (if (null (spline-points object))
				   (pointlist object)
				 (spline-points object))
						      				      
			       :ink (make-gray-color (filled-ink object))
			       :closed t
			       :filled t))
			  
			  (if *global-border* 
			      (draw-polygon*
			       stream 
			       (if (or handle-handling-active
				       (null (spline-points object)))
				   (pointlist object)
				 (spline-points object))			       
			       
			       :ink (if handling-active 
					+flipping-ink+
				      (make-gray-color (ink object)))
			       :line-thickness
			       (if handling-active 
				   1 
				 (+ (linethickness object)
				    (if printing-active +printing-inc+ 0)))
			       
			       :line-dashes (if handling-active
						+dash-pattern+
					      (linestyle object))
			       
			       :closed t 
			       :filled nil))))) 				   

(defmethod draw ((object g-polygon) stream &key
					   (handling-active nil)
					   (printing-active nil)
		 &allow-other-keys)
  (let ((pointlist (pointlist object)))
    (transform-and-draw object stream
			#'(lambda ()			    
			    (if (and (not handling-active) (filledp object) *global-filled*)
				(draw-polygon* stream
					       pointlist
					       :ink (make-gray-color (filled-ink object))
					       :filled t))				       			    
			    
			    (if *global-border*
				(draw-polygon* stream
					       pointlist
					       :ink (if handling-active 
							+flipping-ink+
						      (make-gray-color (ink object)))
					       :line-thickness
					       (if handling-active 
						   1 
						 (+ (linethickness object)
						    (if printing-active +printing-inc+ 0)))
					       
					       :line-dashes (if handling-active
								+dash-pattern+
							      (linestyle object))
					       
					       :filled nil))))))

;;;
;;;
;;;

(defmethod draw-gened ((frame gened) stream &key max-width max-height)  
  (declare (ignore max-width max-height))
  (with-slots (liste) frame
    (dolist (object (reverse liste))    
      (let ((visible 	
	     (and (not (hiddenp object)))))

	(when  (and 
		 (or
		  (and visible (member 'visible *global-display-options*))
		  (and (not visible) (member 'hidden *global-display-options*)))
		 
		 (or
		  (and (typep object 'basic-thing) (not (typep object 'composite-thing))
		       (member 'primitive *global-display-options*))
		  (and (typep object 'composite-thing)
		       (member 'composite *global-display-options*))))	 
		
            (updating-output (stream :unique-id object
				 :cache-value (tickval object))   
              (output-draw object stream)))))))
