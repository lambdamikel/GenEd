;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defun make-right-geom-polygon (object point-list &key (closed t))
  
  (delete-polyrep-from-cache object)
  (make-geom-polygon          
   (mapcar #'(lambda (point)
	       (multiple-value-bind (x y)
		   (let* ((xp (first point))
			  (yp (second point)))
		     (transform-point-gs object 
					 xp yp))
		 (list x y)))
	   point-list)   
   :is-closed closed
   :tickval nil))

;;;
;;;
;;;

(defmethod transform-point-gs ((object basic-thing) x y &key (inverse nil))    
  (let* ((comptrans (get-transformation object))
	 (gstrans (make-scaling-transformation *global-scaling*
					       *global-scaling*))
	 (trans (compose-transformations  gstrans comptrans)))
       
    (multiple-value-bind (x y)
	(if inverse 
	    (untransform-position trans x y)
	  (transform-position trans x y))
      (values  (round x) (round y)))))
;;;
;;;
;;;

(defmethod make-poly-representation ((object g-polygon))
  (with-slots (polygonized-representation pointlist) object
    (setf polygonized-representation
      (make-right-geom-polygon object
			       (generate-brackets pointlist)
			       :closed t))))

(defmethod make-poly-representation ((object g-chain))
  (with-slots (polygonized-representation pointlist) object
    (setf polygonized-representation
      (make-right-geom-polygon object
			       (generate-brackets pointlist)
			       :closed nil)))
  (call-next-method))

(defmethod make-poly-representation ((object g-spline-polygon))
  (with-slots (polygonized-representation pointlist) object
    (setf polygonized-representation
      (make-right-geom-polygon object
			       (generate-brackets (spline-points object))
			       :closed t))))

(defmethod make-poly-representation ((object g-spline-chain))
  (with-slots (polygonized-representation pointlist) object
    (setf polygonized-representation
      (make-right-geom-polygon object
			       (generate-brackets (spline-points object))
			       :closed nil)))
  (call-next-method))

(defmethod make-poly-representation ((object g-rectangle))
  (with-slots (xextend yextend polygonized-representation) object
    (setf polygonized-representation
      (make-right-geom-polygon object      
       (list 
	(list (- xextend) (- yextend))
	(list xextend (- yextend))
	(list xextend yextend)
	(list (- xextend) yextend))
       :closed t))))

(defmethod make-poly-representation ((object g-text))
  (with-slots (polygonized-representation) object
    (multiple-value-bind (xf yf xt yt)
	(get-bounding-rect object)	         	  	  
      (delete-polyrep-from-cache object)
      
      (setf polygonized-representation
	(make-geom-polygon   
	 (list 
	  (list xf yf)
	  (list xt yf)
	  (list xt yt)
	  (list xf yt))
	 :is-closed t
	 :tickval nil)))))

(defmethod make-poly-representation ((object g-point))
  (delete-polyrep-from-cache object)      
  (setf (polygonized-representation object)    
    (make-geom-point (xtrans object)
		     (ytrans object)
		     :tickval nil)))

(defmethod make-poly-representation ((object info-point))
  (delete-polyrep-from-cache object)
  (setf (polygonized-representation object)    
    (make-geom-point (xtrans object)
		     (ytrans object)
		     :tickval nil)))

(defmethod make-poly-representation ((object directed-info-element))
  (with-slots (start-info-point end-info-point) object
    (let* ((startpoint (startpoint object))
	   (endpoint (endpoint object)))

      (multiple-value-bind (xs ys)
	  (transform-point-gs object 
			   (first startpoint)
			   (second startpoint))
	(multiple-value-bind (xe ye)
	    (transform-point-gs object 
			     (first endpoint)
			     (second endpoint))
	
	  (let ((p1 (make-instance 'info-point 
		      :xtrans xs
		      :ytrans ys
		      :startpoint-of object
		      :initialize t))
		(p2 (make-instance 'info-point
		      :xtrans xe
		      :ytrans ye
		      :endpoint-of object
		      :initialize t)))
	  
	    (make-poly-representation p1)
	    (make-poly-representation p2)
	    
	    (when start-info-point
	      (setf (id-number p1) 
		(id-number start-info-point))
	      (when (associated-classic-ind start-info-point)
		(setf (associated-classic-ind p1)
		  (associated-classic-ind start-info-point))))
	      
	    (when end-info-point
	      (setf (id-number p2) (id-number end-info-point))
	      (when (associated-classic-ind end-info-point)
		(setf (associated-classic-ind p2)
		  (associated-classic-ind end-info-point))))
	     
	    (setf (start-info-point object) p1)
	    (setf (end-info-point object) p2)))))))

(defmethod make-poly-representation ((object linesegment))      
  (delete-polyrep-from-cache object)
  
  (with-slots (startpoint endpoint polygonized-representation) object
    (multiple-value-bind (xs ys)
	(transform-point-gs object 
			 (first startpoint)
			 (second startpoint))
      (multiple-value-bind (xe ye)
	  (transform-point-gs object 
			   (first endpoint)
			   (second endpoint))
	(setf polygonized-representation
	  (make-geom-line
	   (make-geom-point xs ys)
	   (make-geom-point xe ye)
	   :tickval nil)))))
  (call-next-method))

(defmethod make-poly-representation ((object g-circle))
  (with-slots (polygonized-representation radius) object
    (setf polygonized-representation
      (make-right-geom-polygon object
       (let ((fac (/ (* 2 pi) +circle-granularity+))
	     (collect nil))
	 (dotimes (i +circle-granularity+ collect)
	   (let ((rad (* i fac)))
	     (push (list (* radius (cos rad))
			 (* radius (sin rad)))
		   collect))))
	      
       :closed t))))  

(defmethod make-poly-representation ((object composite-thing))
  (with-slots (liste) object    
    (dolist (elem liste)
      (make-poly-representation elem))))

;;;
;;;
;;;

(defun get-poly-liste ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (atomize-all-clusters)
      (let* ((liste2 (install-points liste))
	     (poly-liste
	      (mapcan #'(lambda (x)
			  (when (typep x 'thing)
			    (let ((poly-rep
				   (when (slot-exists-p x 'polygonized-representation)
				     (polygonized-representation x))))
			      (if poly-rep (list poly-rep)))))
		      liste2)))
	
	(reinstall-all-clusters)
	poly-liste))))

(defmethod delete-polyrep-from-cache ((object basic-thing))
  (let ((poly-liste (get-poly-liste))
	(poly-elem (polygonized-representation object)))
    (call-next-method)
    (when poly-elem	
      (delete-object-from-cache poly-elem
				poly-liste))))

(defmethod delete-polyrep-from-cache ((object t))
  ())

(defmethod delete-polyrep-from-cache ((object directed-info-element))
  (let ((start-info-point (start-info-point object))
	(end-info-point (end-info-point object)))
    (when start-info-point
      (delete-polyrep-from-cache
       start-info-point))
    (when end-info-point
      (delete-polyrep-from-cache
       end-info-point))))
   

(defmethod delete-polyrep-from-cache ((object info-point))
  (let ((poly-liste (get-poly-liste))
	(poly-elem (polygonized-representation object)))
    (when poly-elem
      (delete-object-from-cache 
       poly-elem
       poly-liste))))

(defmethod delete-polyrep-from-cache ((object composite-thing))
  (dolist (elem (liste object))
    (delete-polyrep-from-cache elem)))
