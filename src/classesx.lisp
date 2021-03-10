;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;; Klassendefinitionen
;;;

(defclass undo-object ()
  ((object-copy :initarg :object-copy :accessor object-copy)
   (original-object :initarg :original-object :accessor original-object)
   
   (operation :initarg :operation :accessor operation)))

;;;

(defclass object-handle ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (parent-object :accessor parent-object :initarg :parent-object)))  

(defclass start-handle (object-handle)
  ())

(defclass end-handle (object-handle)
  ())

(defclass unfixed-object-handle (object-handle)
  ())

(defclass fixed-object-handle (object-handle)
  ((fixed-at-object :accessor fixed-at-object :initform nil)))
      
(defclass unfixed-start-handle (unfixed-object-handle start-handle)
  ())

(defclass unfixed-end-handle (unfixed-object-handle end-handle)
  ())
  
(defclass fixed-start-handle (fixed-object-handle start-handle)
  ())

(defclass fixed-end-handle (fixed-object-handle end-handle)
  ())

;;;
;;;
;;;

(defclass linestyle-mixin ()
  ((linestyle :initform nil :initarg :linestyle :accessor linestyle)))

(defclass ink-mixin ()
  ((ink :initform 0.0 :initarg :ink :accessor ink)))

(defclass linethickness-mixin ()
  ((linethickness :initform 1  :initarg :linethickness  :accessor linethickness)))

(defclass filled-mixin ()
  ((filledp :initform nil :initarg :filledp :accessor filledp)
   (filled-ink :initform 0.0 :initarg :filled-ink :accessor filled-ink)))
	    

(defclass sit-mixin (linestyle-mixin ink-mixin linethickness-mixin)
  ())


;;;
;;;
;;;


(defclass thing ()
  ((attached-handles :initform nil :accessor attached-handles)
   (tickval :initform 0 :accessor tickval)
    
   (associated-classic-ind :initform nil :accessor associated-classic-ind)             
    
   (initialize :initform nil :initarg :initialize 
		:accessor initialize)	; t, wenn initialize-instance taetig werden darf
   
   (xtrans :initform 0 :accessor xtrans :initarg :xtrans) ; rel. zum Screen-Origin
   (ytrans :initform 0 :accessor ytrans :initarg :ytrans) ; xytrans ist immer Mittelpunkt der bounding-box
   
   (parent-concepts :initform nil :accessor parent-concepts :initarg :parent-concepts)
   (ancestor-concepts :initform nil :accessor ancestor-concepts)
      
   (hidden :initform nil :accessor hiddenp :initarg :hiddenp)  
   (output-record :accessor output-record)
   
   (id-number :accessor id-number :initform 0)
   (highlighted :initform nil :accessor highlightedp)
   
   (br-left :accessor br-left)
   (br-right :accessor br-right)
   (br-top :accessor br-top)
   (br-bottom :accessor br-bottom)

   ))


(defclass basic-thing (thing)
  ((polygonized-representation :initform nil :accessor polygonized-representation)))

;;;
;;;
;;;

(defclass rotateable-thing () ; es gibt 2D-Objekte, die nicht rotierbar sind (g-circle)
  ((rotangle :initform 0 :accessor rotangle)))

(defclass scaleable-thing () ; auch 1D-Objekte koennen scalierbar sein, wenn sie nicht flach sind
  ((xscale :initform 1.0 :accessor xscale :initarg :xscale)    
   (yscale :initform 1.0 :accessor yscale :initarg :yscale)
   
   (init-x-extend :accessor init-x-extend)
   (init-y-extend :accessor init-y-extend)))

(defclass scale-and-rotateable-thing (scaleable-thing rotateable-thing)
  ())

;;;
;;;
;;;

(defclass object-with-handles ()
  ((handles :accessor handles :initarg :handles :initform nil)   
   (initialize-handles :accessor initialize-handles :initform nil
		       :initarg :initialize-handles)))

(defclass pointlist-object ()
  ((pointlist :initform nil :initarg :pointlist :accessor pointlist)))


(defclass spline-object ()
  ((spline-points :initform nil :accessor spline-points)))


(defclass object-with-head ()
  ((head :initform t :initarg :head :accessor head)))


(defclass linesegment ()
  ((startpoint :initform '(-10 -10) :initarg :startpoint :accessor startpoint)
   (endpoint   :initform '(10 10)   :initarg :endpoint   :accessor endpoint)))

(defclass directed-info-element ()  
  ((startpoint-related-with :initform nil :initarg :startpoint-related-with :accessor
			    startpoint-related-with)
   (endpoint-related-with :initform nil :initarg :endpoint-related-with :accessor	
			  endpoint-related-with)
   (start-info-point :initform nil :initarg :start-info-point :accessor start-info-point)
   (end-info-point :initform nil :initarg :end-info-point :accessor end-info-point)))
   
;;;
;;;
;;;

(defclass 0d ()
  ((part-of-cluster :initarg :part-of-cluster :initform nil :accessor part-of-cluster)
      
   (disjoint-with :initform nil :accessor disjoint-with)   
   
   (start-linked-over-with :initform nil :accessor start-linked-over-with) 
   (end-linked-over-with :initform nil :accessor end-linked-over-with)
   
   (start-linker-objects :initform nil :accessor start-linker-objects)
   (end-linker-objects :initform nil :accessor end-linker-objects)
   
   (in-relation-with-objects :initform nil :accessor in-relation-with-objects)       	     
   
   (intersects-objects :initform nil :accessor intersects-objects)
   (intersects-0-objects :initform nil :accessor intersects-0-objects)
   
   (touching-objects :initform nil :accessor touching-objects)
   (contained-in-objects :initform nil :accessor contained-in-objects)
   (covered-by-objects   :initform nil :accessor covered-by-objects)
   
   (directly-contained-by-object :initform nil :accessor 
				 directly-contained-by-object)))
     	
(defclass 1d (0d)
  ((intersects-1-objects :initform nil :accessor intersects-1-objects)))

   
(defclass 2d (1d)
  ((intersects-2-objects :initform nil :accessor intersects-2-objects)

   (contains-objects :initform nil :accessor contains-objects)
   (directly-contains-objects :initform nil :accessor directly-contains-objects)
   (covers-objects   :initform nil :accessor covers-objects)))

;;;
;;;
;;;


(defclass composite-thing (thing 2d) 
  (    
   (liste :initform nil :accessor liste :initarg :liste)))       


;;;
;;;
;;;

(defclass g-point (basic-thing 0d linethickness-mixin ink-mixin)
  ())

(defclass info-point (basic-thing 0d)
  ((part-of :initform nil :initarg :part-of :accessor part-of)))

(defclass g-circle (basic-thing 2d scaleable-thing sit-mixin filled-mixin) 
  ((radius :initform 0 :accessor radius :initarg :radius)))

(defclass g-rectangle (basic-thing 2d scale-and-rotateable-thing sit-mixin filled-mixin) 
  ((xextend :initform 10 :accessor xextend :initarg :xextend)
   (yextend :initform 10 :accessor yextend :initarg :yextend))) 

(defclass g-text (basic-thing 2d ink-mixin)
  ((text-string :initform "Text" :accessor text-string :initarg :text-string)
   (face :initform :bold :initarg :face :accessor face)
   (size :initform :large :initarg :size :accessor size)
   (family :initform :sans-serif :initarg :family :accessor family)))
   

(defclass g-polygon (basic-thing 2d scale-and-rotateable-thing 
		     object-with-handles pointlist-object sit-mixin
		   filled-mixin) 
  ())

(defclass g-arrow (basic-thing 1d rotateable-thing object-with-handles linesegment 		   
		   object-with-head sit-mixin
		   directed-info-element)
  ())

(defclass g-chain (basic-thing 1d rotateable-thing pointlist-object object-with-handles 
		   object-with-head sit-mixin
		   directed-info-element)
  ())

(defclass g-spline-chain (basic-thing spline-object 1d rotateable-thing 
			  pointlist-object object-with-handles 
			  object-with-head sit-mixin
			  directed-info-element)
  ())

(defclass g-spline-polygon (basic-thing spline-object 2d scale-and-rotateable-thing
			  pointlist-object object-with-handles sit-mixin filled-mixin)
  ())

;;;
;;;
;;;

(defmethod startpoint ((object pointlist-object))
  (list 
   (first (pointlist object))
   (second (pointlist object))))

(defmethod endpoint ((object pointlist-object))
  (last (pointlist object) 2))

;;;
;;;
;;;
			
			
(defmethod print-object ((object thing) stream)
  (format stream "~A = ~D"
	  (first (parent-concepts object))
	  (id-number object)))

(define-presentation-method present (object (type thing)
					    stream
					    (view textual-view)
					    &key)
  (print-object object stream))

(define-presentation-method accept ((type thing)
				    stream
				    (view textual-view)
				    &key)
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (completing-from-suggestions
       (stream)
       (dolist (elem liste)
	 (suggest (print-object elem) elem))))))

;;;
;;;
;;;

(defun strange-polygon (object)
  (let ((pointlist 
	 (generate-brackets (pointlist object))))
    (some #'(lambda (co1 co2 co3)
	      (let* ((x1 (first co1))
		     (y1 (second co1))
		     (x2 (first co2))
		     (y2 (second co2))
		     (x3 (first co3))
		     (y3 (second co3)))
		(multiple-value-bind (d1 phi1)
		    (distance-and-orientation x1 y1 x2 y2)		  
		  (multiple-value-bind (d2 phi2)
		      (distance-and-orientation x2 y2 x3 y3)
		    (zerop (- (+ phi1 phi2) pi))))))
	  pointlist
	  (cdr pointlist)
	  (cddr pointlist))))

(defmethod strange-polygon-p ((object g-polygon))
  (strange-polygon object))

(defmethod strange-polygon-p ((object g-spline-polygon))
  (strange-polygon object))

;;;
;;;
;;;

(defmethod self-intersecting-p ((object pointlist-object))
  (unless (<= (length (pointlist object)) 6)
    (let* ((pointlist 
	  (let ((brackets (generate-brackets (pointlist object))))	
	    (if (or (typep object 'g-spline-polygon)
		    (typep object 'g-polygon))    
		(append brackets (list (first brackets)))
	      brackets)))
	 (lines
	  (mapcar #'(lambda (point1 point2)
		      (let* ((p1 (make-geom-point (first point1)
						  (second point1)))
			     (p2 (make-geom-point (first point2)
						  (second point2))))
			(make-geom-line p1 p2)))
		  pointlist
		  (cdr pointlist))))
    
    (some #'(lambda (l1)
	      (let ((count
		     (count-if #'(lambda (l2)
				   (intersects l1 l2))
			       (remove l1 lines :test #'equal))))
		
		      
		(cond ((or (typep object 'g-polygon)
			   (typep object 'g-spline-polygon))
		       (> count 2))
		      (t
		       (or
			(and (eq l1 (first lines))
			     (> count 1))
			(and (eq l1 (first (last lines)))
			     (> count 1))
			(> count 2))))))

	  lines))))

;;;
;;;
;;;

(defmethod object-ok-p ((object g-polygon))
  (and
   (not (strange-polygon-p object))
   (not (self-intersecting-p object))))

(defmethod object-ok-p ((object g-spline-polygon))
  (and
   (not (strange-polygon-p object))
   (not (self-intersecting-p object))))

(defmethod object-ok-p ((object pointlist-object))
  (not (self-intersecting-p object)))

(defmethod object-ok-p ((object linesegment))
  (not (equal (startpoint object)
	      (endpoint object))))
    
(defmethod object-ok-p ((object g-text))
  (not (string= "" (text-string object))))

(defmethod object-ok-p ((object g-rectangle))
  (not (or (zerop (xextend object))
	   (zerop (yextend object)))))

(defmethod object-ok-p ((object g-circle))
  (not (zerop (radius object))))

;;;
;;;
;;;

(defmethod set-init-extend ((object t))
  ())


(defmethod set-init-extend ((object scaleable-thing))
  (let ((*global-scaling* 1))
    (multiple-value-bind (xf yf xt yt)
	(get-bounding-rect object)
      (setf (init-x-extend object) (- xt xf) 
	    (init-y-extend object) (- yt yf)))))


;;;
;;;
;;;
	  

(defmethod init-object-properties ((object t))
  ())

(defmethod init-object-properties ((object basic-thing))
  (get-bounding-rect object)
  (make-poly-representation object)
  (call-next-method))
  
(defmethod init-object-properties ((object composite-thing))
  (multiple-value-bind (x y)
      (get-bounding-rect-center object)
    (multiple-value-bind (x y)
	(scale-mouse-position x y :inverse t)
      (setf (xtrans object) x
	    (ytrans object) y)))
    (call-next-method))
      
(defmethod init-object-properties ((object scaleable-thing))
  (set-init-extend object)
  (call-next-method))

(defmethod init-object-properties ((object linesegment))
  (make-object-handles object (list (startpoint object) (endpoint object)))
  (call-next-method))

(defmethod init-object-properties ((object pointlist-object))
  (unless (handles object) 
    (make-object-handles object
			 (generate-brackets 
			   (pointlist object)
			   )))
  (call-next-method))

(defmethod init-object-properties ((object g-spline-chain))
  (setf (spline-points object)
    (mapcan #'identity
	    (filter-to-close-together-out
	     (reverse 
	      (make-spline 
	       (make-spline-chain-list 
		(generate-brackets 
		  (pointlist object)
		  ))
	       4))
	     3.0)))
  (call-next-method))


(defmethod init-object-properties ((object g-spline-polygon))
  (setf (spline-points object)
    (mapcan #'identity
	    (filter-to-close-together-out
	     (reverse 
	      (make-spline 
	       (make-spline-polygon-list 
		(generate-brackets 
		  (pointlist object)))
	       4))
	      3.0)))
  (call-next-method))




;;;
;;;
;;;

(defmethod initialize-instance :after ((object thing) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)   
      (unless (parent-concepts object)
	(setf (parent-concepts object)
	  (list (type-of object))))
      (setf (id-number object) (get-id-number)))))

(defmethod initialize-instance :after ((object object-with-head) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)
      (with-slots (default-arrow-head) gened-frame
	(setf (head object) default-arrow-head)
	(setf (parent-concepts object)
	  (case (type-of object)
	    (g-arrow 
	     (if (head object)		 
		 '(g-arrow)
	       '(g-line)))
	    (g-chain
	     (if (head object)
		 '(g-directed-chain)
	       '(g-chain)))      
	    (g-spline-chain
	      (if (head object)
		  '(g-directed-spline-chain)
		'(g-spline-chain)))))))))

;;;
;;;

(defmethod initialize-instance :after ((object ink-mixin) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)
      (with-slots (default-ink) gened-frame
	(setf (ink object) default-ink)))))
  
(defmethod initialize-instance :after ((object filled-mixin) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)
      (with-slots (default-filled default-filled-ink) gened-frame
	(setf (filledp object) default-filled)
	(setf (filled-ink object) default-filled-ink)))))

(defmethod initialize-instance :after ((object linestyle-mixin) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)
      (with-slots (default-line-style) gened-frame	
	(setf (linestyle object) (decode-line-style object))))))


(defmethod initialize-instance :after ((object linethickness-mixin) &key)
  (when (initialize object)
    (with-application-frame (gened-frame)
      (with-slots (default-line-thickness) gened-frame
	(setf (linethickness object) default-line-thickness)))))

;;;
;;;

(defmethod initialize-instance :after ((object g-text) &key)
  (if (initialize object)
      (with-application-frame (gened-frame)
	(with-slots (default-text-family default-text-face default-text-size) gened-frame
	  (setf (family object) default-text-family)
	  (setf (face object) default-text-face)
	  (setf (size object) default-text-size)))))


(defmethod initialize-instance :after ((object linesegment) &key)
  (if (initialize-handles object)
      (make-object-handles object (list (startpoint object) (endpoint object)))))

(defmethod initialize-instance :after ((object pointlist-object) &key)
  (if (initialize-handles object)
      (make-object-handles object (generate-brackets
				   (pointlist object)))))


