;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;; Object-Copier-Functions for Spatial Relations!
;;;

(defclass stored-relations ()
  ((belongs-to :accessor belongs-to)
   (id-number :accessor id-number)
   (xtrans :accessor xtrans)
   (ytrans :accessor ytrans)))

(defclass s-object-with-head ()
  ((head :accessor head)))

(defclass s-0d (stored-relations 0d)
  ())

(defclass s-1d (stored-relations 1d)
  ())

(defclass s-directed-1d (stored-relations 1d directed-info-element s-object-with-head)
  ())

(defclass s-2d (stored-relations 2d)
  ())

(defclass s-filled-mixin ()
  ((filledp :accessor filledp)))

(defclass s-composite-thing (stored-relations 2d) 
  ((liste :accessor liste :initform nil)))

(defclass s-info-point (stored-relations 0d)
  ((startpoint-of :initform nil :initarg :startpoint-of :accessor startpoint-of)
   (endpoint-of :initform nil :initarg :endpoint-of :accessor endpoint-of)
   (parent-head :accessor parent-head)))

(defclass s-g-text (stored-relations 2d)
  ((text-string :accessor text-string)))

(defclass s-g-circle (stored-relations 2d s-filled-mixin)
  ((radius :accessor radius)))

(defclass s-g-rectangle (stored-relations 2d s-filled-mixin)
  ())

(defclass s-g-polygon (stored-relations 2d s-filled-mixin)
  ())

(defclass s-g-spline-polygon (stored-relations 2d s-filled-mixin)
  ())


;;;
;;;
;;;

(defmethod print-object ((object stored-relations) stream)
  (print "STORED" stream)
  (print-object
   (belongs-to object)
   stream))

(defmethod get-classic-ind ((object stored-relations))
  (get-classic-ind
   (belongs-to object)))

;;;
;;;
;;;

(defmethod copy-relations ((source-object t) (destination-object stored-relations))
  (setf (id-number destination-object)
    (id-number source-object))
  (setf (belongs-to destination-object)
    source-object)
  (setf (xtrans destination-object)
    (xtrans source-object))
  (setf (ytrans destination-object)
    (ytrans source-object)))

(defmethod copy-relations ((source-object composite-thing) (destination-object s-composite-thing))
  (dolist (elem (liste source-object))    
    (push (make-stored-relation elem) 
	  (liste destination-object)))
  (call-next-method))

(defmethod copy-relations  ((source-object filled-mixin) (destination-object s-filled-mixin))
  (setf (filledp destination-object)
    (filledp source-object))
  (call-next-method))

(defmethod copy-relations ((source-object g-circle) (destination-object s-g-circle))
  (setf (radius destination-object)
    (radius source-object))
  (call-next-method))

(defmethod copy-relations ((source-object g-text) (destination-object s-g-text))
  (setf (text-string destination-object)
    (text-string source-object))
  (call-next-method))

(defmethod copy-relations ((source-object info-point) (destination-object s-info-point))
  (setf (startpoint-of destination-object)
    (startpoint-of source-object))
  (setf (endpoint-of destination-object)
    (endpoint-of source-object))
  (setf (parent-head destination-object)
    (head (or (startpoint-of source-object)
	      (endpoint-of source-object))))
  (call-next-method))
  
;;;
;;;
;;;

(defmethod copy-relations ((source-object 0d) (destination-object 0d))
  (with-slots (part-of-cluster disjoint-with 
	       start-linked-over-with end-linked-over-with
	       start-linker-objects end-linker-objects
	       in-relation-with-objects
	       intersects-objects intersects-0-objects
	       touching-objects
	       contained-in-objects
	       covered-by-objects
	       directly-contained-by-object) destination-object
    
    (setf part-of-cluster (part-of-cluster source-object))
    (setf disjoint-with (disjoint-with source-object))
    
    (setf start-linked-over-with (start-linked-over-with source-object))
    (setf end-linked-over-with (end-linked-over-with source-object))
    
    (setf start-linker-objects
      (start-linker-objects source-object))
    (setf end-linker-objects  
      (end-linker-objects source-object))
    
    (setf in-relation-with-objects (in-relation-with-objects source-object))
    (setf intersects-objects (intersects-objects source-object))
    (setf intersects-0-objects (intersects-0-objects source-object))
    (setf touching-objects (touching-objects source-object))
    (setf contained-in-objects (contained-in-objects source-object))
    (setf covered-by-objects (covered-by-objects source-object))
    (setf directly-contained-by-object (directly-contained-by-object source-object)))
  (call-next-method))

(defmethod copy-relations ((source-object 1d) (destination-object 1d))
  (with-slots (intersects-1-objects) destination-object
    (setf intersects-1-objects (intersects-1-objects source-object)))
  (call-next-method))

(defmethod copy-relations ((source-object 2d) (destination-object 2d))
  (with-slots (intersects-2-objects contains-objects covers-objects directly-contains-objects)
      destination-object
    (setf intersects-2-objects (intersects-2-objects source-object))
    (setf contains-objects (contains-objects source-object))
    (setf covers-objects (covers-objects source-object))
    (setf directly-contains-objects (directly-contains-objects source-object)))
  (call-next-method))


(defmethod copy-relations ((source-object directed-info-element) (destination-object s-directed-1d))      
  (setf (startpoint-related-with destination-object) 
    (startpoint-related-with source-object))
  (setf (endpoint-related-with destination-object) 
    (endpoint-related-with source-object))
  (if (head source-object)
      (setf (head destination-object) t)
    (setf (head destination-object) nil))
  (call-next-method))


;;;
;;;
;;;


(defun remove-stored-object-relations ()
  (with-application-frame (gened-frame)
    (with-slots (liste stored-relations) gened-frame
      (setf stored-relations nil))))
  
(defun store-object-relations ()
  (with-application-frame (gened-frame)
    (with-slots (liste stored-relations) gened-frame
      (setf stored-relations nil)
      (dolist (elem liste)
	(let ((rel (make-stored-relation elem)))
	  (push rel stored-relations))))))

(defun make-stored-relation (elem)
  (let ((instance
	 (make-instance 		       
	     (cond ((typep elem 'composite-thing) 's-composite-thing)
		   ((typep elem 'directed-info-element) 's-directed-1d)
		   ((typep elem 'g-circle) 's-g-circle)
		   ((typep elem 'g-text) 's-g-text)				    
		   ((typep elem 'g-rectangle) 's-g-rectangle)
		   ((typep elem 'g-polygon) 's-g-polygon)
		   ((typep elem 'g-spline-polygon) 's-g-spline-polygon)		   
		   ((typep elem '2d) 's-2d)
		   ((typep elem '1d) 's-1d)
		   ((typep elem 'info-point) 's-info-point) 
		   ((typep elem '0d) 's-0d)))))
    (copy-relations elem instance)
    instance))

(defun get-stored-object (object)
  (with-application-frame (gened-frame)
    (with-slots (stored-relations) gened-frame
      (find (id-number object)
	    stored-relations
	    :key #'id-number))))
