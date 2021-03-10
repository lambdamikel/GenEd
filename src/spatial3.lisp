;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

(defun store-output-regions ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display))
	  (*bounding-boxes* t)
	  (*handles-symbol-list* nil))
      
      (with-slots (liste) gened-frame
	  (dolist (object liste)
	    (let ((record (with-output-recording-options (stream :draw nil :record t)
			    (with-output-to-output-record (stream)
			      (draw object stream)))))	     
	      (setf (output-record object) record)))))))


;;;
;;;
;;;
;;;

(defun clear-stored-relations (liste)
  (dolist (elem liste)
    (setf (disjoint-with elem) nil)
    (setf (start-linked-over-with elem) nil)
    (setf (end-linked-over-with elem) nil)
    (setf (in-relation-with-objects elem) nil)
    (setf (touching-objects elem) nil)
    (setf (contained-in-objects elem) nil)
    (setf (covered-by-objects elem) nil)
    (setf (directly-contained-by-object elem) nil)				       
    
    (when (typep elem 'directed-info-element)
      (setf (startpoint-related-with elem) nil)
      (setf (endpoint-related-with elem) nil))
    
    (when (typep elem 'at-least-1d)
      (setf (intersects-objects elem) nil))
    (when (typep elem '2d)
      (setf (contains-objects elem) nil)
      (setf (covers-objects elem) nil)
      (setf (directly-contains-objects elem) nil) 
      )))
		
(defun show-topo-info (elem1 elem2 relation stream) 
  (when *info*
    (multiple-value-bind (dist orient)
	(distance-and-orientation 
	 (xtrans elem1) (ytrans elem1)
	 (xtrans elem2) (ytrans elem2))
    
      (format stream "~% Relation:~%  ~A~%    ~A~% ~A.~%"
	      elem1
	      relation
	      elem2) 
      
      (format stream "Polar-Distance: (~D | ~D)~%"
	      (round dist)
	      (round (* 180 (/ orient pi)))))))
    

(defun find-all-cluster-elements (cluster)
  (if (typep cluster 'composite-thing)      
      (let ((liste (liste cluster)))
	(mapcan
	 #'find-all-cluster-elements liste))
    (list cluster)))

(defun make-all-disjoint (liste)
  (dolist (elem1 liste)
    (dolist (elem2 liste)
      (unless (eq elem1 elem2)
	(push elem1 (disjoint-with elem2))))))

(defun remove-wrong-disjoints (liste)
  (dolist (elem1 liste)
    (dolist (elem2 liste)
      (if (and (not (eq elem1 elem2))
	       (member elem1 (in-relation-with-objects elem2)))
	  (setf (disjoint-with elem2)
	    (delete elem1 (disjoint-with elem2)))))))
      

  
(defun install-relations-for-clusters ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'infos)))

      (with-slots (liste) gened-frame      	
	
	(dolist (cluster1 liste) 	
	  (let ((elems-of-cluster1 (find-all-cluster-elements cluster1)))
	    (dolist (cluster2 liste)
	      
	      (if  (and (not (eq cluster1 cluster2))
			(or (typep cluster1 'composite-thing)
			    (typep cluster2 'composite-thing)))		  
		  
		  (let* ((elems-of-cluster2 (find-all-cluster-elements cluster2)))
		    		    
		    (cond ( (some #'(lambda (x)
				      (and (typep x 'at-least-1d)
					   (some #'(lambda (y)
						     (member y (intersects-objects x)))
						 elems-of-cluster2)))
				  elems-of-cluster1)
			    (memoize-relation cluster1 cluster2 'intersects))
			  
			  ( (and 
			     (some #'(lambda (x)
				       (and (typep x '2d)
					    (some #'(lambda (y)
						      (member y (covers-objects x)))
						  elems-of-cluster2)))
				   elems-of-cluster1)
			     (some #'(lambda (x)
				       (and (typep x '2d)
					    (every #'(lambda (y)
						       (or (member y (contains-objects x))
							   (member y (covers-objects x))))
						   elems-of-cluster2)))
				   elems-of-cluster1))
			    (memoize-relation cluster1 cluster2 'covers)
			    (memoize-relation cluster2 cluster1 'is-covered-by))		
			      
			  
			  ( (some #'(lambda (x)
				      (and (typep x '2d)
					   (every #'(lambda (y)
						      (member y (contains-objects x)))
						  elems-of-cluster2)))
				  elems-of-cluster1)
			    (memoize-relation cluster1 cluster2 'contains)
			    (memoize-relation cluster2 cluster1 'is-inside))

			  
			  ( (and 
			     (some #'(lambda (x)
				       (some #'(lambda (y)
						 (member y (touching-objects x)))
					     elems-of-cluster2))
				   elems-of-cluster1)
			     (not (some #'(lambda (x)
					    (some #'(lambda (y)
						      (and (member y (in-relation-with-objects x))
							   (not (member y (touching-objects x)))))
						  elems-of-cluster2))
					elems-of-cluster1)))
			    (memoize-relation cluster1 cluster2 'touches))
					      			  		
		))))))))))
  
			 
(defun memoize-relation (elem1 elem2 relation)

  (multiple-value-bind (dist orient)
      (distance-and-orientation 
       (xtrans elem1) (ytrans elem1)
       (xtrans elem2) (ytrans elem2))
    
      (unless (eq relation 'is-disjoint-with)
	(push elem1
	      (in-relation-with-objects elem2)))	
      
      (ecase relation
	
	(intersects
	 (push elem1 (intersects-objects elem2)))
	
	(touches
	 (push elem1 (touching-objects elem2)))
	
	(is-inside
	 (push elem1 (contains-objects elem2)))	
	
	(contains
	 (push elem1 (contained-in-objects elem2)))
       
	(is-covered-by
	 (push elem1 (covers-objects elem2))
	 (push elem1 (contains-objects elem2))
	 (push elem1 (directly-contains-objects elem2)))
		
	(covers
	 (push elem1 (covered-by-objects elem2))
	 (push elem1 (contained-in-objects elem2))
	 (push elem1 (directly-contained-by-object elem2))) 
	
	(is-disjoint-with
	 (push elem1 (disjoint-with elem2))))))
	 
	
	
	
;;;
;;;
;;;


(defun store-infos-for-arrows-and-so ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(when (and (typep elem 'directed-info-element)
		   (> (length (in-relation-with-objects elem)) 1))
	  (let ((startpoint 
		 (make-geom-point (+ (xtrans elem) (first (startpoint elem)))
				  (+ (ytrans elem) (second (startpoint elem)))))
		(endpoint
		 (make-geom-point (+ (xtrans elem) (first (endpoint elem)))
				  (+ (ytrans elem) (second (endpoint elem)))))
		(related-start-object)
		(related-end-object))
	    
	    (dolist (touch-elem (touching-objects elem))
	      (when (typep touch-elem 'basic-thing)
		(when (touching-tres 
		       startpoint
		       (polygonized-representation touch-elem)
		       (* 1 (calc-right-tres)))
		  (setf related-start-object touch-elem))
		
		(when (touching-tres 
		       endpoint
		       (polygonized-representation touch-elem)
		       (* 1 (calc-right-tres)))		
		  (setf related-end-object touch-elem))))
	    
	    (dolist (inter-elem (intersects-objects elem))
	      (when (typep inter-elem 'basic-thing)
		(when (inside
		       startpoint
		       (polygonized-representation inter-elem))
		  (setf related-start-object inter-elem))
		
		(when (inside
		       endpoint
		       (polygonized-representation inter-elem))
		  (setf related-end-object inter-elem))))
	    
	    (dolist (cov-elem (covered-by-objects elem))
	      (when (typep cov-elem 'basic-thing)
		(when (covers-tres
		       (polygonized-representation cov-elem)
		       startpoint
		       (calc-right-tres))
		  (setf related-start-object cov-elem))
		
		(when (covers-tres
		       (polygonized-representation cov-elem)
		       endpoint
		       (calc-right-tres))
		  (setf related-end-object cov-elem))))
	    
	    (when related-start-object 	  	    
	      (push related-start-object (startpoint-related-with elem))
	      (when related-end-object
		(push related-start-object (in-relation-with-objects related-end-object))
		(push (list related-end-object elem)
		       (end-linked-over-with related-start-object))))
	    
	    (when related-end-object 	  	    
	      (push related-end-object (endpoint-related-with elem))
	      (when related-start-object
		(push related-end-object (in-relation-with-objects related-start-object))
		(push (list related-start-object elem)
		      (start-linked-over-with related-end-object))))	    
	    ))))))


(defun find-topmost-cluster (elem)
  (let ((cluster elem))
    (loop
      (if (null (part-of-cluster cluster))
	  (return cluster)
	(setf cluster (part-of-cluster cluster))))))

#|
(defun propagate-for-arrows-and-so (liste)  
  (dolist (elem liste)
    (when (and (typep elem 'directed-info-element)
	       (startpoint-related-with elem)
	       (endpoint-related-with elem))
      
      (let ((srw (startpoint-related-with elem))
	    (erw (endpoint-related-with elem)))
	
	(if (and srw erw)
	    (let ((cluster srw))
	      (loop
		(setf cluster (mapcan #'(lambda (x)
					  (if x (let ((part-of (part-of-cluster x)))
						  (when part-of (list part-of)))))
				      cluster))					
		(if cluster
		    (dolist (elem2 cluster)
		      (push elem2 (startpoint-related-with elem)))
		  (return)))	  
	      
	      (let ((cluster erw))
		(loop
		  (setf cluster (mapcan #'(lambda (x)
					    (if x (let ((part-of (part-of-cluster x)))
						    (when part-of (list part-of)))))
					cluster))		  
		  
		  (if cluster
		      (dolist (elem2 cluster)
			(push elem2 (endpoint-related-with elem)))
		    (return))))))))))
|#


(defun propagate-for-arrows-and-so (liste)  
  (dolist (elem liste)
    (when (and (typep elem 'directed-info-element)
	       (startpoint-related-with elem)
	       (endpoint-related-with elem))
      
      (let ((srw (first (startpoint-related-with elem)))
	    (erw (first (endpoint-related-with elem))))
	
	(when (and srw erw)
	  (let ((cluster1 (find-topmost-cluster srw))
		(cluster2 (find-topmost-cluster erw)))
	    (when (not (and (eq cluster1 srw) (eq cluster2 erw)))
	      (push cluster1 (startpoint-related-with elem))
	      (push cluster2 (endpoint-related-with elem))
	      
	      (push (list cluster1 elem)
		    (start-linked-over-with cluster2))
	      (push cluster1 (in-relation-with-objects cluster2))
	      
	      (push (list cluster2 elem)
		    (end-linked-over-with cluster1))
	      (push cluster2 (in-relation-with-objects cluster1))
	      
	      )))))))


(defun resolve-covering-conflicts (liste)
  (dolist (elem liste)
    (when (and (typep elem '2d))
      (let ((cov-elems (covers-objects elem)))
	(when (> (length cov-elems) 1)
	  (dolist (cov-elem cov-elems)
	    (when (typep cov-elem '2d)
	      (let* ((cov-cov-elems
		      (covers-objects cov-elem))
		     (conflict-elem
		      (find-if #'(lambda (X)
				   (member x cov-elems))
			       cov-cov-elems)))
		(when conflict-elem
		  
		  (setf (covers-objects elem)
		    (delete conflict-elem
			    cov-elems))
		  (setf (covered-by-objects conflict-elem)
		    (delete elem
			    (covered-by-objects conflict-elem)))
		  
		  (setf (directly-contains-objects elem)
		    (delete conflict-elem
			    (directly-contains-objects elem)))
		  
		  (if (eq (first (directly-contained-by-object conflict-elem))
			  elem)
		      (setf (directly-contained-by-object conflict-elem)
			nil))
		  
		  (push conflict-elem
			(contains-objects elem))
		  (push elem
			(contained-in-objects conflict-elem))
		  
		  )))))))))
		       
	      

(defun calculate-directly-containing (liste) 
  (flet ((memoize (elem1 elem2)
	   (push elem2 (directly-contains-objects elem1))
	   (setf (directly-contained-by-object elem2) (list elem1))))
    
    (dolist (elem liste)
      
      (let* ((contained-in-objects (remove-duplicates (contained-in-objects elem)))
	     (l (length contained-in-objects)))

	(dolist (elem2 contained-in-objects)
	  (when
	      (or (and (typep elem2 'basic-thing)
		       (= (length
			   (remove-duplicates (contained-in-objects elem2)))
			  (1- l)))
		  (and (typep elem2 'composite-thing)
		       (= (length 
			   (remove-duplicates (contained-in-objects elem2)))
			  (- l 2))))
	 #|   (format t "~A directly-contains ~A ~%" elem2 elem) |#
	    
	    (memoize elem2 elem)))))))


(defun only-dublicates (liste &optional (akku nil))
  (cond ((null liste) akku)
	(t (let* ((elem (first liste))
		  (relem (list (second elem)
			       (first elem))))
	     (if (member relem
			 liste :test #'equal)
		 (only-dublicates (cdr (delete relem
					       liste :test #'equal))
				  (cons elem akku))
	       (only-dublicates (cdr liste) akku))))))

(define-gened-command (com-gened-calc-relations :name "Calculate Relations")    
    ()    
  (with-application-frame (gened-frame)
    (with-slots (liste saved-liste) gened-frame	      
	
      (clear-stored-relations liste)
      (atomize-all-clusters)
      (clear-stored-relations liste)
      
      (store-output-regions)
      
      (dolist (elem1 liste)
	(dolist (elem2 liste)
	  (unless (eq elem1 elem2)
	    
	    (if (not (eql +nowhere+
			  (region-intersection
			   (output-record elem1)
			   (output-record elem2))))
		
		(let ((relation 
		       (relate-poly-to-poly-unary-tres  		     
			(polygonized-representation elem1)
			(polygonized-representation elem2)
			(calc-right-tres))))
		  
		  (memoize-relation elem1 elem2 relation))
	      (memoize-relation elem1 elem2 'is-disjoint-with))
	    )))	
      
      (resolve-covering-conflicts liste)  
      
      (reinstall-all-clusters)	
	
      (make-all-disjoint liste)
      
      (store-infos-for-arrows-and-so)
      (propagate-for-arrows-and-so liste)  
      
      (calculate-directly-containing liste) 

      (install-relations-for-clusters) 
      
      (calculate-directly-containing liste)
         
      (remove-wrong-disjoints liste)
      
      (show-infos)
      )))

(defun show-infos ()
   (with-application-frame (gened-frame)
     (let ((stream (get-frame-pane gened-frame 'infos))
	   (mem-list nil))
       (window-clear stream)        
       (with-slots (liste) gened-frame	      	
	 (dolist (elem liste)
	  (mapcar #'(lambda (relation-accessor-symbol relation-symbol-list)		      
		      (when (slot-exists-p elem relation-accessor-symbol)
			(let ((relation-symbol (first relation-symbol-list))
			      (inverse-relation-symbol (second relation-symbol-list)))
			  (dolist (rel-elem (funcall 
					     (symbol-function relation-accessor-symbol)
					     elem))
			    (when (and (member rel-elem liste)
				       (not (member (list relation-symbol elem rel-elem) mem-list
						    :test #'equal)))
			      (push (list relation-symbol elem rel-elem) mem-list)
			      (push (list relation-symbol rel-elem elem) mem-list)
			      (show-topo-info elem rel-elem relation-symbol stream)
			      (show-topo-info rel-elem elem inverse-relation-symbol stream))))))
		  
		  '(touching-objects
		    intersects-objects
		    directly-contained-by-object
		    contained-in-objects
		    covered-by-objects
		    linked-over-with
		    disjoint-with)
		  
		  '((touches touches)
		    (intersects intersects)
		    (directly-contained-in directly-contains)
		    (contained-in contains)
		    (is-covered-by covers)
		    (is-linked-over-with is-linked-over-with)
		    (is-disjoint-with is-disjoint-with))))))))
		    

;;;
;;;
;;;

(defmethod show-map-over ((object thing) accessor)
  (dolist (elem (funcall accessor object))
    (highlight-object elem))
  (let ((*bounding-boxes* t))
    (redraw)))
  

(define-gened-command (com-gened-show-touching :name "Show Touching Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'touching-objects))))

(define-gened-command (com-show-touching :name "Show Touching Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'touching-objects))

;;;
;;;

(define-gened-command (com-gened-show-intersecting :name "Show Intersecting Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'intersects-objects))))

(define-gened-command (com-show-intersecting :name "Show Intersecting Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'intersects-objects))

;;;
;;;

(define-gened-command (com-gened-show-contained :name "Show Contained In Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'contained-in-objects))))

(define-gened-command (com-show-contained :name "Show Contained In Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'contained-in-objects))

;;;
;;;

(define-gened-command (com-gened-show-containing :name "Show Containing Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'contains-objects))))

(define-gened-command (com-show-containing :name "Show Containing Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'contains-objects))

;;;
;;; 


(define-gened-command (com-gened-show-directly-contained :name "Show Directly 
Contained In Object")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'directly-contained-by-object))))

(define-gened-command (com-show-directly-contained :name "Show Directly Contained In Object")
    ((object 'thing :gesture nil))
  (show-map-over object #'directly-contained-by-object))


;;;
;;;

(define-gened-command (com-gened-show-directly-containing :name "Show Directly Containing Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'directly-contains-objects))))

(define-gened-command (com-show-directly-containing :name "Show Directly Containing Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'directly-contains-objects))

;;;
;;;

(define-gened-command (com-gened-show-covered :name "Show Covered By Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'covered-by-objects))))

(define-gened-command (com-show-covered :name "Show Coverd By Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'covered-by-objects))

;;;
;;;

(define-gened-command (com-gened-show-covering :name "Show Covering Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'covers-objects))))

(define-gened-command (com-show-covering :name "Show Covering Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'covers-objects))

;;;
;;;

(define-gened-command (com-gened-show-disjoint :name "Show Disjoint Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'disjoint-with))))

(define-gened-command (com-show-disjoint :name "Show Disjoint Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'disjoint-with))

;;;
;;;

(define-gened-command (com-gened-show-related :name "Show Related Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'thing)))
	(show-map-over object #'in-relation-with-objects))))

(define-gened-command (com-show-related :name "Show Related Objects")
    ((object 'thing :gesture nil))
  (show-map-over object #'in-relation-with-objects))

;;;
;;;

(define-gened-command (com-gened-show-linked  :name "Show Linked Objects")
    ()
  (when (any-visible-objects)
    (let* ((object (accept 'directed-info-element)))	 
      (highlight-object (first (endpoint-related-with object)))
      (highlight-object (first (startpoint-related-with object))))    
    (let ((*bounding-boxes* t))
      (redraw))))	  

(define-gened-command (com-gened-show-start-linked  :name "Show Start-Linked Objects")
    ()
  (when (any-visible-directed-objects)
    (let ((object (accept 'directed-info-element)))	       
      (highlight-object (first (startpoint-related-with object))))
    (let ((*bounding-boxes* t))
      (redraw))))


(define-gened-command (com-gened-show-end-linked  :name "Show End-Linked Objects")
    ()
  (when (any-visible-directed-objects)
    (let ((object (accept 'directed-info-element)))	       
      (highlight-object (first (endpoint-related-with object))))
    (let ((*bounding-boxes* t))
      (redraw))))



;;;
;;;
;;;

(define-gened-command (com-gened-show-opposite :name "Show Opposite Objects")
    ()
  (when (any-visible-objects)
    (let ((object (accept 'thing)))	       
      (mapc #'(lambda (x)
		(highlight-object (first x)))
	    (append 
	     (start-linked-over-with object)
	     (end-linked-over-with object))))
    (let ((*bounding-boxes* t))
      (redraw))))

;;;;

(define-gened-command (com-gened-show-linker :name "Show Linker Objects")
    ()
  (when (any-visible-objects)
    (let ((object (accept 'thing)))	       
      (mapc #'(lambda (x)
		(highlight-object
		 (second x)))
	    (append
	     (start-linked-over-with object)
	     (end-linked-over-with object)))
      (let ((*bounding-boxes* t))
	(redraw)))))


  
      


