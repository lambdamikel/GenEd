;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defparameter *accessor-table* (make-hash-table))

(defmacro define-relation-accessors (&rest names)
  `(progn
     (defparameter +relation-accessor-symbols+ ',names)
     (clrhash *accessor-table*)
     (dolist (name +relation-accessor-symbols+)
       (setf (gethash name *accessor-table*) (fdefinition `(setf ,name))))))

(defun find-accessor (name)
  (gethash name *accessor-table*))

;;; ----------------------------------------------------------------------

(define-relation-accessors
        
    touching-objects
    
    intersects-objects
    intersects-0-objects
    intersects-1-objects
    intersects-2-objects
    
    contains-objects
    contained-in-objects
    
    directly-contains-objects
    directly-contained-by-object
    
    covers-objects
    covered-by-objects
    
    start-linked-over-with
    end-linked-over-with
    
    startpoint-related-with
    endpoint-related-with
    
    start-linker-objects
    end-linker-objects
    
    disjoint-with
    in-relation-with-objects)

(defconstant +info-relation-accessor-symbols+
    '(
      touching-objects 
      intersects-objects
      intersects-0-objects
      intersects-1-objects
      intersects-2-objects
      
      contained-in-objects
      
      directly-contained-by-object
      
      covered-by-objects
      
      start-linked-over-with
      end-linked-over-with
            
      disjoint-with))
      
(defconstant +map-accessors-to-pretty-names+ 
    '((touching-objects . (touches touches))
      
      (intersects-objects . (intersects intersects))
      (intersects-0-objects . (intersects>dim=0 intersects>dim=0))
      (intersects-1-objects . (intersects>dim=1 intersects>dim=1))
      (intersects-2-objects . (intersects>dim=2 intersects>dim=2))
      
      (contained-in-objects . (is-inside contains))

      (directly-contained-by-object . (directly-inside directly-contains))
      
      (covered-by-objects . (covers is-covered-by))
      
      (start-linked-over-with . (is-end-linked-over-with is-start-linked-over-with))
      (end-linked-over-with . (is-start-linked-over-with is-end-linked-over-with))
      
      (disjoint-with . (is-disjoint-with is-disjoint-with))))
      
(defconstant +map-relations-to-accessors+
    '((touches . (touching-objects))
      
      (intersects . (intersects-objects))
      (intersects-0 . (intersects-0-objects intersects-objects))
      (intersects-1 . (intersects-1-objects intersects-objects))
      (intersects-2 . (intersects-2-objects intersects-objects))
      
      (contains . (contained-in-objects))
      (is-inside . (contains-objects))
      
      (covers . (contained-in-objects covered-by-objects directly-contained-by-object))
      (is-covered-by . (contains-objects covers-objects directly-contains-objects))
      
      (is-disjoint-with . (disjoint-with))))

(defconstant +map-accessors-to-roles+
    '((start-info-point . has-startpoint)
      (end-info-point . has-endpoint)
			
      (disjoint-with . disjoint-with)
      (start-linked-over-with . start-linked-over-with)
      (end-linked-over-with . end-linked-over-with)
      (in-relation-with-objects . in-relation-with-objects)
      (intersects-objects . intersects-objects)
      (intersects-0-objects . intersects-0-objects)
      (touching-objects . touching-objects)
      (contained-in-objects . contained-in-objects)
      (covered-by-objects . covered-by-object) ; richtig geschr.! (mal m. s, mal ohne!)
      (directly-contained-by-object . directly-contained-by-object)
      
      (intersects-1-objects . intersects-1-objects)
      
      (intersects-2-objects . intersects-2-objects)
      (contains-objects . contains)
      (directly-contains-objects . directly-contains-objects)
      
      (start-linker-objects . start-linker-objects)
      (end-linker-objects . end-linker-objects)
      
      (covers-objects . covers-objects)))

;;;
;;;
;;;

(defun store-output-regions ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display))
	  (*bounding-boxes* t)
	  (*handles-symbol-list* nil))
      
      (with-slots (liste) gened-frame
	(dolist (object liste)
	  (unless (typep object 'info-point)
	    (let ((record (with-output-recording-options (stream :draw nil :record t)
			    (with-output-to-output-record (stream)
			      (draw object stream)))))	     
	      (setf (output-record1 object) record))))))))

(defun get-accessor-function-from-name (symbol)
  (fdefinition `(setf ,symbol)))

;;;
;;;
;;;

(defun clear-stored-relations (liste)
  (dolist (elem liste)
    (dolist (accessor +relation-accessor-symbols+)
      (when (slot-exists-p elem accessor)
	(funcall (find-accessor accessor) 
		 nil
		 elem)))))
    
(defun show-topo-info (elem1 elem2 relation) 
  (multiple-value-bind (dist orient)
      (distance-and-orientation 
       (xtrans elem1) (ytrans elem1)
       (xtrans elem2) (ytrans elem2))
    
    (format t "~% Relation:~%  ~A~%    ~A~% ~A."
	    elem1
	    relation
	    elem2) 
    
    (format t "~%Polar-Distance: (~D | ~D)~%"
	    (round dist)
	    (round (* 180 (/ orient pi))))))
    

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
	(pushnew elem1 (disjoint-with elem2))))))

(defun remove-wrong-disjoints (liste)
  (dolist (elem1 liste)
    (dolist (elem2 liste)
      (if (and (not (eq elem1 elem2))
	       (member elem1 (in-relation-with-objects elem2)))
	  (setf (disjoint-with elem2)
	    (delete elem1 (disjoint-with elem2)))))))


(defun remove-not-in-list (liste)
  (dolist (elem liste)
    (dolist (slot +relation-accessor-symbols+)
      (when (slot-exists-p elem slot)
	(let ((content (funcall slot elem)))
	  (dolist (rel-elem content)	  
	    (unless (member rel-elem liste)
	      (funcall (find-accessor slot)
		       (delete rel-elem content)
		       elem))))))))

  
(defun install-relations-for-clusters ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame      	
	
      (dolist (cluster1 liste) 	
	(let ((elems-of-cluster1 (find-all-cluster-elements cluster1)))
	  (dolist (cluster2 liste)
	    
	    (if  (and (not (eq cluster1 cluster2))
		      (or (typep cluster1 'composite-thing)
			  (typep cluster2 'composite-thing)))		  
		  
		(let* ((elems-of-cluster2 (find-all-cluster-elements cluster2)))
		    		    
		  (cond ( (some #'(lambda (x)
				    (some #'(lambda (y)
					      (member y (intersects-objects x)))
					  elems-of-cluster2))
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
						    (and (member y 
								 (in-relation-with-objects x))
							 (not (member y (touching-objects x))) ; !!! nachdenken!							   
							 ))
						elems-of-cluster2))
				      elems-of-cluster1)))
			  (memoize-relation cluster1 cluster2 'touches)))))))))))
  
			 
(defun memoize-relation (elem1 elem2 relation)

  #| (multiple-value-bind (dist orient)
      (distance-and-orientation 
       (xtrans elem1) (ytrans elem1)
       (xtrans elem2) (ytrans elem2)) |#
    
  (unless (eq relation 'is-disjoint-with)
    (push elem1
	  (in-relation-with-objects elem2)))	      
  
  (dolist (accessor (rest (assoc relation +map-relations-to-accessors+)))
    (when (slot-exists-p elem2 accessor)
      (funcall (find-accessor accessor)
	       (cons elem1 (funcall accessor elem2))
	       elem2))))

;;;
;;;
;;;

(defun store-infos-for-arrows-and-so ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(when (and (typep elem 'directed-info-element)
		   (> (length (in-relation-with-objects elem)) 1))
	  
	  (let ((startpoint (start-info-point elem))
		(endpoint (end-info-point elem))
		(related-start-object)
		(related-end-object))
	    	    
	    (dolist (inter-elem (intersects-objects elem))
	      (when (typep inter-elem 'basic-thing)

		(when (member 
		       inter-elem
		       (contained-in-objects startpoint))
		  (setf related-start-object inter-elem))
		
		(when (member
		       inter-elem
		       (contained-in-objects endpoint))
		  (setf related-end-object inter-elem))))
	    
	    (dolist (touch-elem (touching-objects elem))
	      (when (typep touch-elem 'basic-thing)
		 (when (member 
			startpoint
			(touching-objects touch-elem))
		   (setf related-start-object touch-elem))
		 
		 (when (member
			endpoint
			(touching-objects touch-elem))
		   (setf related-end-object touch-elem))))
	    
	    (dolist (cov-elem (covered-by-objects elem))
	      (when (typep cov-elem 'basic-thing)
		(when (member 
		       cov-elem
		       (covered-by-objects startpoint))
		  (setf related-start-object cov-elem))
		
		(when (member 
		       cov-elem
		       (covered-by-objects endpoint))
		  (setf related-end-object cov-elem))))
	    
	    (when  (and related-start-object related-end-object)
	      
	      (push related-start-object (startpoint-related-with elem))
	      (push related-start-object (in-relation-with-objects related-end-object))
	      (push related-end-object (end-linked-over-with related-start-object))	      
	    
	      (push related-end-object (endpoint-related-with elem))
	      (push related-end-object (in-relation-with-objects related-start-object))
	      (push related-start-object (start-linked-over-with related-end-object))
	      
	      (pushnew elem (start-linker-objects related-start-object))
	      (pushnew elem (end-linker-objects related-end-object)))))))))


(defun find-topmost-cluster (elem)
  (let ((cluster elem))
    (loop
      (if (null (part-of-cluster cluster))
	  (return cluster)
	(setf cluster (part-of-cluster cluster))))))

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
	      
	      (push cluster1 (start-linked-over-with cluster2))
	      (push cluster1 (in-relation-with-objects cluster2))
	      
	      (push cluster2 (end-linked-over-with cluster1))
	      (push cluster2 (in-relation-with-objects cluster1))
	      
	      (pushnew elem (start-linker-objects cluster1))
	      (pushnew elem (end-linker-objects cluster2)))))))))


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
			(contained-in-objects conflict-elem)))))))))))		       
	      

(defun remove-cluster-parts (liste)
  (let ((liste2 liste))
    (dolist (elem liste)
      (if (typep elem 'composite-thing)
	  (setf liste2
	    (set-difference liste2 (liste elem)))))
    liste2))

(defun calculate-directly-containing (liste) 
  (flet ((memoize (elem1 elem2)
	   (pushnew elem2 (directly-contains-objects elem1))
	   (setf (directly-contained-by-object elem2) (list elem1))))
    
    (dolist (elem liste)
      
      (let* ((contained-in-objects 
	      (remove-cluster-parts (remove-duplicates (contained-in-objects elem))))
	     (l (length contained-in-objects)))
	

	(dolist (elem2 contained-in-objects)
	  
	  (when
	      (or (and (typep elem2 'thing)
		       (= (length
			    (remove-cluster-parts (remove-duplicates (contained-in-objects elem2))))
			  (1- l)))
		  nil)
	    
	    (memoize elem2 elem)))))))

(defun install-points (liste)
  (mapcan #'(lambda (elem)
	      (if (typep elem 'directed-info-element)
		  (list (start-info-point elem)
			(end-info-point elem) 
			elem)
		(list elem)))
	  liste))

(defun remove-points (liste)
  (remove-if #'(lambda (x)
		 (typep x 'info-point))
	     liste))

(defun calc-relations ()
  (with-application-frame (gened-frame)
    (with-slots (liste saved-liste calc-int-dim) gened-frame	      
	
      (clear-stored-relations liste)
      
      (setf liste (install-points liste)) 
      
      (atomize-all-clusters)        
      
      (store-output-regions)
            
      (clear-stored-relations liste)
    
      (dolist (elem1 liste)
	(dolist (elem2 liste)
	  (unless (eq elem1 elem2)
	    
	    (if (or (typep elem1 'info-point)
		    (typep elem2 'info-point)
		    (not (eql +nowhere+
			      (region-intersection
			       (output-record1 elem1)
			       (output-record1 elem2)))))
		
		(let ((relation 
		       (relate-poly-to-poly-unary-tres  		     
			(polygonized-representation elem1)
			(polygonized-representation elem2)
			(calc-right-tres)
			:dim calc-int-dim)))		  
		  (memoize-relation elem1 elem2 relation))
	      (memoize-relation elem1 elem2 'is-disjoint-with)))))	
      
      (resolve-covering-conflicts liste)        
      
      (reinstall-all-clusters)	
	
      (make-all-disjoint liste)
      
      (store-infos-for-arrows-and-so)
      (propagate-for-arrows-and-so liste)  
      
      (calculate-directly-containing liste) 

      (install-relations-for-clusters) 
      
      (calculate-directly-containing liste)
      
      (remove-wrong-disjoints liste)
      (remove-not-in-list liste) 
      
      (setf liste (remove-points liste))  

      (when *info* (show-infos)))))

(define-gened-command (com-gened-show-relations :name "Show Topological Relations")    
    ()    
  (let ((*info* t))
    (calc-relations)))

(define-gened-command (com-gened-calc-relations :name "Calculate Topological Relations")    
    ()    
  (let ((*info* nil))
    (calc-relations)))

(defun show-infos ()
   (with-application-frame (gened-frame)
     (let ((mem-list nil))
       (format t "~%~%Hash-Table-Size: ~D~%" (hash-table-count geometry::*hash-table*))
       (with-slots (liste) gened-frame	      	
	 
	 (dolist (elem liste)
	   (mapc #'(lambda (relation-accessor-symbol)		      

		     (when (slot-exists-p elem relation-accessor-symbol)

		       (let ((relation-symbols (rest (assoc relation-accessor-symbol
							    +map-accessors-to-pretty-names+))))			
			 
			 (dolist (rel-elem (funcall 
					    (symbol-function relation-accessor-symbol)
					    elem))
			   
			   (when (and 
				  (not (member (list (first relation-symbols) elem rel-elem) mem-list
					       :test #'equal))
				  (not (member (list (second relation-symbols) elem rel-elem) mem-list
					       :test #'equal)))
			       
			     (push (list (first relation-symbols) elem rel-elem) mem-list)
			     (push (list (second relation-symbols) rel-elem elem) mem-list)
			       
			     (show-topo-info elem rel-elem (first relation-symbols))
			     (show-topo-info rel-elem elem (second relation-symbols)))))))		 
		 
		 +info-relation-accessor-symbols+))))))
		    
;;;
;;;
;;;

(defmethod show-map-over ((object thing) accessor)
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (unhighlight-all-objects)
      (dolist (elem (funcall accessor object))
	(highlight-object elem)))))
  
(define-gened-command (com-gened-show-touching :name "Show Touching Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'touching-objects))))

(define-gened-command (com-show-touching-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'touching-objects))

;;;
;;;
;;;

(define-gened-command (com-gened-show-intersecting :name "Show Intersecting Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'intersects-objects))))

(define-gened-command (com-show-intersecting-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'intersects-objects))


(define-gened-command (com-gened-show-intersecting-dim=0 :name "Show Intersecting Objects, DIM = 0")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'intersects-0-objects))))

(define-gened-command (com-gened-show-intersecting-dim=1 :name "Show Intersecting Objects, DIM = 1")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-1d-object)))
	(terpri)
	(show-map-over object #'intersects-1-objects))))

(define-gened-command (com-gened-show-intersecting-dim=2 :name "Show Intersecting Objects, DIM = 2")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-2d-object)))
	(terpri)
	(show-map-over object #'intersects-2-objects))))


;;;
;;;
;;;

(define-gened-command (com-gened-show-contained :name "Show Contained In Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'contained-in-objects))))

(define-gened-command (com-show-contained-in-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'contained-in-objects))

;;;
;;;
;;;

(define-gened-command (com-gened-show-containing :name "Show Containing Objects")
    ()
  (if (any-visible-2d-objects)
      (let ((object (accept 'at-least-2d-object)))
	(terpri)
	(show-map-over object #'contains-objects))))

(define-gened-command (com-show-containing-objects)
    ((object 'at-least-2d-object :gesture nil))
  (show-map-over object #'contains-objects))

;;;
;;; 
;;;

(define-gened-command (com-gened-show-directly-contained :name "Show Directly Contained In Object")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'directly-contained-by-object))))

(define-gened-command (com-show-directly-contained-in-object)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'directly-contained-by-object))

;;;
;;;
;;;

(define-gened-command (com-gened-show-directly-containing :name "Show Directly Containing Objects")
    ()
  (if (any-visible-2d-objects)
      (let ((object (accept 'at-least-2d-object)))
	(terpri)
	(show-map-over object #'directly-contains-objects))))

(define-gened-command (com-show-directly-containing-objects)
    ((object 'at-least-2d-object :gesture nil))
  (show-map-over object #'directly-contains-objects))

;;;
;;;
;;;

(define-gened-command (com-gened-show-covered :name "Show Covered By Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'covered-by-objects))))

(define-gened-command (com-show-covered-by-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'covered-by-objects))

;;;
;;;
;;;

(define-gened-command (com-gened-show-covering :name "Show Covering Objects")
    ()
  (if (any-visible-2d-objects)
      (let ((object (accept 'at-least-2d-object)))
	(terpri)
	(show-map-over object #'covers-objects))))

(define-gened-command (com-show-covering-objects)
    ((object 'at-least-2d-object :gesture nil))
  (show-map-over object #'covers-objects))

;;;
;;;
;;;

(define-gened-command (com-gened-show-disjoint :name "Show Disjoint Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'disjoint-with))))

(define-gened-command (com-show-disjoint-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'disjoint-with))

;;;
;;;
;;;

(define-gened-command (com-gened-show-related :name "Show Related Objects")
    ()
  (if (any-visible-objects)
      (let ((object (accept 'at-least-0d-object)))
	(terpri)
	(show-map-over object #'in-relation-with-objects))))

(define-gened-command (com-show-related-objects)
    ((object 'at-least-0d-object :gesture nil))
  (show-map-over object #'in-relation-with-objects))

;;;
;;;
;;;

(defmethod points-related-with ((object directed-info-element))
  (append
   (startpoint-related-with object)
   (endpoint-related-with object)))

(defmethod linked-over-with ((object thing))
  (append
   (start-linked-over-with object)
   (end-linked-over-with object)))

(defmethod linker-objects ((object thing))
  (append
   (start-linker-objects object)
   (end-linker-objects object)))

;;;
;;;
;;;

(define-gened-command (com-gened-show-linked  :name "Show Linked Objects")
    ()
  (when (any-visible-objects)
    (let* ((object (accept '(or g-arrow g-spline-chain g-chain))))
      (terpri)
      (show-map-over object #'points-related-with))))

(define-gened-command (com-gened-show-start-linked  :name "Show Start-Linked Objects")
    ()
  (when (any-visible-directed-objects)
    (let ((object (accept '(or g-arrow g-spline-chain g-chain))))
      (terpri)
      (when (head object)
	(show-map-over object #'startpoint-related-with)))))


(define-gened-command (com-gened-show-end-linked  :name "Show End-Linked Objects")
    ()
  (when (any-visible-directed-objects)
    (let ((object (accept '(or g-arrow g-spline-chain g-chain))))
      (terpri)
      (when (head object)
	(show-map-over object #'endpoint-related-with)))))

;;;
;;; 
;;;

(define-gened-command (com-gened-show-opposite :name "Show Opposite Objects")
    () 
  (when (any-visible-objects)
    (let ((object (accept 'at-least-0d-object)))	        
      (terpri)
      (show-map-over object #'linked-over-with))))

;;;
;;;
;;;

(define-gened-command (com-gened-show-linker :name "Show Linker Objects")
    ()
  (when (any-visible-objects)
    (let ((object (accept 'at-least-0d-object)))	       
      (terpri)
      (show-map-over object #'linker-objects))))

(define-gened-command (com-gened-show-start-linker :name "Show Start-Linker Objects")
    ()
  (when (any-visible-objects)
    (let ((object (accept 'at-least-0d-object)))	       
      (terpri)
      (show-map-over object #'start-linker-objects))))

(define-gened-command (com-gened-show-end-linker :name "Show End-Linker Objects")
    ()
  (when (any-visible-objects)
    (let ((object (accept 'at-least-0d-object)))	       
      (terpri)
      (show-map-over object #'end-linker-objects))))
