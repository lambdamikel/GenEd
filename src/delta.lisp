;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: GENED -*-

(in-package gened)


(defun do-incremental-updates (&key (backward nil))
  (with-application-frame (gened-frame)
    (with-slots (inc-mode liste prev-op-liste object-copy-buffer) gened-frame
      (if inc-mode
	  (progn
	    (calc-relations)
	    (let* ((undo-object 
		    (if backward
			(unstack-undo-object)
		      (get-undo-object))))
	      
	      (if undo-object
		  (let* ((op (operation undo-object))
			 (copy (if backward 
				   (original-object undo-object)
				 (object-copy undo-object)))	       
			 (orig-object (if backward 
					  (object-copy undo-object)
					(original-object undo-object))))
		    
		    #|
		    (terpri)		    
		    (print op)
		    (terpri)
		    (terpri)
		    (if orig-object
			(dolist (slot (all-slots orig-object))
			  (format t "ORIG!! Slot ~A: ~A~%" slot (funcall slot orig-object))))
		    (terpri)
		    (if copy
			(dolist (slot (all-slots copy))
			(format t "COPY! Slot ~A: ~A~%" slot (funcall slot copy)))) 		    
			|#
		    (print op)
		    
		           
		    (case op
		      
		      (cluster
		       (if backward
			   (progn
			     (remove-all-roles copy)
			     (dolist (elem (liste copy))
			       (fill-all-roles elem)))
			 (when orig-object
			   (create-classic-ind orig-object)
			   (fill-all-roles orig-object))))

		      (delete
		       (if backward	; dann UNDO delete = (re)create !
			   (progn
			     (fill-all-roles copy)
			     (setf (associated-classic-ind orig-object)
			       (associated-classic-ind copy)))
		     
			 (when orig-object
			   (delete-classic-ind orig-object)
			   (remove-all-roles orig-object))))
			  
		      ((create load-object copy)	
		       (if backward	; dann UNDO create = delete !
			   (remove-all-roles copy)		
			 (when orig-object
			   (create-classic-ind orig-object)
			   (fill-all-roles orig-object))))
		      
		      (load-scene
		       (com-gened-create-classic-inds))		       
		      
		      (otherwise
		       
		       (when backward 
			 (setf (associated-classic-ind orig-object)
			   (associated-classic-ind copy))
			 (when (typep orig-object 'composite-thing)
			   (mapc #'(lambda (o-elem c-elem)
				     (setf (associated-classic-ind o-elem)
				       (associated-classic-ind c-elem)))
				 (liste orig-object)
				 (liste copy))))		       
		       
		       (loop for i from 0 to (1- +buffer-size+) do 
			     (let ((second-undo-object 
				    (aref object-copy-buffer i)))
			       
			       (if (and second-undo-object 
					(original-object second-undo-object)
					orig-object
					(= (id-number (original-object second-undo-object))
					   (id-number orig-object)))
				   (setf (original-object second-undo-object)
				     orig-object)))) 
		       
		       (when (and orig-object copy)
			 (do-incremental-classic-updates orig-object copy)
			 (when (typep orig-object 'directed-info-element)
			   (do-incremental-classic-updates 
			       (start-info-point orig-object)
			     (start-info-point copy))
			   (do-incremental-classic-updates
			       (end-info-point orig-object)
			     (end-info-point copy))
			   
			   (let ((delta (object-delta orig-object copy)))

			     (when (member 'startpoint-related-with delta)
			       
			       (if (and (startpoint-related-with copy)
					(startpoint-related-with orig-object))
				   (do-incremental-classic-updates
				       (first (startpoint-related-with orig-object))
				     (first (startpoint-related-with copy))))			       
			       
			       (if (and (startpoint-related-with copy)
					(not (startpoint-related-with orig-object)))
				   (remove-all-roles orig-object))
			       
			       (if (and (not (startpoint-related-with copy))
					(startpoint-related-with orig-object))
				   (fill-all-roles orig-object)))
			     
			     (when (member 'endpoint-related-with delta)
			     
			       (if (and (endpoint-related-with copy)
					(endpoint-related-with orig-object))
				   (do-incremental-classic-updates
				       (first (endpoint-related-with orig-object))
				     (first (endpoint-related-with copy))))			       
			       
			       (if (and (endpoint-related-with copy)
					(not (endpoint-related-with orig-object)))
				   (remove-all-roles orig-object))
			       
			       (if (and (not (endpoint-related-with copy))
					(endpoint-related-with orig-object))
				   (fill-all-roles orig-object)))

			 )))))))))
      
	(if backward (unstack-undo-object))))))


(defmethod do-incremental-classic-updates ((orig-object null) (copy 0d))
  (fill-all-roles copy))

(defmethod do-incremental-classic-updates ((orig-object 0d) (copy null))
  (remove-all-roles orig-object))

(defmethod do-incremental-classic-updates ((orig-object 0d) (copy 0d))
  (dolist (slot (object-delta orig-object copy))

    (let ((prev-cont (get-unique-slot-output (funcall slot copy)))
	  (cont (get-unique-slot-output (funcall slot orig-object))))			       

      #| (format t "DELTA Slot: ~A ~A ~A~%" slot prev-cont cont) |#
      
      (if (or (and (listp cont)				     
		   (subsetp 
		    prev-cont cont :key #'id-number))
	      (and (numberp cont)
		   (not (equalp prev-cont cont))))
	  (add-information orig-object slot
			   (set-difference cont prev-cont :key #'id-number)))
			       
      (if (or (and (listp prev-cont)				     
		   (subsetp 
		    cont prev-cont :key #'id-number))
	      (and (numberp prev-cont)
		   (not (equalp prev-cont cont))))
	  (remove-information orig-object slot
			      (set-difference prev-cont cont
					      :key #'id-number))))))	
			  
		      
;;;;

(defgeneric all-slots (object)
  (:method-combination append))

;;;;

(defun get-unique-slot-output (content)
  (when content
    (if (atom content) 
	(list content)
      content)))

(defun get-delta (object1 object2 slot-names)
  
  (mapcan #'(lambda (rel-sym)
	      (let ((l1 (funcall rel-sym object1))
		    (l2 (funcall rel-sym object2)))

		(when (or
		       (and l1 (not l2))
		       (and l2 (not l1))
		       (and (listp l1)
			    (listp l2)
			    (not (and (subsetp l1 l2 :key #'id-number)
				      (subsetp l2 l1 :key #'id-number))))		     
		       (and (numberp l1) 
			    (numberp l2)
			    (not (equalp l1 l2))))
		  (list rel-sym))))
	  slot-names))


;;; Achtung: unbed. die richtige Reihenfolge einhalten!

(defmethod all-slots append ((object composite-thing))
  '(liste))

(defmethod all-slots append ((object 0d))
  '(part-of-cluster 
    
    disjoint-with
        
    start-linked-over-with
    end-linked-over-with
    
    start-linker-objects
    end-linker-objects
    
    intersects-objects
    intersects-0-objects
    
    touching-objects

    directly-contained-by-object 
    contained-in-objects
    covered-by-objects))

(defmethod all-slots append ((object 1d))
  '(intersects-1-objects))

(defmethod all-slots append ((object 2d))
  '(intersects-2-objects
    directly-contains-objects   
    contains-objects
    covers-objects))

(defmethod all-slots append ((object directed-info-element))
  '(startpoint-related-with
    endpoint-related-with
    start-info-point
    end-info-point))

;;;
;;;


(defmethod object-delta ((object1 0d) (object2 0d))
  (get-delta object1 object2 
	     (union (all-slots object1)
		    (all-slots object2))))

(defmethod object-delta ((object1 null) (object2 0d))
  (all-slots object2))

(defmethod object-delta ((object1 0d) (object2 null))
  (all-slots object1))

  
