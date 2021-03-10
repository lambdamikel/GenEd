;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: GENED -*-

(in-package gened)

;;;
;;;
;;;

#-:classic
(defun do-incremental-updates (&key (backward nil))
  nil)


#+:classic 
(defun do-incremental-updates (&key (backward nil))
  (with-application-frame (gened-frame)
    (with-slots (inc-mode liste stored-relations) gened-frame
      (when inc-mode
	(let ((*info* nil))
	  (calc-relations))		
		
	(setf liste (install-points liste))
	
	(let ((delta1 (set-difference liste stored-relations :key #'id-number))
	      (delta2 (set-difference stored-relations liste :key #'id-number)))
	  
	  (if backward
	      (progn						
		(dolist (elem delta2)
		  (delete-classic-ind elem))
		
		(dolist (elem delta1)	; Objekte, die neu hinzugekommen sind
		  (create-classic-ind elem))
		(dolist (elem delta1)
		  (fill-all-roles elem)))
		
	    (progn
	      
	      (dolist (elem delta2)
		(delete-classic-ind elem))
	  
	      (dolist (elem delta1)	; Objekte, die neu hinzugekommen sind
		(create-classic-ind elem))
	      (dolist (elem delta1)
		(fill-all-roles elem)))) 
	      
	  (dolist (copy stored-relations)
	    (let ((original (get-object (id-number copy))))
	      (when (not (or (member (id-number copy) delta1 :key #'id-number)
			     (member (id-number copy) delta2 :key #'id-number)))  
	    
		(when *info* 
		  (format t "~%Updating DELTA for Object ~A~%" original))
		
		(do-incremental-classic-updates original copy :backward backward)))))
	
	(update-parent-concepts)			
	(store-object-relations)
	(setf liste (remove-points liste))))))


#-:classic
(defun do-incremental-classic-updates (orig-object copy &key (backward nil))
  nil)

#+:classic
(defun do-incremental-classic-updates (orig-object copy &key (backward nil))
  
  (dolist (slot (object-delta orig-object copy))

    (let ((prev-cont
	   (if backward 
	       (funcall slot orig-object)
	     (funcall slot copy)))
	  (cont
	   (if backward 
	       (funcall slot copy)
	     (funcall slot orig-object))))			       
      
      (if (and (listp cont)
	       (listp prev-cont))
	  
	  (if
	      (subsetp 
	       prev-cont cont :key #'id-number)
	  
	      (if backward
		  (remove-information orig-object slot
				      (set-difference cont prev-cont :key #'id-number))
		(add-information orig-object slot
				 (set-difference cont prev-cont :key #'id-number)))
	    
	    (if	(subsetp 
		 cont prev-cont :key #'id-number)

		(if backward
		    (add-information orig-object slot
				     (set-difference prev-cont cont :key #'id-number))
		  (remove-information orig-object slot
				      (set-difference prev-cont cont :key #'id-number)))))
	
	(if backward
	    (progn
	      (remove-information orig-object slot cont)
	      (add-information orig-object slot prev-cont))
	  (progn
	    (remove-information orig-object slot prev-cont)
	    (add-information orig-object slot cont)))))))
       		  		      
;;;
;;;
;;;

(defgeneric all-slots (object)
  (:method-combination append))

;;;
;;;
;;;

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
		       (not (equalp l1 l2)))
		  (list rel-sym))))
	  slot-names))

;;;
;;;
;;;

(defmethod all-slots append ((object composite-thing))
  '(liste))

(defmethod all-slots append ((object s-composite-thing))
  '(liste))

(defmethod all-slots append ((object filled-mixin))
  '(filledp))

(defmethod all-slots append ((object s-filled-mixin))
  '(filledp))

(defmethod all-slots append ((object g-text))
  '(text-string))

(defmethod all-slots append ((object s-g-text))
  '(text-string))

(defmethod all-slots append ((object g-circle))
  '(radius))

(defmethod all-slots append ((object s-g-circle))
  '(radius))

(defmethod all-slots append ((object thing))
  '(xtrans ytrans))

(defmethod all-slots append ((object stored-relations))
  '(xtrans ytrans))

(defmethod all-slots append ((object 0d))
  '(part-of-cluster 
    
    ; disjoint-with
    
    calc-linker-objects
    calc-start-linker-objects
    calc-end-linker-objects
    
    calc-linked-over-with
    calc-start-linked-over-with
    calc-end-linked-over-with

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

;;; Achtung: calc-... sind VIRTUELLE SLOTS, die erst berechnet werden muessen! -> MAPPING in "interface-to-classic"

(defmethod all-slots append ((object directed-info-element))
  '(calc-startpoint-related-with
    calc-endpoint-related-with
    calc-points-related-with))

(defmethod all-slots append ((object s-directed-1d))
  '(calc-startpoint-related-with
    calc-endpoint-related-with
    calc-points-related-with))

(defmethod all-slots append ((object s-info-point))
  '(calc-startpoint-of
    calc-endpoint-of
    calc-point-of))

(defmethod all-slots append ((object info-point))
  '(calc-startpoint-of
    calc-endpoint-of
    calc-point-of))

;;;
;;;
;;;

(defmethod object-delta ((object1 0d) (object2 0d))
  (get-delta object1 object2 
	     (union (all-slots object1)
		    (all-slots object2))))



