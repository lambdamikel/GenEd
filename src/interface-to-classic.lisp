;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;; Error-Hook-Fn.
;;;

(defun inconsistent-classic-object (code args path object)
  (if (and (cl-instance? object (cl-named-concept 'gened-thing))
	   (listp (cl-fillers object (cl-named-role 'belongs-to-clos-object))))
      (let ((id (first (cl-fillers object
				   (cl-named-role 'belongs-to-clos-object)))))
	(find-object id))))


;;;
;;;

(defun add-information (object slot delta)
 #| (print 'add)
  (print slot)
  (print delta) |#
  (when delta
    (if (listp delta)
	(insert-fillers-for-slotname object slot delta)
      (insert-fillers-for-slotname object slot (list delta)))))

(defun remove-information (object slot delta)
 #| (print 'remove)
  (print slot)
  (print delta) |#
  (when delta
    (if (listp delta)
	(remove-fillers-for-slotname object slot delta)
      (remove-fillers-for-slotname object slot (list delta)))))


(defmethod name-for ((object thing))
  (concatenate 'string
    (generate-name (first (parent-concepts object)))
    "-"			 
    (write-to-string (id-number object))))			 


(defun create-classic-ind-for (object)
   (when *info*
    (format t "Creating Classic Individual for Object ~A.~%" object))
   (let ((symbol (intern (name-for object))))
    
     (cl-create-ind symbol 
		    `(and 
		      ,@(parent-concepts object)))
     
     (cl-ind-add (cl-named-ind symbol)
		 `(fills belongs-to-clos-object 
			 ,(id-number object)))      
     (setf (associated-classic-ind object) symbol)))

(defmethod create-classic-ind ((object thing))
  (create-classic-ind-for object))

(defun create-info-classic-inds (object)
  (create-classic-ind-for object)
  (create-classic-ind-for (start-info-point object))
  (create-classic-ind-for (end-info-point object)))

(defmethod create-classic-ind ((object g-arrow))
  (create-info-classic-inds object))

(defmethod create-classic-ind ((object g-chain))
  (create-info-classic-inds object))

(defmethod create-classic-ind ((object g-spline-chain))
  (create-info-classic-inds object))

(defmethod create-classic-ind ((object composite-thing))
  (create-classic-ind-for object)
  (dolist (part (liste object))
    (create-classic-ind part)))
;;;

(defmethod fill-all-roles ((object thing))
  (dolist (slot (all-slots object))
    (let ((cont (funcall slot object)))
      (if cont
	  (add-information object slot cont))))
  (when (typep object 'directed-info-element)
    (fill-all-roles (start-info-point object))
    (fill-all-roles (end-info-point object))))

(defmethod remove-all-roles ((object thing))
  (dolist (slot (all-slots object))
    (print slot)
    (let ((cont (funcall slot object)))
      (if cont
	  (remove-information object slot cont))))
  (when (typep object 'directed-info-element)
    (remove-all-roles (start-info-point object))
    (remove-all-roles (end-info-point object))))
		      
;;;
;;;

(defmethod delete-classic-ind ((object thing))
  ())
		
;;;
;;;
;;;

(defun get-accessor-function (role-symbol)
  (case role-symbol
    (has-parts 
     'liste)
    (has-startpoint
     'start-info-point)
    (has-endpoint
     'end-info-point)
    (filled-bool
     'filledp)
    (radius-real
     'radius)
    (covered-by-object
     'covered-by-objects)
    (otherwise
     role-symbol)))

(defun get-role-from-accessor (accessor-symbol)
  (case accessor-symbol
    (liste
     'has-parts)
    (start-info-point
     'has-startpoint)
    (end-info-point
     'has-endpoint)
    (filledp 
     'filled-bool)
    (radius 
     'radius-real)
    (covered-by-objects
     'covered-by-object)
    (otherwise
     accessor-symbol)))
  
;;;
;;;
;;;


(defmethod points-related-with ((object directed-info-element))
  (append
   (startpoint-related-with object)
   (endpoint-related-with object)))
  
(defun insert-filler-from-rolename (object role-symbol)
  (let ((object-classic-ind (get-classic-ind object)))
    (dolist (rel-object
		(let ((objects 
		       (funcall (get-accessor-function role-symbol) object)))
		  (unless (null objects)
		    (if (atom objects)
			(list objects)
		      objects))))
      (let ((classic-ind 
	     (if (typep rel-object 'thing)
		 (associated-classic-ind rel-object)
	       rel-object)))
	(cl-ind-add object-classic-ind
		    `(fills ,role-symbol ,classic-ind))))))


(defun insert-fillers-for-slotname (object slot-name fillers-to-add)
  (let ((object-classic-ind (get-classic-ind object))
	(role-symbol (get-role-from-accessor slot-name)))
    
    (let ((fillers 
	   (mapcar #'(lambda (object)
		       (cl-ind-name 
			(get-classic-ind object)))
		   fillers-to-add)))

      (dolist (elem fillers)
	(cl-ind-add object-classic-ind
		    `(fills ,role-symbol
			    ,elem))))))

(defun remove-all-fillers-for-slotname (object slot-name)
  (let* ((object-classic-ind (get-classic-ind object))
	 (role-symbol (get-role-from-accessor slot-name))
	 (role
	  (cl-named-role role-symbol))
	 (inv-role
	  (cl-role-inverse role))
	 (inv-role-symbol 
	  (cl-role-name inv-role)))
    
    (let* ((all-fillers
	    (cl-fillers object-classic-ind role))
	   
	   (all-filler-names
	    (mapcar #'cl-name all-fillers))
	   
	   (told-fillers
	    (cl-told-fillers (cl-told-ind object-classic-ind)
			     role))
	   
	   (told-filler-names	  
	    (mapcar #'cl-told-ind-name
		    told-fillers))
	   
	   (derived-filler-names	  
	    (set-difference all-filler-names told-filler-names)))	

      (when (cl-ind-closed-role? object-classic-ind role)
	(cl-ind-unclose-role
	 object-classic-ind
	 role))	
            
      (if told-filler-names 
	  (cl-ind-remove object-classic-ind
			 `(fills ,role-symbol
				 ,@told-filler-names)))
      
      (if (and inv-role-symbol derived-filler-names)	
	  (let ((object-name (cl-name object-classic-ind)))
	    (dolist (name derived-filler-names)
	      (let ((inverse-ind (cl-named-ind name)))
		(cl-ind-remove inverse-ind
			       `(fills ,inv-role-symbol
				       ,object-name)))))))))

(defun remove-fillers-for-slotname (object slot-name fillers-to-remove)
  (let* ((object-classic-ind (get-classic-ind object))
	 (role-symbol (get-role-from-accessor slot-name))
	 (role
	  (cl-named-role role-symbol))
	 (inv-role
	  (cl-role-inverse role))
	 (inv-role-symbol 
	  (cl-role-name inv-role)))
    
    (let* ((all-fillers
	    (cl-fillers object-classic-ind role))
	   
	   (all-filler-names
	    (mapcar #'cl-name all-fillers))

	   (fillers-to-remove-names 	   
	    (mapcar #'(lambda (x)
			(cl-name 
			 (associated-classic-ind x)))
		    fillers-to-remove))
	   
	   (told-fillers
	    (cl-told-fillers (cl-told-ind object-classic-ind)
			     role))
	   
	   (told-filler-names	  
	    (mapcar #'cl-told-ind-name
		    told-fillers))
	   
	   (derived-filler-names	  
	    (set-difference all-filler-names told-filler-names))
	   
	   (remove-told-filler-names
	    (intersection fillers-to-remove-names
			  told-filler-names))
	   
	   (remove-derived-filler-names
	    (intersection fillers-to-remove-names
			  derived-filler-names)))	   

      (when (cl-ind-closed-role? object-classic-ind role)
	(cl-ind-unclose-role
	 object-classic-ind
	 role))	
      
      (if remove-told-filler-names 
	  (cl-ind-remove object-classic-ind
			 `(fills ,role-symbol
				 ,@remove-told-filler-names)))
      
      (if (and inv-role-symbol remove-derived-filler-names)
	  (let ((object-name (cl-name object-classic-ind)))
	    (dolist (name remove-derived-filler-names)
	      (let ((inverse-ind (cl-named-ind name)))
		(cl-ind-remove inverse-ind
			       `(fills ,inv-role-symbol
				       ,object-name)))))))))


    
(defmethod get-classic-ind ((object thing))
  (cl-named-ind (associated-classic-ind object)))

(defun insert-all-fillers (object)
    (dolist (role (set-difference
		   (first +known-roles+)
		   '(linked-over-with
		     start-linked-over-with
		     end-linked-over-with
		     points-related-with
		     startpoint-related-with
		     endpoint-related-with
		     linker-objects
		     start-linker-objects
		     end-linker-objects
		     )))
		   
      (let ((fn (get-accessor-function role)))
	(when (slot-exists-p object fn)
	  (insert-filler-from-rolename object role)))))

(defun insert-spatial-fillers ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (object liste)	  
	(when *info*
	  (format t "Inserting Fillers for Object ~A.~%" object))

	(let ((classic-ind (get-classic-ind object)))
	  (insert-all-fillers object)	  	  	  
	    
	  (dolist (elem (start-linked-over-with object))
	    (let* ((linker (second elem))
		   (linker-ind (associated-classic-ind linker))
		   (rel-object (first elem))
		   (rel-classic-ind (associated-classic-ind rel-object)))
	      
	      (if (head linker)
		  (progn
		    (cl-ind-add classic-ind
				`(fills start-linked-over-with ,rel-classic-ind))
		    (cl-ind-add classic-ind
				`(fills end-linker-objects ,linker-ind)))
		(progn		  
		  (cl-ind-add classic-ind
			      `(fills linked-over-with ,rel-classic-ind))
		  (cl-ind-add classic-ind
			      `(fills linker-objects ,linker-ind))))))
	  
	  
	  (dolist (elem (end-linked-over-with object))
	    (let* ((linker (second elem))
		   (linker-ind (associated-classic-ind linker))
		   (rel-object (first elem))
		   (rel-classic-ind (associated-classic-ind rel-object)))
	      
	      (if (head linker)
		  (progn
		    (cl-ind-add classic-ind
				`(fills end-linked-over-with ,rel-classic-ind))
		    (cl-ind-add classic-ind
				`(fills start-linker-objects ,linker-ind)))
		  (progn		  
		    (cl-ind-add classic-ind
				`(fills linked-over-with ,rel-classic-ind))
		    (cl-ind-add classic-ind
				`(fills linker-objects ,linker-ind))))))
	      
	  
	  (when (typep object 'directed-info-element)
	    (if (head object)
		(progn
		  (insert-filler-from-rolename object 'startpoint-related-with)
		  (insert-filler-from-rolename object 'endpoint-related-with))
	      (insert-filler-from-rolename object 'points-related-with))))))))	
		     
(defmethod get-associated-classic-ind ((object thing))
  (cl-named-ind
   (associated-classic-ind object)))

;;;
;;;
;;;

(define-gened-command (com-gened-create-classic-inds)
    ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'infos)))
      (window-clear stream)   
      (with-slots (liste) gened-frame
	(dolist (elem liste)
	  (create-classic-ind elem))
	(dolist (elem liste)
	  (fill-all-roles elem))))))

(define-gened-command (com-print-ind)
    ((object '(or thing object-handle) :gesture :classic-inspect))
  (terpri)
  (terpri)
  (if (typep object 'thing)
      (cl-print-object
       (get-associated-classic-ind object))
    (if (and (or (typep object 'start-handle)
		 (typep object 'end-handle))
	     (typep (parent-object object) 'directed-info-element))
	(cl-print-object
	 (get-associated-classic-ind 
	  (if (typep object 'start-handle)
	      (start-info-point (parent-object object))
	    (end-info-point (parent-object object))))))))

;;;
;;;
;;;

(defun close-role (object role-symbol)
  (let ((classic-ind 
	 (get-associated-classic-ind object))
	(role
	 (cl-named-role role-symbol)))
    (cl-ind-close-role
     classic-ind
     role)))

(defun close-all-roles-for-object (object role-set)
  (dolist (role-sym role-set)
    (close-role object role-sym)))

(defun close-all-roles ()
  (when *info* (format t "Closing Roles for Objects...~%"))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (role-set +known-roles+)
	(dolist (object liste)
	  (close-all-roles-for-object object role-set))))))

;;;
;;;
;;;

(defun unclose-role-if-neccessary (object role-symbol)
  (let ((classic-ind 
	 (get-associated-classic-ind object))
	(role
	 (cl-named-role role-symbol)))
    (when (cl-ind-closed-role? classic-ind role)
      (cl-ind-unclose-role
       classic-ind
       role))))
 
(defun unclose-all-roles-if-neccessary ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (object liste)	
	(mapc #'(lambda (role-set)
		  (mapc #'(lambda (rol-sym)
			    (unclose-role-if-neccessary object rol-sym))
			role-set))
	      +known-roles+)))))

;;;
;;;
;;;

(define-gened-command (com-gened-classify)
    ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (let ((stream (get-frame-pane gened-frame 'infos)))
	(window-clear stream)
	
	(setf liste (install-points liste))
	(atomize-all-clusters)
	
	(unclose-all-roles-if-neccessary)
	
	(reinstall-all-clusters)
            
	(unclose-all-roles-if-neccessary)
      
	(insert-spatial-fillers)
      
	(close-all-roles)  
	(update-parent-concepts)
	(setf liste (remove-points liste))
	(redraw)))))

#|
(defun update-parent-concepts ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (object liste)
	(remove-all-but-basic-concept object)
	(let ((classic-ind 
	       (get-associated-classic-ind object)))
	  (dolist (concept +known-concepts+)
	    (when (cl-instance? classic-ind
				(cl-named-concept concept))
	      
	      (push-concept object concept))))))))
|#

(defun update-parent-concepts ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (object liste)
	(remove-all-but-basic-concept object)
	(let* ((classic-ind 
		(get-associated-classic-ind object))
	       (parents (mapcar
			 #'cl-name 
			 (cl-ind-parents classic-ind)))
	       (ancestors (mapcar 
			   #'cl-name
			   (cl-ind-ancestors classic-ind)))
	       (p-concepts (reverse
			    (intersection +known-concepts+
					  parents)))
	       (a-concepts (reverse
			    (intersection +known-concepts+
					  ancestors))))
	  (mapc #'(lambda (parent ancestor)
		    (push-concept object parent)
		    (push-concept object ancestor :ancestor t))
		p-concepts
		a-concepts))))))
		    
(define-gened-command (com-gened-clear-kb)
    ()
  (cl-clear-kb))

(define-gened-command (com-gened-revert-all-to-bottom :name "Revert All To Bottom Concept")
    ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
     (setf liste (install-points liste))
      (dolist (object liste)
	(remove-all-but-basic-concept object))
      (setf liste (remove-points liste))))    
  (redraw))



  
