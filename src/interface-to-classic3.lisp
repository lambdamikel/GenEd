;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;; Error-Hook-Fn.
;;;

(defun inconsistent-classic-object (code args path object)
  (declare (ignore code args path))
  (if (and (cl-instance? object (cl-named-concept 'gened-thing))
	   (listp (cl-fillers object (cl-named-role 'belongs-to-clos-object))))
      (let ((id (first (cl-fillers object
				   (cl-named-role 'belongs-to-clos-object)))))
	(find-object id))))

;;;
;;;
;;;

(defmethod name-for ((object thing))
  (concatenate 'string
    (generate-name (first (parent-concepts object)))
    "-"			 
    (write-to-string (id-number object))))			 

;;;
;;;
;;;

(defun add-information (object slot delta)
  (when delta   
    (when *info* (format t "+ Information: Object ~A, Slot ~A~%" object slot))   
    (if (or (listp delta) (typep delta 'thing))
	(if (listp delta)
	    (insert-fillers-for-slotname object slot delta)
	  (insert-fillers-for-slotname object slot (list delta)))	  
      (insert-filler-for-attribute object slot delta))))

(defun remove-information (object slot delta)
  (when delta 
    (when *info* (format t "- Information: Object ~A, Slot ~A~%" object slot))  
    (if (or (listp delta) (typep delta 'thing))
	(if (listp delta)
	    (remove-fillers-for-slotname object slot delta)
	  (remove-fillers-for-slotname object slot (list delta)))	  
      (remove-filler-for-attribute object slot delta))))

;;;
;;;
;;;


(defun create-classic-ind-for (object)
  (unless (associated-classic-ind object)
    (when *info*
      (format t "~%Creating Classic Individual for Object ~A.~%" object))
  
    (let* ((symbol (intern (name-for object)))
	   (classic-ind      
	    (cl-create-ind symbol 
			   `(and 
			     ,@(parent-concepts object)))))
      (unless (eq classic-ind 'classic-error)     
	(cl-ind-add (cl-named-ind symbol)
		    `(fills belongs-to-clos-object 
			    ,(id-number object)))      
	(setf (associated-classic-ind object) symbol)))))

(defmethod create-classic-ind ((object thing))
  (create-classic-ind-for object))

;;; Nachdenken! Alle Relationen zwischen Part-Of-Objekten gehen verloren! (Black-Box!)

(defmethod create-classic-ind ((object composite-thing))
  (dolist (part (liste object))
    (create-classic-ind part))
  (create-classic-ind-for object))

;;;
;;;
;;;

(defun fill-all-roles-for-object (object)
  (dolist (slot (all-slots object))
    (let ((cont (funcall slot object)))
      (add-information object slot cont))))

(defmethod fill-all-roles ((object thing))
  (fill-all-roles-for-object object)
  (call-next-method))

(defmethod fill-all-roles ((object stored-relations))
  (fill-all-roles-for-object object)
  (call-next-method))

(defmethod fill-all-roles ((object t))
  ())

;;;
;;;
;;;

(defun remove-all-roles-for-object (object)
  (dolist (slot (all-slots object))
    (let ((cont (funcall slot object)))
      (remove-information object slot cont))))

(defmethod remove-all-roles ((object thing))
  (remove-all-roles-for-object object)
  (call-next-method))

(defmethod remove-all-roles ((object stored-relations))
  (remove-all-roles-for-object object)
  (call-next-method))
			      
(defmethod remove-all-roles ((object t))
  ())
		      
;;;
;;;
;;;

(defmethod delete-classic-ind ((object t))
  (when *info*
    (format t "~%Removing All Information for Object ~A.~%" object))
  (remove-all-roles object))


(defmethod delete-classic-ind ((object composite-thing))
  (when *info*
    (format t "~%Removing All Information for Object ~A.~%" object))
  (remove-all-roles object)
  (dolist (part (liste object))
    (delete-classic-ind part)))

;;;
;;;
;;;

(defun get-role-from-accessor (accessor-symbol)
  (case accessor-symbol
    (liste
     'has-parts)
    (calc-linked-over-with
     'linked-over-with)
    (calc-start-linked-over-with
     'start-linked-over-with)
    (calc-end-linked-over-with
     'end-linked-over-with)
    (calc-linker-objects
     'linker-objects)
    (calc-start-linker-objects
     'start-linker-objects)
    (calc-end-linker-objects
     'end-linker-objects)    
    (calc-startpoint-related-with
     'startpoint-related-with)
    (calc-endpoint-related-with
     'endpoint-related-with)
    (calc-points-related-with
     'points-related-with)
    (part-of-cluster
     'part-of)
    (calc-point-of
     'point-part-of)
    (calc-startpoint-of
     'startpoint-part-of-directed-element)
    (calc-endpoint-of
     'endpoint-part-of-directed-element)
    (filledp 
     'filled-bool)
    (xtrans
     'xtrans-integer)
    (ytrans
     'ytrans-integer)
    (radius 
     'radius-real)
    (covered-by-objects
     'covered-by-object)    
    (otherwise
     accessor-symbol)))

;;;
;;;
;;;

(defun update-classic-ind-concepts (object &key (remove 
						 nil)
						(clos-update t))
  (with-application-frame (gened-frame)
    (with-slots (inc-mode) gened-frame
      (when inc-mode	
	(let ((object-classic-ind (get-classic-ind object))
	      (parent-concepts (parent-concepts object))
	      (ancestor-concepts (ancestor-concepts object)))
	  (when object-classic-ind
	    (if remove
		(cl-ind-remove object-classic-ind
			       `(and ,@parent-concepts ,@ancestor-concepts))
	      (cl-ind-add object-classic-ind
			  `(and ,@parent-concepts ,@ancestor-concepts)))
	    (if clos-update (update-parent-concepts))))))))		      

;;;
;;;
;;;

(defun create-classic-inds ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (elem liste)
	(create-classic-ind elem))
      (dolist (elem liste)
	(fill-all-roles elem)))))
  
;;;
;;;
;;;

(defmethod calc-points-related-with ((object directed-info-element))
  (if (head object)
      nil
    (append
     (startpoint-related-with object)
     (endpoint-related-with object))))

(defmethod calc-startpoint-related-with ((object directed-info-element))
  (if (head object)
      (startpoint-related-with object)
    nil))

(defmethod calc-endpoint-related-with ((object directed-info-element))
  (if (head object)
      (endpoint-related-with object)
    nil))

;;;
;;;
;;;

(defmethod calc-linker-objects ((object 0d))
  (remove-if #'head
	     (append
	      (start-linker-objects object)
	      (end-linker-objects object))))

(defmethod calc-start-linker-objects ((object 0d))
  (remove-if-not #'head
		 (start-linker-objects object)))

(defmethod calc-end-linker-objects ((object 0d))
  (remove-if-not #'head
		 (end-linker-objects object)))

;;;
;;;
;;;

(defmethod calc-linked-over-with ((object 0d))
  (mapcan #'(lambda (goal linker)
	      (unless (head linker)
		(list goal)))
	  (append 
	   (start-linked-over-with object)
	   (end-linked-over-with object))
	  (append
	   (start-linker-objects object)
	   (end-linker-objects object))))

 
(defmethod calc-start-linked-over-with ((object 0d))

  (mapcan #'(lambda (goal linker)
	      (if (head linker)
		  (list goal)))
	  (start-linked-over-with object)
	  (end-linker-objects object)))

(defmethod calc-end-linked-over-with ((object 0d))
  (mapcan #'(lambda (goal linker)
	      (if (head linker)
		  (list goal)))		 
	  (end-linked-over-with object)
	  (start-linker-objects object)))


;;;
;;;
;;;

(defun f-point-of (object)
  (let ((parent (or (startpoint-of object)
		    (endpoint-of object))))
    (if (head parent)
	nil
      (list 
       (or 
	(startpoint-of object)
	(endpoint-of object))))))


(defmethod calc-point-of ((object info-point))
  (f-point-of object))

(defmethod calc-point-of ((object s-info-point))
  (f-point-of object))

(defun f-startpoint-of (object)
  (let ((parent (startpoint-of object)))
    (if parent
	(if (head parent)
	    (list parent)
	  nil)
      nil)))


(defmethod calc-startpoint-of ((object info-point))
  (f-startpoint-of object))

(defmethod calc-startpoint-of ((object s-info-point))
  (f-startpoint-of object))

(defun f-endpoint-of (object)
  (let ((parent (endpoint-of object)))
    (if parent
	(if (head parent)
	    (list parent)
	  nil)
      nil)))

(defmethod calc-endpoint-of ((object info-point))
  (f-endpoint-of object))

(defmethod calc-endpoint-of ((object s-info-point))
  (f-endpoint-of object))


;;;
;;;
;;;

(defun insert-filler-for-attribute (object attrib-name filler-to-add)
  (let ((object-classic-ind (get-classic-ind object))
	(filler-to-add (if (numberp filler-to-add)
			 (round filler-to-add)
			 filler-to-add)))
    (when (and object-classic-ind filler-to-add)
      (let*
	  ((role-symbol (get-role-from-accessor attrib-name))
	   (present-fillers (cl-fillers object-classic-ind 
					(cl-named-role role-symbol))))
    
	(unclose-all-roles-for-object-i.n. object)
    
	(if present-fillers
	    (cl-ind-remove object-classic-ind
			   `(fills ,role-symbol
				   ,@present-fillers)))
    
	(cl-ind-add object-classic-ind
		    `(fills ,role-symbol
			    ,filler-to-add))))))

(defun insert-fillers-for-slotname (object slot-name fillers-to-add)
  (let ((object-classic-ind (get-classic-ind object)))
    (when object-classic-ind
      (let* 
	  ((role-symbol (get-role-from-accessor slot-name))
	   (present-fillers (cl-fillers object-classic-ind 
					(cl-named-role role-symbol))))
    
	(let ((filler-names 
	       (mapcar #'(lambda (object)
			   (cl-name
			    (get-classic-ind object)))
		       fillers-to-add))
	      
	      (present-filler-names
	       (mapcar #'cl-ind-name present-fillers)))
	  
	  (unclose-all-roles-for-object-i.n. object)   
	  
	  (dolist (filler-name filler-names)
	    (let ((filler-ind (cl-named-ind filler-name)))
	      (unclose-all-roles-for-classic-ind-i.n. filler-ind)
	      (unless (member filler-name present-filler-names)
		(cl-ind-add object-classic-ind
			    `(fills ,role-symbol
				    ,filler-name))))))))))

;;;
;;;
;;;

(defun remove-all-fillers-for-slotname (object slot-name)
  (remove-fillers-for-slotname object slot-name 
			       (funcall slot-name object)))

(defun remove-filler-for-attribute (object attrib-name filler-to-remove)
  (let* ((object-classic-ind (get-classic-ind object))
	 (role-symbol (get-role-from-accessor attrib-name))
	 (filler-to-remove 
	  (if (numberp filler-to-remove)
	      (round filler-to-remove)
	    filler-to-remove)))
    
    (when (and object-classic-ind filler-to-remove)
      (unclose-role-for-classic-ind-i.n. object-classic-ind role-symbol)
      
      (cl-ind-remove object-classic-ind
		     `(fills ,role-symbol
			     ,filler-to-remove)))))

(defun remove-fillers-for-slotname (object slot-name fillers-to-remove)
  (let ((object-classic-ind (get-classic-ind object)))
    (when object-classic-ind
      (let* (
	     (role-symbol (get-role-from-accessor slot-name))
	     (role
	      (cl-named-role role-symbol))
	     (inv-role
	      (cl-role-inverse role))
	     (inv-role-symbol 
	      (cl-role-name inv-role))

	     (all-fillers
	      (cl-fillers object-classic-ind role))
	     
	     (all-filler-names
	      (mapcar #'cl-name all-fillers))
	     
	     (fillers-to-remove-names 	   
	      (mapcar #'(lambda (x)
			  (cl-name
			   (get-classic-ind x)))
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

	(unclose-all-roles-for-classic-ind-i.n. object-classic-ind)
      
	(dolist (filler-name remove-told-filler-names)
	  (unclose-all-roles-for-classic-ind-i.n. (cl-named-ind filler-name))
	  
	  (cl-ind-remove object-classic-ind
			 `(fills ,role-symbol
				 ,filler-name)))
	
	(if (and inv-role-symbol remove-derived-filler-names)
	    (let ((object-name (cl-name object-classic-ind)))
	      (dolist (name remove-derived-filler-names)
		(let ((inverse-ind (cl-named-ind name)))
		  
		  (unclose-all-roles-for-classic-ind-i.n. 
		   inverse-ind)
		  
		  (cl-ind-remove inverse-ind
				 `(fills ,inv-role-symbol
					 ,object-name))))))))))
  
;;;
;;;
;;;
    
(defmethod get-classic-ind ((object thing))
  (let ((classic-ind
	 (cl-named-ind (associated-classic-ind object))))
    (if classic-ind
	classic-ind
      (progn
	(format t "~%*** ERROR - NO CLASSIC-INDIVIDUAL~%     FOR OBJECT ~A EXISTS! ***~%" object)
	nil))))

;;;
;;;
;;;

(defun print-classic-individual (object)
  (terpri)
  (terpri)
  (if (typep object 'thing)
      (cl-print-object
       (get-classic-ind object))
    (if (and (or (typep object 'start-handle)
		 (typep object 'end-handle))
	     (typep (parent-object object) 'directed-info-element))
	(cl-print-object
	 (get-classic-ind 
	  (if (typep object 'start-handle)
	      (start-info-point (parent-object object))
	    (end-info-point (parent-object object)))))
      (format t "~A is not a handle of a Linker-Object!~%" object))))

(define-gened-command (com-print-classic-individual)
    ((object '(or thing start-handle end-handle) :gesture :classic-inspect))
  (print-classic-individual object))

;;;
;;;
;;;

(defun close-role-for-object (object role-symbol)
  (let ((classic-ind 
	  (get-classic-ind object)))
    (when classic-ind
      (let* (
	     (cl-ind
	      (if (cl-told-classic-ind? classic-ind)
		  classic-ind
		(cl-told-ind classic-ind)))
	     (role
	      (cl-named-role role-symbol)))
	(unless (cl-told-ind-closed-role? 
		 cl-ind role)
	  (cl-ind-close-role classic-ind
			     role))))))

(defun close-roles-for-object (object role-set)
  (dolist (role-sym role-set)
    (close-role-for-object object role-sym)))

(defun close-all-roles-for-object (object)
  (dolist (role-set +known-roles+)
    (close-roles-for-object object role-set)))


(defun close-all-roles ()
  (when *info* 
    (format t "~%Closing Roles for Objects..."))
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (role-set +known-roles+)
	(dolist (object liste)
	  (close-roles-for-object object role-set)))))
  (when *info* 
    (format t "  DONE!~%")
    (beep))
  (update-parent-concepts))


(define-gened-command (com-gened-close-all-roles :name "Close All Roles For All Objects")
    ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (setf liste (install-points liste))
      (close-all-roles)
      (setf liste (remove-points liste)))))

;;;
;;;
;;;

(defun unclose-role-for-object-i.n. (object role-symbol)
  (let ((classic-ind (get-classic-ind object)))
    (when classic-ind
      (unclose-role-for-classic-ind-i.n. 
       classic-ind
       role-symbol))))

(defun unclose-roles-for-object-i.n. (object role-set)
  (let ((classic-ind (get-classic-ind object)))
    (when classic-ind
      (dolist (role-symbol role-set)
	(unclose-role-for-classic-ind-i.n. classic-ind role-symbol)))))

(defun unclose-role-for-classic-ind-i.n. (classic-ind role-symbol)
  (let* ((cl-ind
	  (if (cl-told-classic-ind? classic-ind)
	      classic-ind
	    (cl-told-ind classic-ind)))
	 (role
	  (cl-named-role role-symbol)))
    
    (when (cl-told-ind-closed-role? cl-ind role)
      (cl-ind-unclose-role classic-ind
			   role))))

(defun unclose-all-roles-for-object-i.n. (object)
  (mapc #'(lambda (role-set)
	    (mapc #'(lambda (rol-sym)
		      (unclose-role-for-object-i.n. object rol-sym))
		  (reverse role-set)))
	(reverse +known-roles+)))

(defun unclose-all-roles-for-classic-ind-i.n. (classic-ind)
  (mapc #'(lambda (role-set)
	    (mapc #'(lambda (rol-sym)
		      (unclose-role-for-classic-ind-i.n. classic-ind rol-sym))
		  (reverse role-set)))
	(reverse +known-roles+)))


(defun unclose-all-roles ()
  (when *info* 
    (format t "~%Unclosing Roles for Objects..."))      
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (role-set (reverse +known-roles+))
	(dolist (object liste)
	  (unclose-roles-for-object-i.n. object (reverse role-set))))))
  (when *info* 
    (format t "  DONE!~%")
    (beep)))


(define-gened-command (com-gened-open-all-roles :name "Open All Roles For All Objects")
    ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (setf liste (install-points liste))
      (unclose-all-roles)
      (setf liste (remove-points liste)))))

;;;
;;;
;;;

(define-gened-command (com-gened-classify :name "Classic, Classify!")
    ()
  (with-application-frame (gened-frame)
    (with-slots (inc-mode liste) gened-frame	
      (when *info* 
	(format t "~%Classic, Classify!~%"))
      (if inc-mode
	  (progn
	    (setf liste (install-points liste))
	    (close-all-roles)
	    (setf liste (remove-points liste)))
	(progn
	  (clear-knowledge-base)
	  (let ((*info* nil))
	    (calc-relations))
	  (setf liste (install-points liste))
	  
	  #| (atomize-all-clusters) |#
	  (create-classic-inds)	
 	  #| (reinstall-all-clusters) |#
	  
	  (close-all-roles)  
	  (setf liste (remove-points liste))))
      (update-parent-concepts)
      (redraw))))

;;;
;;;
;;;

(defun update-parent-concepts-for-object (object)
  (remove-all-concepts object :classic-ind-update nil)
  (let ((classic-ind 
	 (get-classic-ind object)))
    (when classic-ind
      (let* ((parents (mapcar
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
		  (push-concept object parent :classic-ind-update nil)
		  (push-concept object ancestor :ancestor t :classic-ind-update nil))
	      p-concepts
	      a-concepts)))))


(defun update-parent-concepts ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (dolist (object liste)
	(update-parent-concepts-for-object object)))))

;;;
;;;
;;;

(defmethod clear-classic-ind ((object basic-thing))
  (setf (associated-classic-ind object) nil))

(defmethod clear-classic-ind ((object composite-thing))
  (dolist (elem (liste object))
    (clear-classic-ind elem))
  (setf (associated-classic-ind object) nil))

(define-gened-command (com-gened-clear-kb :name "Clear Knowledge-Base")
    ()
  (with-application-frame (gened-frame)
      (let ((yes-or-no (notify-user gened-frame
				    "Clear Knowledge-Base selected! Are you sure?"
				    :style :question)))
	(when yes-or-no
	  (clear-knowledge-base)))))


(defun clear-knowledge-base ()
  (with-application-frame (gened-frame)
    (with-slots (liste stored-relations) gened-frame    
      (cl-clear-kb)
      (setf liste (install-points liste))
      (dolist (elem liste)
	(clear-classic-ind elem))
      (setf liste (remove-points liste))
      (setf stored-relations nil))))
