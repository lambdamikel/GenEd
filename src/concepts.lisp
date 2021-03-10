;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defun generate-name (symbol)
  (write-to-string
   symbol :escape nil))

(defun construct-concept-menu ()
  (mapcar #'(lambda (concept)
	      (list concept ':items 
		    (let ((num 0))
		      (loop for visu in (get-visualisations-of-concept concept) collect 
			    (cons (list concept num) visu) do
			    (incf num)))))
	  +library-concepts+))		      

(defun select-visualisation ()
   (let ((*handles-symbol-list* nil)
	(*concept-labels* nil)
	(*bounding-boxes* t)
	(*global-scaling* 1))
     (load-concepts)

     (with-application-frame (gened-frame)
       (if (null +library-concepts+)
	   (notify-user gened-frame
			"No known concepts at all!"
			:style :error)	     	  
	 (let ((item 
		(menu-choose (construct-concept-menu) 
			     :label "Select A Concept-Visualisation!"
			     :printer #'(lambda (item stream)
					  (if (not (member ':items item))
					      (let ((graphic-object (third item)))
						(multiple-value-bind (xf yf xt yt)
						    (get-bounding-rect graphic-object)
						  (with-scaling (stream (/ 50 (- xt xf))
									(/ 50 (- yt yf)))
						    (draw graphic-object stream))))
					    (princ (first item) stream))))))
	   (if (and item (atom item))
	       (progn 
		 (notify-user gened-frame
			      (format nil "No visualisation selected for ~A!"
				      item)
			      :style :error)
		 nil)
	     item))))))

(defun select-concept (object &key (only-library-concepts nil))
  (let ((object
	 (cond ((typep object 'start-handle)
		(start-info-point (parent-object object)))
	       ((typep object 'end-handle)
		(end-info-point (parent-object object)))
	       (t object))))
    (let ((concepts 
	   (let ((concepts 
		  (remove-duplicates 
		   (append (parent-concepts object)
			   (ancestor-concepts object)))))
	     (if only-library-concepts
		 (reverse 
		  (intersection 
		   +library-concepts+
		   concepts))
	       concepts))))	       
      (if (null concepts)
	  'error
	(menu-choose 
	 concepts
	 :label "Select One Of Objects Concepts!")))))
  
(define-gened-command (com-gened-select-concept-visualisation :name "Select Concept-Visualisation")
    ()
  (with-application-frame (gened-frame)
    (with-slots (concept-type) gened-frame       
      (let ((item 
	     (select-visualisation)))	    
	(setf concept-type item)))))

;;;
;;;
;;;

(defmethod remove-concept-from-object ((object thing) &key (classic-ind-update t))
  (with-application-frame (gened-frame)
    (let ((item (select-concept object)))
      (if (eq item 'error)
	  (notify-user gened-frame
		       "No more concepts to remove!" 
		       :style :error)	
	(unless (null item)
          #+:classic
	  (when classic-ind-update
	    (update-classic-ind-concepts object :remove t))
	  (setf (parent-concepts object)
	    (delete item (parent-concepts object)))
	  (setf (ancestor-concepts object)
	    (delete item (ancestor-concepts object)))
          #+:classic
	  (when classic-ind-update
	    (update-classic-ind-concepts object))))))
  (only-tick-object object))

(defmethod remove-concept-from-object ((object start-handle) &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-concept-from-object 
     (start-info-point object)
     :classic-ind-update classic-ind-update)))

(defmethod remove-concept-from-object ((object end-handle) &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-concept-from-object 
     (end-info-point object)
     :classic-ind-update classic-ind-update)))

(define-gened-command (com-gened-remove-concept :name "Remove Concept From Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept '(or thing object-handle))))
	(terpri)
	(remove-concept-from-object source-object))))
  
(define-gened-command (com-remove-concept-from-object)
    ((object '(or thing object-handle) :gesture nil))
  (remove-concept-from-object object))

;;;
;;;
;;;

(define-gened-command (com-gened-remove-from-library :name "Remove Object From Library")
    ()
  (let ((item 
	 (select-visualisation)))	    
    (when item
      (setf *library*
	(remove 
	 (list (first item)
	       (get-nth-visualisation-of-concept 
		(first item) (second item)))
	 *library* :test #'equal)))))
  
;;;
;;;
;;;

(defun get-concept-name (symbol)
  (write-to-string symbol :escape nil))

(defmethod push-concept ((object thing) concept-symbol &key (into-library nil)
							    (ancestor nil)
							    (classic-ind-update t))
  (if ancestor
      (unless (member concept-symbol (ancestor-concepts object))
	(push concept-symbol
	      (ancestor-concepts object)))
    (unless (member concept-symbol (parent-concepts object))
      (push concept-symbol
	    (parent-concepts object))))
  (when into-library
    (store-into-library object))
  (only-tick-object object)
  #+:classic-ind
  (when classic-ind-update 
    (update-classic-ind-concepts object)))

(defmethod push-concept ((object start-handle) concept-symbol &key (into-library nil)
								   (ancestor nil)
								   (classic-ind-update t))
  (let ((object (parent-object object)))
    (push-concept
     (start-info-point object)
     concept-symbol
     :into-library into-library
     :ancestor ancestor
     :classic-ind-update classic-ind-update)))

(defmethod push-concept ((object end-handle) concept-symbol &key (into-library nil)
								   (ancestor nil)
								   (classic-ind-update t))
  (let ((object (parent-object object)))
    (push-concept
     (end-info-point object)
     concept-symbol
     :into-library into-library
     :ancestor ancestor
     :classic-ind-update classic-ind-update)))

;;;
;;;
;;;

(defmethod remove-all-concepts ((object thing) &key (classic-ind-update t))
  #+:classic
  (when classic-ind-update 
    (update-classic-ind-concepts object :remove t))
  (setf (parent-concepts object) nil)
  (setf (ancestor-concepts object) nil)
  (only-tick-object object))

(defmethod remove-all-concepts ((object start-handle) &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-all-concepts
     (start-info-point object)	
     :classic-ind-update classic-ind-update)))


(defmethod remove-all-concepts ((object end-handle) &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-all-concepts
     (end-info-point object)	
     :classic-ind-update classic-ind-update)))

(defmethod assign-to-concept ((object t)) ; object thing
  (load-concepts)
  (with-application-frame (gened-frame)
    (if (null +library-concepts+)
	(notify-user gened-frame "No concepts at all!" :style :error)
      (let ((item
	     (menu-choose +library-concepts+
			  :label "Select A Library-Concept!")))
	(unless (null item)
	  (push-concept object item))))))

;;;
;;;
;;;

(defmethod store-into-library ((object thing))
  (with-application-frame (gened-frame)
    (let ((concept (select-concept object :only-library-concepts t)))
      (if (eq concept 'error)
	  (notify-user gened-frame "No concepts at all!" :style :error)
	(if (not (null concept))
	    (let ((visualisations 
		   (mapcar #'second
			   (get-visualisations-of-concept 
			    concept))))
	      (unless (member object visualisations :test #'eq)
		(let ((destination-object (make-instance (type-of object) :initialize nil)))
		  (setf (parent-concepts destination-object) 
		    (list concept))
		  (copy-values object destination-object)		    
		  (push (list 
			 concept
			 destination-object)
			*library*))))
	  (notify-user gened-frame "No concept selected!" :style :error))))))

;;;
;;;
;;;

(define-gened-command (com-gened-assign-to-concept :name "Assign Concept To Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept '(or thing object-handle))))
	(terpri)
	(assign-to-concept source-object))))

(define-gened-command (com-assign-concept-to-object)
    ((object '(or thing object-handle) :gesture nil))
  (assign-to-concept object))

;;;
;;;
;;;


(define-gened-command (com-gened-store-into-library :name "Store Object Into Library")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(store-into-library source-object))))

(define-gened-command (com-store-object-into-library)
    ((object 'thing :gesture nil))
  (store-into-library object))

;;;
;;;
;;;

(define-gened-command (com-gened-clear-library :name "Clear Library")
    ()
  (with-application-frame (gened-frame)
    (let ((yes-or-no (notify-user gened-frame
				  "Clear Library selected! Are you sure?"
				  :style :question)))
      (if yes-or-no
	  (With-slots (liste) gened-frame		      
		      (setf *library* nil))))))

;;;
;;;
;;;

(defmethod remove-all-but-basic-concept ((object thing) &key (classic-ind-update t)) 
  (remove-all-but-one-concept 
   object 
   (get-basic-concept object)
   :classic-ind-update
   classic-ind-update))

(defmethod remove-all-but-basic-concept ((object object-handle) &key (classic-ind-update t)) 
  (remove-all-but-one-concept 
   object 'info-point :classic-ind-update
   classic-ind-update))

(define-gened-command (com-gened-only-basic-concept :name "Only Basic-Concept For Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept '(or thing object-handle))))
	(terpri)
	(remove-all-but-basic-concept source-object))))

(define-gened-command (com-only-basic-concept-for-object)
    ((object '(or thing object-handle) :gesture nil))
  (remove-all-but-basic-concept object))

;;;
;;;
;;;

(defmethod remove-all-but-one-concept ((object thing) 
				       concept &key (classic-ind-update t))
  #+:classic 
  (when classic-ind-update
    (update-classic-ind-concepts object :remove t))
  (setf (parent-concepts object)
    (list concept))
  (setf (ancestor-concepts object) nil)
  (only-tick-object object)
  #+:classic
  (when classic-ind-update
    (update-classic-ind-concepts object)))
					
(defmethod remove-all-but-one-concept ((object start-handle) 
				       concept &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-all-but-one-concept
     (start-info-point object)
     concept
     :classic-ind-update classic-ind-update)))

(defmethod remove-all-but-one-concept ((object end-handle) 
				       concept &key (classic-ind-update t))
  (let ((object (parent-object object)))
    (remove-all-but-one-concept
     (end-info-point object)
     concept
     :classic-ind-update classic-ind-update)))

;;;
;;;
;;;

(defmethod only-one-concept ((object t))
  (with-application-frame (gened-frame)
    (if (null +library-concepts+)
	(notify-user gened-frame "No concepts at all!" :style :error)
      (let ((item
	     (menu-choose +library-concepts+
			  :label "Select A Library-Concept!")))
	(unless (null item)
	  (remove-all-but-one-concept object item))))))

(define-gened-command (com-gened-only-one-concept :name "Only One Concept For Object")
    ()
  (if (any-visible-objects)
      (let* ((source-object (accept '(or thing object-handle))))
	(terpri)
	(only-one-concept source-object))))

(define-gened-command (com-only-one-concept-for-object)
    ((object '(or thing object-handle) :gesture nil))
  (only-one-concept object))

;;;
;;;
;;;

(define-gened-command (com-gened-only-basic-concept-all-objects :name "Only Basic-Concept For All Objects")
    ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (setf liste (install-points liste))
      (dolist (object liste)
	(remove-all-but-basic-concept object))
      (setf liste (remove-points liste))))    
  (redraw))

  
						    
