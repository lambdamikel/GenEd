;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defclass undo-object ()
  ((object-copy :initarg :object-copy :accessor object-copy)
   (original-object :initarg :original-object :accessor original-object)
   
   (operation :initarg :operation :accessor operation)))

;;;
;;;
;;;

(defun init-undo-buffer ()
  (with-application-frame (gened-frame) 
    (with-slots (object-copy-buffer
		 buffer-counter) gened-frame
      (setf buffer-counter 0)
      (setf object-copy-buffer
	(make-array +buffer-size+)))))    


(defun make-undo-object (object operation)  
  
  (menu-name-for-undo-command object operation) 

  (with-application-frame (gened-frame)
    (with-slots (object-copy-buffer 
		 buffer-counter) gened-frame
      
      (setf buffer-counter
	(mod (1+ buffer-counter) +buffer-size+))
      
      (case operation
	
	((create cluster load-object copy)
	 (setf (aref object-copy-buffer buffer-counter) 
	   (make-instance 'undo-object
	     :object-copy nil
	     :original-object object
	     :operation operation)))	 
	
	(load-scene
	 (setf (aref object-copy-buffer buffer-counter) 
	   (make-instance 'undo-object
	     :object-copy nil
	     :original-object nil
	     :operation operation)))	 	
	
	(otherwise
	 (let ((copy (make-instance (type-of object)
		       :initialize nil)))
	   (copy-values object copy)
	    
	   (setf (aref object-copy-buffer buffer-counter) 
	     (make-instance 'undo-object
	       :object-copy copy
	       :original-object object
	       :operation operation))
	    
	   (setf (id-number copy)
	     (id-number object))
	    
	   (if (typep object 'composite-thing)
	       (mapc #'(lambda (part copy-part)
			 (setf (id-number copy-part)
			   (id-number part)))
		     (liste object) (liste copy)))))))))
		    
	
(defun unstack-undo-object ()
  (with-application-frame (gened-frame)
    (with-slots 
	(object-copy-buffer 
	 buffer-counter liste) gened-frame
      (let ((object
	     (aref object-copy-buffer buffer-counter)))
	
	(setf (aref object-copy-buffer buffer-counter) nil)
	(setf buffer-counter (mod (1- buffer-counter) +buffer-size+))
	
	 (let ((next-object (get-undo-object)))
	  (if next-object
	      (menu-name-for-undo-command (original-object next-object)
					  (operation next-object))
	    (menu-name-for-undo-command nil nil)))
	 object))))

(defun menu-name-for-undo-command  (object operation)
  (remove-command-from-command-table 
   'com-gened-undo 'manipulate-table)
  
  (if (and object operation)
      (add-command-to-command-table 'com-gened-undo
				    'manipulate-table
				    :menu (list 
					   (concatenate 'string
					     "UNDO "
					     (command-name-from-symbol operation)
					     " "
					     (write-to-string object))
					   ':after  ':start))
    (add-command-to-command-table 'com-gened-undo
				  'manipulate-table
				  :menu (list 
					 "UNDO NOTHING"
					 ':after  ':start))))
	
(defun get-undo-object ()
  (with-application-frame (gened-frame)
    (with-slots 
	(object-copy-buffer buffer-counter) gened-frame
      (aref object-copy-buffer buffer-counter))))

(defun undo ()
  (with-application-frame (gened-frame)

    (with-slots 
	(object-copy-buffer 
	 buffer-counter liste) gened-frame
      
      (let* ((undo-object 
	      (unstack-undo-object)))	
	
	(if undo-object	    
	    (let ((operation (operation undo-object))
		  (original-object 
		   (original-object undo-object))
		  (object-copy 
		   (object-copy undo-object)))
	      
	      (format *standard-output*
		      "~%---> UNDOING   ~a   :    ~a~%" 
		      operation
		      original-object)   
	      
	      (case operation
		
		(delete
		 (make-poly-representation object-copy)
		 (push object-copy liste))
		
		((create load-object copy)
		 (let ((object (get-object (id-number original-object))))
		   (delete-object object :undo-object nil)))
		
		(cluster
		 (let ((object (get-object (id-number original-object))))		   
		   (atomize-cluster object)))
		
		(load-scene
		 (com-gened-new))
		
		(otherwise
		 (delete-polyrep-from-cache  
		  (get-object (id-number original-object)))		  
		 (setf liste
		   (mapcar #'(lambda (elem)
			       (if (= (id-number elem) 
				      (id-number object-copy))
				   object-copy
				 elem))
			   liste))
		 (make-poly-representation object-copy)))
	      (redraw)	      
	      (do-incremental-updates :backward t)))
	undo-object))))

(define-gened-command (com-gened-undo :name "Undo Last Operation")
    ()      
  (unless (undo)
    (beep)))

