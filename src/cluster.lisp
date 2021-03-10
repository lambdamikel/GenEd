;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(define-gened-command (com-gened-build-cluster :name "Build Composite Object")
    ()
  (when *info*
    (format t "~%Hold Shift & Left Mouse-Button~% And Drag A Rectangle Over Objects To Compose!~%Release Mouse-Button To Finish!~%"))

  (with-application-frame (gened-frame)
    (if (any-visible-objects)
	(with-slots (liste) gened-frame
	  (let ((stream (get-frame-pane gened-frame 'display)))
	    (multiple-value-bind (xf yf xt yt)
		(pointer-input-rectangle*
		 :stream stream :finish-on-release t)
	      (multiple-value-bind (xf yf)
		  (scale-mouse-position xf yf :inverse t)
		(multiple-value-bind (xt yt)
		    (scale-mouse-position xt yt :inverse t)
		  (let ((rect (make-rectangle* xf yf xt yt))
			(collect ()))
		    (dolist (elem liste)
		      (let ((xtrans (xtrans elem))
			    (ytrans (ytrans elem)))
			(when (region-contains-position-p
			       rect xtrans ytrans)
			  (push elem collect)
			  (free-my-handles elem)
			  (free-all-handles elem)
			  (setf liste (delete elem liste :test #'equal)))))
		    (if (not (null collect))
			(let ((object (make-instance 'composite-thing :liste collect :initialize t)))
			  (init-object-properties object)
			  (push object liste)
			  (make-undo-object object 'cluster)
			  (do-incremental-updates))
		      (notify-user gened-frame
				   "Nothing composed! OK?"
				   :style :inform)))))))))))    


(defmethod atomize-cluster ((object composite-thing))
  (with-application-frame (gened-frame)     
    (with-slots (liste) gened-frame
      (dolist (elem (liste object))
	(push elem liste))
      (setf liste (delete object liste))
      (when (attached-handles object)
	(dolist (handle (attached-handles object))
	  (free-handle handle))))))

;;;
;;;
;;;

(defun atomize-all-clusters ()
  (with-application-frame (gened-frame)
    (with-slots (liste saved-liste) gened-frame
      
      (setf saved-liste (copy-list liste))
      
      (dolist (elem liste)
	(setf (part-of-cluster elem) nil))            
      
      (loop
	(if
	    (some #'(lambda (elem) (typep elem 'composite-thing)) liste)
	    
	    (dolist (cluster liste)
	      
	      (when (typep cluster 'composite-thing)
				
		(setf liste (delete cluster liste))

		(dolist (cluster-part (liste cluster))
		  (push cluster-part liste)
		  (setf (part-of-cluster cluster-part) cluster))))
	  
	  (return))))))	

(defun reinstall-all-clusters ()
  (with-application-frame (gened-frame)
    (with-slots (liste saved-liste) gened-frame
      (if saved-liste 
	  (setf liste saved-liste)))))

;;;
;;;
;;;

(define-gened-command (com-gened-atomize-cluster :name "Decompose Composite Object")
    ()
  (if (any-visible-clusters)
      (let ((source-object (accept 'composite-thing)))
	(terpri)
	(atomize-cluster source-object)
	(do-incremental-updates))))

(define-gened-command (com-decompose-composite-object)
    ((object 'composite-thing :gesture :atomize))
  (atomize-cluster object)
  (do-incremental-updates))

