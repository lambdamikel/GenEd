;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;; Writer-Methods
;;;


(defmethod write-object ((object t) stream &key &allow-other-keys)
  (declare (ignore stream))
  ())

(defun write-it (object stream)
  (write object :stream stream)
  (terpri stream))

(defmethod write-object ((object filled-mixin) stream &key &allow-other-keys)
  (with-slots (filledp filled-ink) object
    (write-it 'filled-mixin stream)
    (write-it filledp stream)
    (write-it filled-ink stream)
  (call-next-method)))

(defmethod write-object ((object thing) stream &key &allow-other-keys)
  (with-slots (hidden xtrans ytrans parent-concepts ancestor-concepts) object
    (write-it 'thing stream)
    (write-it parent-concepts stream)
    (write-it ancestor-concepts stream)
    (write-it xtrans stream)
    (write-it ytrans stream)
    (write-it hidden stream)
    (call-next-method)))

(defmethod write-object ((object ink-mixin) stream &key &allow-other-keys)
  (with-slots (ink) object
    (write-it 'ink-mixin stream)
    (write-it ink stream))
  (call-next-method))

(defmethod write-object ((object linestyle-mixin) stream &key &allow-other-keys)
  (with-slots (linestyle) object
    (write-it 'linestyle-mixin stream)
    (write-it linestyle stream)
  (call-next-method)))


(defmethod write-object ((object linethickness-mixin) stream &key &allow-other-keys)
  (with-slots (linethickness) object
    (write-it 'linethickness-mixin stream)
    (write-it linethickness  stream))
  (call-next-method))

(defmethod write-object ((object basic-thing) stream &key &allow-other-keys)
  (write-it 'basic-thing stream)
  (call-next-method))

(defmethod write-object ((object composite-thing) stream &key &allow-other-keys)
  (with-slots (liste) object
    (write-it 'composite-thing stream)     
    (write-it (length liste) stream)     
    (dolist (elem liste) 
      (write-it (type-of elem) stream)
      (write-object elem stream))
    (call-next-method)))

(defmethod write-object ((object g-rectangle) stream &key &allow-other-keys)
  (with-slots (xextend yextend) object
    (write-it 'g-rectangle  stream)     
    (write-it xextend  stream)     
    (write-it yextend  stream)     
    (call-next-method)))

(defmethod write-object ((object g-arrow) stream &key &allow-other-keys)
  (write-it 'g-arrow  stream)   
  (call-next-method))

(defmethod write-object ((object linesegment) stream &key &allow-other-keys)
  (with-slots (startpoint endpoint) object
    (write-it 'linesegment stream)
    (write-it startpoint  stream)     
    (write-it endpoint  stream)     
    (call-next-method)))

(defmethod write-object ((object object-with-head) stream &key &allow-other-keys)
  (write-it 'object-with-head stream)
  (write-it (head object) stream)
  (call-next-method))

(defmethod write-object ((object pointlist-object) stream &key &allow-other-keys)
  (write-it 'pointlist-object stream)
  (with-slots (pointlist) object
    (write-it (/ (length pointlist) 2)  stream)     
    (dolist (point (reverse (generate-brackets pointlist)))
      (write-it (first point) stream)       
      (write-it (second point) stream))
    (call-next-method)))

(defmethod write-object ((object g-polygon) stream &key &allow-other-keys)
  (write-it 'g-polygon  stream)   
  (call-next-method))

(defmethod write-object ((object g-chain) stream &key &allow-other-keys)
  (write-it 'g-chain  stream)   
  (call-next-method))

(defmethod write-object ((object spline-object) stream &key &allow-other-keys)
  (write-it 'spline-object stream)
  (write-it (spline-points object) stream)   
  (call-next-method))

(defmethod write-object ((object g-spline-chain) stream &key &allow-other-keys)
  (write-it 'g-spline-chain stream)   
  (call-next-method))

(defmethod write-object ((object g-spline-polygon) stream &key &allow-other-keys)
  (write-it 'g-spline-polygon stream)   
  (call-next-method))

(defmethod write-object ((object g-circle) stream &key &allow-other-keys)
  (with-slots (radius) object
    (write-it 'g-circle stream)     
    (write-it radius stream)     
    (call-next-method)))

(defmethod write-object ((object g-text) stream &key &allow-other-keys)
  (with-slots (text-string face size family) object
    (write-it 'g-text stream)     
    (write-it text-string stream)     
    (write-it face stream)     
    (write-it family stream)     
    (write-it size stream)     
    (call-next-method)))

      
(defmethod write-object ((object g-point) stream &key &allow-other-keys)
  (write-it 'g-point  stream)   
  (call-next-method))

(defmethod write-object ((object object-with-handles) stream &key &allow-other-keys)  
  (with-slots (handles) object
    (write-it 'object-with-handles stream)
    (write-it (length handles)  stream)     
    (dolist (handle (reverse handles))
      (write-it (type-of handle)  stream)       
      (write-it (x handle)  stream)       
      (write-it (y handle)  stream)
       )
    (call-next-method)))

(defmethod write-object ((object scaleable-thing) stream &key &allow-other-keys)
  (with-slots (xscale yscale init-x-extend init-y-extend) object
    (write-it 'scaleable-thing stream)
    (write-it xscale  stream)     
    (write-it yscale  stream)     
    (write-it init-x-extend  stream)     
    (write-it init-y-extend  stream)     
    (call-next-method)))
    
(defmethod write-object ((object rotateable-thing) stream &key &allow-other-keys)
  (with-slots (rotangle) object
    (write-it 'rotateable-thing stream)
    (write-it rotangle  stream)     
    (call-next-method)))

;;;
;;; Reader Methods
;;;

(defmethod read-object ((object t) stream &key  )
  (declare (ignore stream))
  (get-bounding-rect object))

(defmethod read-object ((object thing) stream &key  )
  (with-slots (xtrans ytrans hidden parent-concepts ancestor-concepts id-number) object
    (read stream)
    (setf parent-concepts (read stream))
    (setf ancestor-concepts (read stream))
    (setf xtrans (read stream))
    (setf ytrans (read stream))
    (setf hidden (read stream))
    (setf id-number (get-id-number)))
  (call-next-method  ))


(defmethod read-object ((object filled-mixin) stream &key  )
  (with-slots (filledp filled-ink) object
    (read stream)
    (setf filledp (read stream))
    (setf filled-ink (read stream)))
  (call-next-method  ))

(defmethod read-object ((object ink-mixin) stream &key  )
  (with-slots (ink) object
    (read stream)
    (setf ink (read stream)))
  (call-next-method  ))

(defmethod read-object ((object linestyle-mixin) stream &key  )
  (with-slots (linestyle) object
    (read stream)
    (setf linestyle (read stream)))
  (call-next-method  ))


(defmethod read-object ((object linethickness-mixin) stream &key  )
  (with-slots (linethickness) object
    (read stream)
    (setf linethickness (read stream)))
  (call-next-method  ))

(defmethod read-object ((object basic-thing) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object composite-thing) stream &key  )
  (with-slots (liste) object
    (read stream)
    (dotimes (n (read stream))
      (let ((object (make-instance (read stream))))
	(read-object object stream)
	(push object liste)))
    (call-next-method  )))

(defmethod read-object ((object g-rectangle) stream &key  )
  (with-slots (xextend yextend) object
    (read stream)
    (setf xextend (read stream))
    (setf yextend (read stream))
    (call-next-method  )))

(defmethod read-object ((object g-circle) stream &key  )
  (with-slots (radius) object
    (read stream)
    (setf radius (read stream))
    (call-next-method  )))

(defmethod read-object ((object linesegment) stream &key  )
  (with-slots (startpoint endpoint) object
    (read stream)
    (setf startpoint (read stream))
    (setf endpoint (read stream))
    (call-next-method  )))

(defmethod read-object ((object g-arrow) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object object-with-head) stream &key  )
  (read stream)
  (setf (head object) (read stream))
  (call-next-method  ))

(defmethod read-object ((object g-polygon) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object g-chain) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object spline-object) stream &key  )
  (read stream)
  (with-slots (spline-points) object
    (setf spline-points (read stream)))
  (call-next-method  ))

(defmethod read-object ((object g-spline-chain) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object g-spline-polygon) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object pointlist-object) stream &key  )
  (read stream)
  (with-slots (pointlist) object 
    (let ((counter (read stream))
	  (x) (y))
      (dotimes (i counter)
	(setf x (read stream))
	(setf y (read stream))		 
	(push y pointlist)
	(push x pointlist))
      (call-next-method  ))))

(defmethod read-object ((object g-point) stream &key  )
  (read stream)
  (call-next-method  ))

(defmethod read-object ((object g-text) stream &key  )
  (read stream)
  (with-slots (text-string face size family) object
    (setf text-string (read stream))
    (setf face (read stream))
    (setf family (read stream))
    (setf size (read stream))
    (call-next-method  )))

(defmethod read-object ((object object-with-handles) stream &key  )
  (read stream)
  (let ((counter (read stream))
	(handle))
    (dotimes (i counter)
      (setf handle (make-instance (read stream)))
      (setf (x handle) (read stream))
      (setf (y handle) (read stream))
      (setf (parent-object handle) object)
      (push handle (handles object)))          
    (call-next-method  )))

(defmethod read-object ((object scaleable-thing) stream &key  )
  (read stream)
  (with-slots (xscale yscale init-x-extend init-y-extend) object
    (setf xscale (read stream))   
    (setf yscale (read stream))
    (setf init-x-extend (read stream))
    (setf init-y-extend (read stream))
    (call-next-method  )))
    
(defmethod read-object ((object rotateable-thing) stream &key  )
  (read stream)
  (with-slots (rotangle) object
    (setf rotangle (read stream))
    (call-next-method)))

    
;;;
;;; Save & Load
;;;


(defun file-selector (title directory)
  (with-application-frame (gened-frame)
    (let* ((file 
            (select-file gened-frame
                         :title title
                         :directory directory))
           (file
            (and file 
                 (namestring file))))
      (if (and file (not (string= "" file)))
	  (if (char= (elt file
			  (1- (length file)))
		     #\/)
	      (progn
		(notify-user gened-frame
			     "You must select a file, not a directory!"
			     :style :error)
		nil)
	    file)
	(progn
	  (notify-user gened-frame
		       (format nil "No file selected!")
		       :style :error)
	  nil)))))
	

(define-gened-command (com-gened-save-scene :name "Save Scene")
    ()
  (with-application-frame (gened-frame) 
    (let ((file (file-selector "Save Scene" +scenes-dir+)))
      (when file
		   
	(when (and (probe-file file) 
		   (not (notify-user gened-frame
				     (format nil "Overwrite ~A" file) :style :warning))) 
	  (return-from
	      com-gened-save-scene))
	(with-open-file (output-stream file
			 :direction :output :if-exists :supersede :if-does-not-exist
			 :create) 
	  (with-standard-io-syntax 
	    (write 'scene-data :stream
		   output-stream) 
	    (terpri output-stream)
	    (with-slots (liste)	gened-frame 
	      (write (length liste) :stream output-stream) 
	      (terpri output-stream) 
	      (dolist (object (reverse liste)) 
		(write-it (type-of object) output-stream)
		(write-object object output-stream)))))
	(setf *current-file* file)))))



(define-gened-command (com-gened-load-scene :name "Load Scene")
    ()
  (with-application-frame (gened-frame)
    (let ((file (file-selector "Load Scene" +scenes-dir+))
	  (objects 0)
	  (nowobject))      
      
      (when file
	
	(unless (probe-file file)
	  (notify-user gened-frame
		       (format nil "File ~A does not exist" file)
		       :style :error)
	  (return-from com-gened-load-scene))
	(new)
	(with-slots (liste) gened-frame
	  (with-open-file (s file :direction :input)	  
	    (with-standard-io-syntax	    
	      (unless (eq (read s) 'scene-data)
		(notify-user gened-frame
			     (format nil "File ~A is not a scene-file!" file)
			     :style :error)
		(return-from com-gened-load-scene))
	    
	      (setf objects (read s))
	      (dotimes (num objects)
		(setf nowobject (make-instance (read s) :initialize nil))
		(push nowobject liste)
		(read-object nowobject s)
		(make-poly-representation nowobject))))
	  (dolist (elem liste)
	    (if (typep elem 'object-with-handles)
		(dolist (handle (handles elem))
		  (if (typep handle 'fixed-object-handle)
		      (fix-handle handle)))))
	  (setf *current-file* file)
	  (make-undo-object nil 'load-scene)
	  (do-incremental-updates))))))
  
;;;
;;;
;;;

(define-gened-command (com-gened-save-library :name "Save Library")
    ()
  (with-application-frame (gened-frame)
    (let ((file (file-selector "Save Library"
			       +libraries-dir+)))

      (when file	
	
	(when (and (probe-file file)
		   (not (notify-user gened-frame (format nil "Overwrite ~A" file)
				     :style :warning)))
	  (return-from com-gened-save-library))
	
	(with-open-file (output-stream file :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-standard-io-syntax	  
	    
	    (write 'library-data :stream output-stream)
	    (terpri output-stream)
	    
	    (write (length *library*)
		   :stream output-stream)
	    (terpri output-stream)
	    (dolist (lib-item (reverse *library*))
	      (write (first lib-item)
		     :stream output-stream)
	      (terpri output-stream)
	      (write-it (type-of (second lib-item)) output-stream)
	      (write-object (second lib-item) output-stream))))))))


(define-gened-command (com-gened-load-library :name "Load Library")
    ()
  (with-application-frame (gened-frame)
    (let ((file (file-selector "Load Library"
			       +libraries-dir+)))
      (when file
	(unless (probe-file file)
	  (notify-user gened-frame (format nil "File ~A does not exist" file)
		       :style :error)
	  (return-from com-gened-load-library))
	
	(with-open-file (s file :direction :input)
	  (with-standard-io-syntax	  
	    
	    (unless (eq (read s) 'library-data)
	      (notify-user gened-frame 
			   (format nil "File ~A is not a library-file!" file)
			   :style :error)
	      (return-from com-gened-load-library))	    
	    
	    (let ((length (read s)))
	      (dotimes (i length)
		(let ((label (read s))
		      (now-object (make-instance (read s) :initialize nil)))
		  (push (list label now-object) *library*)
		  (read-object now-object s)
		  (make-poly-representation now-object))))))))))		             

;;;
;;;
;;;

(defmethod save-object ((object thing))
  (with-application-frame (gened-frame)
    (let ((file (file-selector "Save Object"
			       +objects-dir+)))
      (when file
	(when (and (probe-file file)
		   (not (notify-user gened-frame (format nil "Overwrite ~A" file)
				     :style :warning)))
	  (return-from save-object))
	
	(with-open-file (output-stream file :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-standard-io-syntax
	    (write 'object-data :stream output-stream)
	    (terpri output-stream)
	    (write-it (type-of object) output-stream)
	    (write-object object output-stream)))))))


(define-gened-command (com-save-object)
    ((object 'thing :gesture nil))
  (save-object object))
    

(define-gened-command (com-gened-save-object :name "Save Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(save-object source-object))))

;;;
;;;
;;;

(defun load-object (file)
  (when file
    (unless (probe-file file)
      (return-from load-object 'error-does-not-exists))
    
    (with-open-file (s file :direction :input)
      (with-standard-io-syntax
	
	(unless (eq (read s) 'object-data)
	  (return-from load-object 'error-not-an-object-file))
	
	(let ((now-object (make-instance (read s) :initialize nil)))
	  (read-object now-object s)
	  now-object)))))

(defun load-query-object ()
  (with-application-frame (gened-frame)
    (with-slots (liste) gened-frame
      (let* ((file (file-selector "Load Data"
				  +objects-dir+)))
	(when file
	  (let ((object (load-object file)))
	    (case object
	      (error-does-not-exists
	       (notify-user gened-frame (format nil "File ~A does not exist" file)
			:style :error))
	      (error-not-an-object-file      
	       (notify-user gened-frame 
			    (format nil "File ~A is not an object-file!" file)
			    :style :error))
	      (otherwise	 
	       (make-poly-representation object)
	       (push object liste)
	       (make-undo-object object 'load-object)
	       (do-incremental-updates)))))))))


(define-gened-command (com-gened-load-object :name "Load Object")
    ()
  (load-query-object))

;;;
;;;
;;;

#+:allegro
(define-gened-command (com-gened-print-scene :name "Print Scene")
    ()
  (with-application-frame (gened-frame)
    (with-open-stream 
	(pipe (excl:run-shell-command  (format nil "lpr -P~A" '|r131_hp|)
				       :input :stream :wait nil))
      (with-output-to-postscript-stream (stream pipe 
						:orientation :landscape
						:scale-to-fit t)
	(with-slots (liste) gened-frame
	  (dolist (object (reverse liste))
	    (draw object stream :printing-active t)))))))

(define-gened-command (com-gened-save-postscript :name "Save Scene As Postscript")
    ()
  (with-application-frame (gened-frame)
    (let ((file (file-selector "Save Postscript-File"
			       +prints-dir+))
	  (*global-scaling* 1))
      (when file
	(when (and (probe-file file)
		   (not (notify-user gened-frame (format nil "Overwrite ~A" file)
				     :style :warning)))
	  (return-from com-gened-save-postscript))
	(with-open-file (output-stream file :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-standard-io-syntax	  
	    (with-output-to-postscript-stream (stream output-stream 
						    :orientation :portrait
						    :scale-to-fit t)	    
	      (with-translation (stream 0 150)
		(with-slots (liste) gened-frame
		  (dolist (object (reverse liste))
		    (draw object stream :printing-active t)))))))))))
