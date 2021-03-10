;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;
	   
(define-application-frame gened ()
  ( (liste :initform nil)
    (stored-relations :initform nil)
    
    (saved-liste :initform nil)
    (prev-op-liste :initform nil)
    
    (mode :initform 'g-circle)
    (concept-type :initform nil)

    (calc-int-dim :initform nil)
    (inc-mode :initform nil)
    
    (default-arrow-head :initform t)
    (default-line-thickness :initform 2)
    (default-line-style :initform :solid)    
    (default-ink :initform 0.4)		
    
    
    (default-text-family :initform :sans-serif)
    (default-text-face :initform :bold)    
    (default-text-size :initform :normal)
    
    (default-filled-ink :initform 0.8)
    (default-filled :initform nil)
        
    (buffer-counter :initform 0)        
    
    (object-copy-buffer :initform (make-array +buffer-size+)))
  
  (:command-table (gened
		   :inherit-from (concept-table file-table tool-table manipulate-table reasoning-table
						spatial-table)
		   :menu (("File" :menu file-table)
			  ("Tools" :menu tool-table)
			  ("Manipulate" :menu manipulate-table)
			  ("Reasoning" :menu reasoning-table)
			  ("Concepts" :menu concept-table)
			  ("Spatial" :menu spatial-table))))

  (:panes   
   
   (pointer-documentation-pane
    (make-clim-stream-pane 
     :type 'pointer-documentation-pane
     :foreground +white+
     :background +black+
     :text-style (make-text-style
		  :sans-serif :bold :small)
     :scroll-bars nil
     :min-height '(1 :line)
     :height '(1 :line)))
   
   (display :application
	    :display-function 'draw-gened
	    :textcursor nil
	    :width 600
	    :redisplay-after-commands nil
	    :incremental-redisplay t)
   
   (label :application
	  :scroll-bars nil
	  :min-width 200
	  :redisplay-after-commands nil
	  :incremental-redisplay t
	  :display-function 'draw-label
	  :foreground +white+
	  :background +black+)
   
   (infos :application
	  :textcursor nil
	  :text-style
	  (parse-text-style '(:sans-serif :roman :very-small))
	  :width 300
	  :end-of-line-action :allow
	  :scroll-bars :both
	  :incremental-redisplay nil)     	  
   
   (command :interactor
	    :height '(2 :line)) 
   
   (options1 :accept-values
	     :min-height :compute
	     :min-width :compute :width :compute :max-width :compute
	     :scroll-bars nil
	     :display-function
	     `(accept-values-pane-displayer
	       :displayer ,#'(lambda (frame stream)
			       (accept-options1
				frame stream))))   
     
   (options2 :accept-values
	     :min-height :compute
	     :min-width :compute :width :compute :max-width :compute
	     :scroll-bars nil
	     :display-function
	     `(accept-values-pane-displayer
	       :displayer ,#'(lambda (frame stream)
			       (accept-options2
				frame stream))))
   
   (elements :accept-values
	     :scroll-bars nil
	     :min-height :compute :height :compute :max-height :compute
	     :display-function
	     `(accept-values-pane-displayer
	       :displayer ,#'(lambda (frame stream)
			       (accept-elements
				frame stream))))

   (buttons :accept-values
	    :scroll-bars nil
	    :min-height :compute :height :compute :max-height :compute
	    :display-function
	    `(accept-values-pane-displayer
	     #| :resynchronize-every-pass t |#
	      :displayer ,#'(lambda (frame stream)
			      (accept-buttons
			       frame stream)))))

  
  (:layouts
   (:default
       (vertically ()	 
	 (horizontally ()
	     (vertically ()
	       elements
	       options1
	       options2)
	     (outlining (:thichness 2) 
	       label))
	 (horizontally ()
	   buttons
	   display 
	   infos)	   
	 command 
	 pointer-documentation-pane))))

#|
(defmethod read-frame-command ((frame gened) &key (stream *standard-input*))
  (read-command (frame-command-table frame) :use-keystrokes t :stream stream))
|#

(defun accept-it (stream type default prompt query-id &key (view 'gadget-dialog-view)
							   (min-width 200)
							   (min-height 20)
							   orientation)
  (let (object ptype changed)
      (formatting-cell (stream :align-x (ecase orientation
					  (:horizontal :left)
					  (:vertical :center))
			       :align-y :center
			       :min-width min-width
			       :min-height min-height)

	(multiple-value-setq (object ptype changed)	
	  (accept type
		  :stream stream :default default
		  :query-identifier query-id :prompt prompt :view view)))
      (values object changed)))
  
(defmethod accept-options1 ((frame gened) stream
			   &key (orientation :horizontal))

  (with-slots (default-line-style default-line-thickness
		  default-ink default-arrow-head) frame     
    
    (formatting-table (stream)
      (flet ((do-body (stream)
	       (multiple-value-bind (real)
		   (accept-it stream 'ink default-ink
			      "Ink" 'ink :orientation orientation
			      :min-width 205)
		 (setf default-ink real)

		 (multiple-value-bind (thickness)
		     (accept-it stream 'line-thickness default-line-thickness
				"Thickness" 'thickness :orientation orientation
				:min-width 323)
		   (setf default-line-thickness thickness)
		   
		   (multiple-value-bind (line-style)
		       (accept-it stream 'line-style-type default-line-style
				  "Style" 'style :orientation orientation
				  :min-width 270)
		     (setf default-line-style line-style))
		   
		   (multiple-value-bind (bool)
		       (accept-it stream 'boolean default-arrow-head
				  "Head " 'head :orientation orientation)
		     (setf default-arrow-head bool))))))
		   		   		     
	(ecase orientation
	  (:horizontal
	   (formatting-row (stream) (do-body stream)))
	  (:vertical
	   (formatting-column (stream) (do-body stream))))))))


(defmethod accept-options2 ((frame gened) stream
			   &key (orientation :horizontal))

  (with-slots (default-filled
		  default-filled-ink
		  default-text-family
	       default-text-face		
	       default-text-size
		) frame    
    
    (formatting-table (stream)

      (flet ((do-body (stream)
	       (multiple-value-bind (real)
		   (accept-it stream 'ink default-filled-ink
			      "Ink" 'ink :orientation orientation)
		 (setf default-filled-ink real)  
		 		 
		   (multiple-value-bind (text-style)
		       (accept-it stream 'p-text-style 
				  (list default-text-family
					default-text-face)
				  "Text-Style" 'text-style :orientation orientation)
		     
		     (setf default-text-family (first text-style))
		     (setf default-text-face (second text-style))
		     
		     (multiple-value-bind (text-size)
			 (accept-it stream 'p-text-size default-text-size
				    "Text-Size" 'text-size :orientation orientation
				    :min-width 300)		     
		       
		       (setf default-text-size text-size)
		       
		       		 (multiple-value-bind (bool)
		     (accept-it stream 'boolean default-filled
				"Filled" 'filled :orientation orientation)
		   (setf default-filled bool))))))) 
	
	(ecase orientation
	  (:horizontal
	   (formatting-row (stream) (do-body stream)))
	  (:vertical
	   (formatting-column (stream) (do-body stream))))))))

;;;
;;; 
;;;

(defun get-symbol-list ()
  (with-application-frame (gened-frame)
    (with-slots (calc-int-dim inc-mode) gened-frame
      (append 
       (when *bounding-boxes*
	 (list 'boxes))
       (when *origins*
	 (list 'origins))
       (when *info*
	 (list 'infos))
       (when *pretty-hl*
	 (list 'pretty))
       (when calc-int-dim
	 (list 'dimensions))
       (when inc-mode
     (list 'incremental))
       (when *classic-warnings*
	 (list 'warnings))))))

					  
(defun text-cell (stream format-string &key (orientation :center))
  (formatting-cell (stream :align-x orientation)
    (write-string format-string stream)
    (write-string "    " stream)))
  

(defmethod accept-buttons ((frame gened) stream
			   &key (orientation :vertical))
    
  (with-slots (calc-int-dim inc-mode) frame
    (let ((last-symbol-list (get-symbol-list)))
      
      (formatting-table (stream :y-spacing '(1 :character))       
	(flet ((do-body (stream)
	       
		 (text-cell stream "Global Display Options:")
			   
		 (multiple-value-bind (symbol-list changed0p)
			       
		     (accept-it stream 
				'(subset-completion (VISIBLE HIDDEN PRIMITIVE COMPOSITE))
				*global-display-options*		 
				nil 'global-display-options :orientation orientation
				:view 'list-pane-view)
		 
		   (setf *global-display-options* symbol-list)
		 
		   (text-cell stream "Object Display Options:")
		 
		   (multiple-value-bind (symbol-list changed1p)
		       (accept-it stream
				  '(subset-completion (INTERIOR BORDER))		
				  (list (if *global-border* 'BORDER)
					(if *global-filled* 'INTERIOR))				
				  nil 'object-display-options :orientation orientation
				  :view 'list-pane-view)
		   
		     (setf *global-filled* (and (member 'INTERIOR  symbol-list) t)
			   *global-border* (and (member 'BORDER symbol-list) t))
		   
		   
		     (text-cell stream "Object Handle Options:")
		   
		     (multiple-value-bind (symbol-list changed2p)
			 (accept-it stream 
				    `(subset-completion (NORMAL FIXED UNFIXED START END))	
				    *handles-symbol-list*
				    nil 'handles :orientation orientation
				    :view 'list-pane-view)
		     
		       (setf *handles-symbol-list* symbol-list)
		     
		       (text-cell stream "Global Display Scaling:")
		     
		       (multiple-value-bind (number changed3p)
			   (accept-it stream 
				      `(completion (1/2 1 2))
				      *global-scaling*
				      nil 'gs :view 'list-pane-view
				      :orientation orientation)
		       
			 (setf *global-scaling* number)
		       
			 (text-cell stream "Concept Label Options:")
		       
			 (multiple-value-bind (symbol-list changed4p)
			     (accept-it stream 
					'(subset-completion (IDs PARENTS ANCESTORS))
					*concept-labels*
					nil
					'labels :view 'list-pane-view
					:orientation orientation)
			   (setf *concept-labels* symbol-list)
			 
			   (text-cell stream "Others:")			 
			   
			   (multiple-value-bind (symbol-list changed5p)
			       
			       (accept-it stream 
					  '(subset-completion (Boxes Origins 
							       Pretty
							       Infos Warnings
							       Dimensions
							       Incremental))
					  (get-symbol-list)
					  
					  nil 'others :orientation orientation
					  :view 'list-pane-view)				     
			     
			     (setf *pretty-hl* (member 'pretty symbol-list))
			     (setf *bounding-boxes* (member 'boxes symbol-list))
			     (setf *origins* (member 'origins symbol-list))
			     
			     (setf *info* (member 'infos symbol-list))
			     (setf *classic-warnings* (member 'warnings symbol-list))
			     (set-classic-infos *classic-warnings*)
			     
			     (setf calc-int-dim (member 'dimensions symbol-list))
			     (setf inc-mode (member 'incremental symbol-list))
			   
			     (when (or changed0p changed1p changed2p changed3p changed4p changed5p)	
			       
			       (when changed5p
				 (when
				   (or (and (member 'dimensions symbol-list)
					    (not (member 'dimensions last-symbol-list)))
				       (and (not (member 'dimensions symbol-list))
					    (member 'dimensions last-symbol-list)))
				   (clear-hashtable)
				   (do-incremental-updates))
				 
				 (when  
				     (or (and (member 'incremental symbol-list)
					      (not (member 'incremental last-symbol-list)))
					 (and (not (member 'incremental symbol-list))
					      (member 'incremental last-symbol-list)))				   
				   
				   (when inc-mode				   
				     (clear-knowledge-base)
				     (do-incremental-updates))))
				  
			       (if changed3p
				   (progn
				     (tick-all-objects)
				     (do-incremental-updates))
				 (only-tick-all-objects)))))))))))	
		     
	  (ecase orientation
	    (:horizontal
	     (formatting-row (stream) (do-body stream)))
	    (:vertical
	     (formatting-column (stream) (do-body stream)))))))))


(defmethod accept-elements ((frame gened) stream &key &allow-other-keys)
  (with-slots (mode) frame    
    (formatting-table (stream)
      (formatting-column (stream) 
	(formatting-cell (stream :align-x :center)
	  (let ((element
		 (accept 'elements :stream stream :default mode :prompt "Element")))
	    (setf mode element)))))))
	  

;;;
;;;
;;;

(define-presentation-type line-thickness ()
    :inherit-from `((completion (1 2 3 4))
		    :name-key identity
		    :printer present-line-thickness
		    :highlighter highlight-line-thickness))

(defun present-line-thickness (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 2 26 (- y 2)
		       :filled nil :ink +background-ink+)
      (draw-line* stream 0 (floor y 2) 26 (floor y 2)
		  :line-thickness object))))

(defun highlight-line-thickness (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))

;;;
;;;
;;;

(define-presentation-type line-style-type ()
    :inherit-from `((completion (:solid :pattern1 :pattern2 :pattern3))
		    :name-key identity
		    :printer present-line-style
		    :highlighter highlight-line-style))

(defun decode-line-style (object)
  (second (assoc object
		 +line-pattern-list+)))

(defun encode-line-style (object)
  (first (rassoc (list object)
		 +line-pattern-list+ :test #'equal)))

(defun present-line-style (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 2 22 (- y 2)
		       :filled nil :ink +background-ink+)
      (draw-line* stream 0 (floor y 2) 22 (floor y 2)
		  :line-dashes (decode-line-style object)))))
			

(defun highlight-line-style (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))

;;;
;;;
;;;

(define-presentation-type ink ()
  :inherit-from '((completion (0.4 0.6 0.8 0.9))
		  :name-key identity
		  :printer present-ink)) 

(defun present-ink (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 2 16 (- y 2)
		       :filled t :ink 
		       (make-gray-color object)))))
      
;;;
;;;
;;;
	  
(define-presentation-type p-text-style () 
  :inherit-from `((completion ((:sans-serif :bold) 
			       (:serif :italic)
			       (:sans-serif nil)
			       (:fix :bold))
			      :test equalp)
		  :name-key identity
		  :printer present-text-style))

(defun present-text-style (object stream &key)
  (let ((text-style
	 (parse-text-style (append object '(:small)))))
    (with-text-style (stream text-style)
      (draw-text* 
       stream 
       "ABC"
       0 0 
       :ink +black+))))

(define-presentation-type p-text-size ()
  :inherit-from '((completion 
		   (:tiny :very-small :normal :very-large
			  :huge))
		  :name-key identity
		  :printer present-text-size))

(defun present-text-size (object stream &key)
  (case object
    (:tiny       (write-string "T" stream))
    (:very-small (write-string "S" stream))
    (:normal     (write-string "N" stream))
    (:very-large (write-string "L" stream))
    (:huge       (write-string "H" stream))))  
		  
;;;
;;;
;;;

(define-presentation-type elements ()
  :inherit-from 
  `((completion (g-concept g-circle g-rectangle g-arrow
			   g-chain g-spline-chain g-polygon
			   g-spline-polygon g-text g-point))
		  :name-key identity
		  :printer present-elements
		  :highlighter highlight-element))

(defun present-elements (object stream &key)
  (let ((*concept-labels* nil)
	(*handles-symbol-list* nil)
	(*bounding-boxes* nil)
	(*origins* nil))
    (draw (second (assoc object
			 *sample-graphic-objects*))
	  stream))) 

(defun highlight-element (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))
    
;;;
;;;
;;;

(defmethod run-frame-top-level :before ((frame gened) &key)
  (initialize-gened frame))

(defmethod frame-standard-input ((frame gened))
  (get-frame-pane frame 'command))

(defmethod frame-standard-output ((frame gened))
  (get-frame-pane frame 'infos))

(defmethod initialize-gened ((frame gened))
  (load-concepts)  
  (load-label-object)
  
  (setf *current-file* "NONAME")
    
  (setf *global-scaling* 1)
  
  (setf *cl-error-msg-stream* 
    (get-frame-pane frame
		    'infos))
  (setf *cl-warn-msg-stream* 
    (get-frame-pane frame
		    'infos))
  
  (cl-set-classic-error-mode nil)
  
  (setf *classic-print-right-margin* 40))

(defun set-classic-infos (bool) 
  (cl-set-classic-warn-mode bool)) 

(defmethod frame-error-output ((frame gened))
  (get-frame-pane frame 'infos))

(defun clear-info-pane ()
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'infos)))
      (window-clear stream))))   

;;;
;;;
;;;

(defmethod draw-label ((frame gened) stream &key max-width max-height)  
  (declare (ignore max-width max-height))
  (updating-output (stream :unique-id *current-file*
			   :cache-value *current-file*)   
    (multiple-value-bind (x y)
	(window-inside-size stream)
      (with-translation (stream (+ 4 (/ x 2)) (+ 12 (/ y 2)))
	(let ((*origins* nil)
	      (*concept-labels* nil)
	      (*bounding-boxes* nil)
	      (*pretty-hl* nil)
	      (*global-border* t)
	      (*global-filled* t)
	      (*global-scaling* 1)
	      (name 
	       (let ((num (position #\/ *current-file* :from-end t)))
		 (subseq *current-file*
			 (if num (1+ num) 0)
			 (length *current-file*)))))
	  (draw *label-shadow-object* stream)
	  (draw *label-object* stream)
	  (format stream "File: ~A" name))))))

;;;
;;;
;;;

(define-gesture-name :move   :pointer-button (:left))
(define-gesture-name :create :pointer-button (:left :shift))

(define-gesture-name :delete :pointer-button (:middle :shift))
(define-gesture-name :inspect :pointer-button (:left :meta))
(define-gesture-name :classic-inspect :pointer-button (:right :meta))

(define-gesture-name :copy   :pointer-button (:middle :control))
(define-gesture-name :scale  :pointer-button (:right :shift))
(define-gesture-name :rotate :pointer-button (:left :control))

(define-gesture-name :atomize :pointer-button (:left :control)) 

(define-gesture-name :fix/free :pointer-button (:middle))


;;; -----------------------------
    
(defparameter *gened-text-style*
    (make-text-style :sans-serif
		     :roman
		     :normal
		     ))

(defvar *gened-frame*)

(defun gened (&key (force t)
		    (process t)
		   (left nil)		   
		    top width height
	       &allow-other-keys)
  (let ((port (find-port)))
    (setf (clim:text-style-mapping port 
				   *gened-text-style*)
      "-*-lucida-medium-r-normal-*-12-*-*-*-*-*-*-*")
    (when (or force (null *gened-frame*))
      (unless left
	(multiple-value-bind (screen-width screen-height)
	    (bounding-rectangle-size 
	     (sheet-region (find-graft :port port)))
	  (setf left 0
		top 0
		width screen-width 
		height screen-height)))
      (setf *gened-frame*
	(make-application-frame
	 'gened
	 :left (+ 10 left)
	 :top (+ 10 top)
	 :width (- width 10)
	 :height (- height 10)))
      #+:Allegro
      (if process
	  (mp:process-run-function
	   "Model Browser"
	   #'(lambda ()
	       (run-frame-top-level *gened-frame*)))
	(run-frame-top-level *gened-frame*))
      #-:Allegro
      (run-frame-top-level *gened-frame*)
      *gened-frame*)))


