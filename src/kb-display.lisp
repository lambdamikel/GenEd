;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: KNOWLEDGE; Base: 10 -*-

(cl:in-package knowledge)

(defparameter *browser-text-style*
    (make-text-style :sans-serif :roman :normal))

(defparameter *list-pane-text-style* 
    (make-text-style :serif :roman :small))

(defparameter *relation-text-style* 
    (make-text-style :serif :roman :small))

(defparameter *command-listener-text-style*
    (make-text-style
     :sans-serif :roman :normal))

(define-application-frame browser
    ()
  ((described-concept :initform nil :accessor browser-described-concept)
   (current-concepts :initform nil :initarg :roots :accessor browser-current-concepts)
   (show-super-concepts-p :initform nil
			  :accessor browser-show-super-concepts-p))
  
  (:command-definer t)
  (:command-table (browser))
  (:panes

   (concept-hierarchy-display-pane
    :application
    :incremental-redisplay t
    :display-function 'draw-concept-hierarchy
    :label "Concept Hierarchy"
    :scroll-bars ':both
    :end-of-page-action ':allow
    :end-of-line-action ':allow)
    
   (concept-info-pane
    :application
    :incremental-redisplay t
    #+allegro :excl-recording-p #+allegro t
    :display-function 'draw-concept-description
    :label "Concept Info"
    :text-style *command-listener-text-style*
    :scroll-bars ':both
    :end-of-page-action ':allow
    :end-of-line-action ':allow)

   (listener-pane
    :interactor
    :label nil
    :text-style *command-listener-text-style*
    :scroll-bars ':both
    :min-height '(4 :line)
    :max-height '(4 :line)
    :height '(4 :line))
   
   (pointer-documentation-pane
    (make-clim-stream-pane 
     :type 'pointer-documentation-pane
     :foreground +white+
     :background +black+
     :text-style (make-text-style
		  :sans-serif :bold :small)
     :scroll-bars nil
     :min-height '(1 :line)
     :max-height '(1 :line)
     :height '(1 :line))))
  (:layouts
   (default
       (vertically ()
	 (horizontally ()
	   (2/3 concept-hierarchy-display-pane)
	   (1/3 concept-info-pane))
	 listener-pane
	 pointer-documentation-pane))))


(defmethod frame-standard-output ((frame browser))
  (get-frame-pane frame 'listener-pane))

(defmethod frame-standard-input ((frame browser))
  (get-frame-pane frame 'listener-pane))
(define-border-type :ellipse (stream record left top right bottom)
  (let ((width/2 (floor (- right left) 2))
	(height/2 (floor (- bottom top) 2))) 
    (draw-ellipse* stream
		   (+ left width/2)
		   (+ top height/2)
		   (+ width/2 3)
		   10
		   10
		   (+ height/2 7)
		   :filled nil)))


;;; ----------------------------------------------------------------------

(define-presentation-type concept ())

(define-presentation-method presentation-typep
    ((object t)
     (type concept))
  (classic:cl-named-concept (classic:cl-name object)))


(define-presentation-method present (object
				     (type concept)
				     stream
				     (view textual-view)
				     &key)
  (format stream "~S" (classic:cl-name object)))

(define-presentation-method accept ((type concept)
				    stream
				    (view textual-view)
				    &key)
  (completing-from-suggestions
   (stream)
   (dolist (concept (classic:cl-concept-descendants
		     (classic:cl-named-concept 'classic:thing)))
     (suggest (format nil "~S" (classic:cl-name concept)) concept))))


(defun print-concept-node (concept-node stream)
  (with-output-as-presentation (stream concept-node 'concept :single-box t)
    (surrounding-output-with-border (stream :shape :ellipse)
      (format stream "~S" (classic:cl-name concept-node)))))
			 

(defun draw-concept-graph-arc-superconcept
    (stream from-node to-node x1 y1 x2 y2
     &rest drawing-options
     &key path)
  (declare (ignore from-node to-node drawing-options))
  (if (null path)
      (draw-arrow* stream x2 y2 x1 y1)
    (let ((x3 (first path))
	  (y3 (second path)))
      (draw-arrow* stream x3 y3 x1 y1)
      (draw-polygon* stream (append path (list x2 y2)) :filled nil :closed nil))))

(defun draw-concept-graph-arc-subconcept
    (stream from-node to-node x1 y1 x2 y2
     &rest drawing-options
     &key path)
  (declare (ignore from-node to-node drawing-options))
  (if (null path)
      (draw-arrow* stream x1 y1 x2 y2)
    (let ((x3 (first path))
	  (y3 (second path)))
      (draw-arrow* stream x1 y1 x3 y3)
      (draw-polygon* stream (append path (list x2 y2)) :filled nil :closed nil))))




(defun filter-concept-parents (concept)
  (classic:cl-concept-parents concept))
	       
(defun filter-concept-children (concept)
  (classic:cl-concept-children concept))
	       
	       
(defmethod draw-concept-hierarchy ((frame browser) stream &key)
  (let ((current-concepts (browser-current-concepts frame)))
    (when current-concepts
      (updating-output (stream :unique-id 'concept-hierarchy
			       :cache-test #'equal
			       :cache-value 
			       current-concepts)
	(stream-set-cursor-position stream 10 10)
	(if (browser-show-super-concepts-p frame)
	    (format-graph-from-roots current-concepts
				     'print-concept-node
				     'filter-concept-parents
				     :graph-type :dag
				     :generation-separation 30
				     :arc-drawer 'draw-concept-graph-arc-superconcept
				     :stream stream
				     :orientation ':vertical
				     :merge-duplicates t)
	  (format-graph-from-roots current-concepts
				     'print-concept-node
				     'filter-concept-children
				     :graph-type :dag
				     :generation-separation 30
				     :arc-drawer 'draw-concept-graph-arc-subconcept
				     :stream stream
				     :orientation ':horizontal
				     :merge-duplicates t))))))



(defmethod draw-concept-description ((frame browser) stream &key)
  (let ((concept (browser-described-concept frame)))
    (updating-output (stream :unique-id 'concept-info
			     :cache-value concept)
      (when concept
	(let ((*standard-output* stream))
	  (classic:cl-print-object concept))))))

;;; ----------------------------------------------------------------------


(define-browser-command (com-quit-browser
			       :name t
			       :menu "Quit")
    ()
  (with-application-frame (browser-frame)
    (frame-exit browser-frame)))
			       
(define-browser-command (com-clear-output-history
			       :name t
			       :menu nil)
    ()
  (with-application-frame (browser-frame)
    (window-clear (get-frame-pane browser-frame 'listener-pane))))
			       
(define-browser-command (com-show-concept-descendants
			       :name t
			       :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (browser-frame)
    (window-set-viewport-position 
     (get-frame-pane browser-frame 'concept-hierarchy-display-pane)
     0 0)
    (setf (browser-show-super-concepts-p browser-frame) nil)
    (setf (browser-current-concepts browser-frame)
      (list concept))))

(define-presentation-to-command-translator show-concept-descendants-translator
    (concept
     com-show-concept-descendants 
     browser)
  (object)
  (list object))

(define-browser-command (com-show-concept-ancestors
			       :name t
			       :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (browser-frame)
    (window-set-viewport-position 
     (get-frame-pane browser-frame 'concept-hierarchy-display-pane)
     0 0)
    (setf (browser-show-super-concepts-p browser-frame) t)
    (setf (browser-current-concepts browser-frame)
      (list concept))))

(define-gesture-name :select-super :pointer-button (:left :shift))

(define-presentation-to-command-translator show-concept-ancestors-translator
    (concept
     com-show-concept-ancestors
     browser
     :gesture :select-super)
  (object)
  (list object))

(define-browser-command (com-show-concept-info
			 :name t
			 :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (browser-frame)
    (setf (browser-described-concept browser-frame) concept)))

(define-presentation-to-command-translator show-concept-info-translator
    ((or concept expression)
     com-show-concept-info
     browser
     :gesture :describe
     :tester ((object) (setf *x* object) (classic:cl-concept? object)))
  (object)
  (list object))





;;; ----------------------------------------------------------------------


(defvar *browser-frame*)

(defun browser (&key (force t)
			   (process t)
			   (left nil)
		     top width height
		     (roots (list (cl-named-concept 'classic:classic-thing)))
		      &allow-other-keys)
  (let ((port (find-port)))
    (setf (clim:text-style-mapping port 
				   *browser-text-style*)
      "-*-lucida-medium-r-normal-*-12-*-*-*-*-*-*-*")
    (when (or force (null *browser-frame*))
      (unless left
	(multiple-value-bind (screen-width screen-height)
	    (bounding-rectangle-size 
	     (sheet-region (find-graft :port port)))
	  (setf left 0
		top 0
		width screen-width 
		height screen-height)))
      (setf *browser-frame*
	(make-application-frame
	 'browser
	 :roots roots
	 :left (+ 10 left)
	 :top (+ 10 top)
	 :width (- width 50)
	 :height (- height 50)))
      #+:Allegro
      (if process
	  (mp:process-run-function
	   "Model Browser"
	   #'(lambda ()
	       (run-frame-top-level *browser-frame*)))
	(run-frame-top-level *browser-frame*))
      #-:Allegro
      (run-frame-top-level *browser-frame*)
      *browser-frame*)))




