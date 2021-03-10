


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
	       
	       
(defmethod draw-concept-hierarchy ((frame gened) stream &key)
  (let ((current-concepts (gened-current-concepts frame)))
    (when current-concepts
      (updating-output (stream :unique-id 'concept-hierarchy
			       :cache-test #'equal
			       :cache-value 
			       current-concepts)
	(stream-set-cursor-position stream 10 10)
	(if (gened-show-super-concepts-p frame)
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



(defmethod draw-concept-description ((frame gened) stream &key)
  (let ((concept (gened-described-concept frame)))
    (updating-output (stream :unique-id 'concept-info
			     :cache-value concept)
      (when concept
	(let ((*standard-output* stream))
	  (classic:cl-print-object concept))))))

;;; ----------------------------------------------------------------------
		       
(define-gened-command (com-clear-output-history
			       :name t
			       :menu nil)
    ()
  (with-application-frame (gened-frame)
    (window-clear (get-frame-pane gened-frame 'listener-pane))))
			       
(define-gened-command (com-show-concept-descendants
			       :name t
			       :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (gened-frame)
    (window-set-viewport-position 
     (get-frame-pane gened-frame 'concept-hierarchy-display-pane)
     0 0)
    (setf (gened-show-super-concepts-p gened-frame) nil)
    (setf (gened-current-concepts gened-frame)
      (list concept))))

(define-presentation-to-command-translator show-concept-descendants-translator
    (concept
     com-show-concept-descendants 
     gened)
  (object)
  (list object))

(define-gened-command (com-show-concept-ancestors
			       :name t
			       :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (gened-frame)
    (window-set-viewport-position 
     (get-frame-pane gened-frame 'concept-hierarchy-display-pane)
     0 0)
    (setf (gened-show-super-concepts-p gened-frame) t)
    (setf (gened-current-concepts gened-frame)
      (list concept))))

(define-gesture-name :select-super :pointer-button (:left :shift))

(define-presentation-to-command-translator show-concept-ancestors-translator
    (concept
     com-show-concept-ancestors
     gened
     :gesture :select-super)
  (object)
  (list object))

(define-gened-command (com-show-concept-info
			 :name t
			 :menu nil)
    ((concept '(or concept expression)))
  (with-application-frame (gened-frame)
    (setf (gened-described-concept gened-frame) concept)))

(define-presentation-to-command-translator show-concept-info-translator
    ((or concept expression)
     com-show-concept-info
     gened
     :gesture :describe
     :tester ((object) (setf *x* object) (classic:cl-concept? object)))
  (object)
  (list object))




