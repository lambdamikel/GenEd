;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defmethod get-dialog-closure ((object t) stream)
  (declare (ignore stream))
  #'(lambda ()))

(defmethod get-dialog-closure ((object thing) stream)
  #'(lambda ()
      (with-slots (xtrans ytrans hidden)  object
	(formatting-cell (stream :align-x :center)
	  (setf xtrans 
	    (accept 'integer :stream stream :view 'gadget-dialog-view
		    :default xtrans :prompt " X"))
	  (terpri stream)
	  (setf ytrans
	    (accept 'integer :stream stream :view 'gadget-dialog-view
		    :default ytrans :prompt " Y"))
	  (terpri stream))
	(formatting-cell (stream :align-x :center)	
	  (setf hidden
	    (accept 'boolean :stream stream :view 'gadget-dialog-view
		    :default hidden :prompt "Hidden"))
	  (terpri stream))
	
	(funcall (call-next-method)))))

(defmethod get-dialog-closure ((object ink-mixin) stream)
  #'(lambda ()
      (with-slots (ink) object
	(formatting-cell (stream :align-x :center)
	  (setf ink
	    (accept 'ink :stream stream :view 'gadget-dialog-view
		    :default ink :prompt "Ink"))
	  (terpri stream)))
      (funcall (call-next-method))))

(defmethod get-dialog-closure ((object g-text) stream)
  #'(lambda ()
      (with-slots (text-string face size family) object
	(formatting-cell (stream :align-x :center)
	  (setf text-string
	    (accept 'string :stream stream :view 'gadget-dialog-view
		    :default text-string :prompt "Text")))
	
	(terpri stream)
	
	(formatting-cell (stream :align-x :center)
	  (let ((style
		 (accept 'p-text-style :stream stream :view 'gadget-dialog-view
			 :default (list family face)
			 :prompt "Text-Style")))
	    (setf family (first style))
	    (setf face (second style))))
	    
	(terpri stream)
	
	(formatting-cell (stream :align-x :center)
	  (let ((text-size
		 (accept 'p-text-size :stream stream :view 'gadget-dialog-view
			 :default size
			 :prompt "Text-Size")))
	    (setf size text-size))))	

      (funcall (call-next-method))))


(defmethod get-dialog-closure ((object linestyle-mixin) stream)
  #'(lambda ()
      (with-slots (linestyle) object
	(formatting-cell (stream :align-x :center)
	  (let ((object 	       		
		 (accept 'line-style-type :stream stream :view 'gadget-dialog-view
			 :default (encode-line-style linestyle) :prompt "Style ")))
	    (setf linestyle (decode-line-style object)))
	  (terpri stream))
	(funcall (call-next-method)))))

(defmethod get-dialog-closure ((object linethickness-mixin) stream)
  #'(lambda ()
      (with-slots (linethickness) object
	(formatting-cell (stream :align-x :center)
	  (setf linethickness
	    (accept 'line-thickness :stream stream :view 'gadget-dialog-view
		    :default linethickness :prompt "Thickness"))
	  (terpri stream))
	(funcall (call-next-method)))))



(defmethod get-dialog-closure ((object filled-mixin) stream)
  #'(lambda ()
      (with-slots (filledp filled-ink) object	
	(formatting-cell (stream :align-x :center)	  
	  (setf filled-ink
	    (accept 'ink :stream stream :view 'gadget-dialog-view
		    :default filled-ink :prompt "Filled-Ink"))
	  (terpri stream))
   
	(formatting-cell (stream :align-x :center)
	  (setf filledp
	    (accept 'boolean :stream stream :view 'gadget-dialog-view
		    :default filledp :prompt "Filled"))
	  (terpri stream)))
	  
	(funcall (call-next-method))))

(defmethod get-dialog-closure ((object rotateable-thing) stream)
  #'(lambda ()
      (with-slots (rotangle) object
	(formatting-cell (stream :align-x :center)	  
	  (let ((object
		 (accept 'real :stream stream :view 'gadget-dialog-view
			 :default (* 180 (/ rotangle pi)) :prompt "Angle")))
	    (setf rotangle (* pi (/ object 180))))
	  (terpri stream))
	(funcall (call-next-method)))))
  
(defmethod get-dialog-closure ((object scaleable-thing) stream)
  #'(lambda ()
      (with-slots (xscale yscale) object
	(formatting-cell (stream :align-x :center)
	  (setf xscale
	    (accept 'real :stream stream :view 'gadget-dialog-view
		    :default xscale :prompt " X-Scale"))
	  (terpri stream)
	  (setf yscale
	    (accept 'real :stream stream :view 'gadget-dialog-view
		    :default yscale :prompt " Y-Scale"))
	  (terpri stream))
	  
	(funcall (call-next-method)))))

(defmethod get-dialog-closure ((object linesegment) stream)
  #'(lambda ()
      (with-slots (startpoint endpoint) object
	(formatting-cell (stream :align-x :center)
	  (setf startpoint
	    (accept '(sequence-enumerated integer integer)
		    :stream stream :view 'gadget-dialog-view
		    :default startpoint :prompt " Start"))
	  (terpri stream)
	  (setf endpoint
	    (accept '(sequence-enumerated integer integer)
		    :stream stream :view 'gadget-dialog-view
		    :default endpoint :prompt " End"))
	  (terpri stream))
	(funcall (call-next-method)))))

(defmethod get-dialog-closure ((object object-with-head) stream)
  #'(lambda ()
      (with-slots (head) object
	(formatting-cell (stream :align-x :center)
	  (setf head
	    (accept 'boolean
		    :stream stream :view 'gadget-dialog-view
		    :default head :prompt " Arrow-Head"))
	  (adjust-parent-concept object)
	  (terpri stream)))
      (funcall (call-next-method))))

(defmethod get-dialog-closure ((object g-circle) stream)
  #'(lambda ()
      (with-slots (radius) object
	(formatting-cell (stream :align-x :center)
	  (setf radius
	    (accept 'integer :stream stream :view 'gadget-dialog-view
		    :default radius :prompt "Radius"))
	  (terpri stream))
	(funcall (call-next-method)))))
	  

(defmethod get-dialog-closure ((object g-rectangle) stream)
  #'(lambda ()
      (with-slots (xextend yextend) object
	(formatting-cell (stream :align-x :center)
	  (setf xextend
	    (accept 'integer :stream stream :view 'gadget-dialog-view
		    :default xextend :prompt " X-Ext"))
	  (terpri stream)
	  (setf yextend
	    (accept 'integer :stream stream :view 'gadget-dialog-view
		    :default yextend :prompt " Y-Ext"))
	  (terpri stream))
	(funcall (call-next-method)))))

;;;
;;;
;;;

(defmethod inspect-object ((object thing))
  (make-undo-object object 'inspect)
  (with-application-frame (gened-frame)
    (let ((stream (get-frame-pane gened-frame 'display)))      
      (accepting-values (stream :own-window t :label "Inspector")
	(terpri stream)
	(formatting-table (stream :y-spacing '(1 :character)
				  :x-spacing '(2 :character))
	  (formatting-column (stream)
	    (funcall (get-dialog-closure object stream)))))))
  (adjust-object-origin object)
  (tick-object object)
  (do-incremental-updates))

(define-gened-command (com-inspect-object)
    ((object 'thing :gesture :inspect))
  (inspect-object object))

(define-gened-command (com-gened-inspect-object :name "Inspect Object")
    ()
  (if (any-visible-objects)
      (let ((source-object (accept 'thing)))
	(terpri)
	(inspect-object source-object))))
