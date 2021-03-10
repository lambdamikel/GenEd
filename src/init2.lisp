;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defparameter *spline-chain* 
    (make-instance 'g-spline-chain 
      :pointlist
      '(-23 -13 -12 6 -8 -10 0 0 8 -4 16 14)
      :initialize-handles t
      :head t
      :parent-concepts '("directed-spline-chain")))

(defparameter *spline-polygon* 
    (make-instance 'g-spline-polygon 
      :pointlist '(8 16 10 12 16 16 22 18 24 12 22 8
		   16 0 8 -8 4 -16 0 -20 -8 -16
		   -10 -8 -12 0 -16 4 -12 12 -4 16)
      :initialize-handles t
      :parent-concepts '("spline-polygon")))

(defparameter *label-object* nil) ; wird spaeter gebunden, Frame muss erst existieren!
(defparameter *label-shadow-object* nil)

(defun load-label-object ()
  (setf *label-object*
    (load-object 
     (make-pathname :host (pathname-host gened::+objects-dir+)
                    :directory (pathname-directory gened::+objects-dir+) 
                    :name "label-object")))
  (setf *label-shadow-object*
    (load-object 
     (make-pathname :host (pathname-host gened::+objects-dir+)
                    :directory (pathname-directory gened::+objects-dir+) 
                    :name "label-shadow-object")))
  (translate-object *label-object* 0 0)
  (translate-object *label-shadow-object* 3 3))

(let ((liste 
       (make-spline 
	(make-spline-chain-list (generate-brackets (pointlist *spline-chain*)))
	3)))
  (setf (spline-points *spline-chain*) 
    (append (mapcan #'identity liste) '(-28 -20))))

(let ((liste
       (make-spline 
	(make-spline-polygon-list 
	 (generate-brackets (pointlist *spline-polygon*))) 
	6)))
  (setf (spline-points *spline-polygon*) 
    (mapcan #'identity liste)))

(defparameter *sample-graphic-objects*
    (list
     (list 'g-circle 
	   (make-instance 'g-circle :radius 20))

     (list 'g-rectangle
	   (make-instance 'g-rectangle :xextend 20 :yextend 16))
     
     (list 'g-arrow
	   (make-instance 'g-arrow :startpoint '(-20 20) :endpoint '(20 -20) :initialize-handles t
			  :head t))
     (list 'g-chain
	   (make-instance 'g-chain :pointlist '(-20 -10 -12 6 -8 -10 0 0 4 -4 12 8 20 20) :initialize-handles t
			  :head t))
     
     (list 'g-spline-chain *spline-chain*)     
     
     (list 'g-spline-polygon *spline-polygon*)
     
     (list 'g-polygon
	   (make-instance 'g-polygon :pointlist '(-16 12 16 20 12 8 16 -12 8 -20 -20 -12 -16 -8)
			  :initialize-handles t))

     (list 'g-text
	   (make-instance 'g-text
	     :text-string "ABC"
	     :size :normal
	     :face :italic
	     :family :sans-serif))
     
     (list 'g-point
	   (make-instance 'g-point
	     :linethickness 3))
     
     (list 'g-concept
	   (make-instance 'g-text
	     :text-string "*!@?"       
	     :size :normal))))

;;;
;;;
;;;

(defun load-concepts ()
  (mapc #'(lambda (x)
	    (init-object-properties (second x)))
	*library*))
     
(defparameter *library* 
  nil)

(defun get-visualisations-of-concept (item)
  (find-it *library*
	   #'(lambda (x) 
	       (eql (first x) item))))

(defun get-graphic-object (item)
  (second item))

(defun get-nth-visualisation-of-concept (item nth)
  (get-graphic-object
   (nth nth
	(get-visualisations-of-concept item))))
	
;;;
;;;
;;;

#|
(cl-add-error-hook #'inconsistent-classic-object)
|#
