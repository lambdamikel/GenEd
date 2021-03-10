;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: GEOMETRY -*-

(in-package geometry)

(defconstant +big-int+ 10000)

(defparameter *tick-counter* 1)

(defparameter *hash-table* (make-hash-table :size 200 :rehash-size 50))

;;;
;;; Topological Relations for Polygons.
;;;
            
;;;
;;; 
;;;

(defclass geom-thing ()
  ((tickval :initform nil :accessor tickval :initarg :tickval)))

(defclass geom-line (geom-thing)
  ((p1 :initform nil :initarg :p1 :accessor p1)
   (p2 :initform nil :initarg :p2 :accessor p2)))

(defclass geom-point (geom-thing)
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defclass geom-polygon (geom-thing)
  ((point-arr :initform nil :initarg :point-arr :accessor point-arr)
   (line-arr :initform nil :initarg :line-arr :accessor line-arr) 
   (closed   :initform t   :initarg :closed   :accessor closed)))
     

(defmethod get-points ((object geom-polygon))
  (mapcan
   #'(lambda (point)
       (list (x point)
	     (y point)))
   (coerce 
    (point-arr object)
    'list)))

;;;
;;; 
;;;
                  
(defmethod ccw ((p0 geom-point) (p1 geom-point) (p2 geom-point))
  "Quelle: Sedgewick"
  (let* ((x0 (x p0))
	 (x1 (x p1))
	 (x2 (x p2))
	 (y0 (y p0))
	 (y1 (y p1))
	 (y2 (y p2)) 
         
	 (dx1 (- x1 x0))
	 (dy1 (- y1 y0))
	 (dx2 (- x2 x0))
	 (dy2 (- y2 y0)))
       
    (cond ((> (* dx1 dy2) (* dy1 dx2)) 1)
	  ((< (* dx1 dy2) (* dy1 dx2)) -1)
	  ((or (< (* dx1 dx2) 0)
	       (< (* dy1 dy2) 0))
	   -1)
	  ((< (+ (* dx1 dx1) (* dy1 dy1))
	      (+ (* dx2 dx2) (* dy2 dy2)))
	   1)
	  (t 0)))) 
            
;;;
;;;
;;;

(defun make-geom-polygon (point-list 
			  &key (is-closed t)
			       (tickval nil))
  (let* ((poly 
	  (make-instance 'geom-polygon :closed is-closed :tickval tickval)))
    (setf 
	(point-arr poly)
      (make-geom-point-arr point-list)
	
      (line-arr poly)
      (make-geom-line-arr 
       (point-arr poly) :closed is-closed))

    poly))

(defun make-geom-point (x y &key (tickval nil))
  (make-instance 'geom-point 
    :x x
    :y y
    :tickval tickval))

(defun make-geom-point-arr (liste)
  (let ((arr (make-array (+ 2 (length liste))))
	(i 1))
    (dolist (elem liste)
      (setf (aref arr i)
	(make-geom-point (first elem)
			 (second elem)))
      (incf i))
    (setf (aref arr 0)
      (aref arr (1- i)))
    (setf (aref arr i)
      (aref arr 1))
    arr))

(defun make-geom-line (p1 p2 &key (tickval nil))
  (make-instance 'geom-line
    :p1 p1
    :p2 p2
    :tickval tickval))	

(defun make-geom-line-arr (point-arr
			   &key (closed t))
  
  (let* ((dim (if closed 
		  (- (first (array-dimensions point-arr)) 2)
		  (- (first (array-dimensions point-arr)) 3)))
	 (arr (make-array (list dim))))
    (dotimes (i dim)
      (setf (aref arr i)
	(make-geom-line	
	 (aref point-arr (+ i 1))
	 (aref point-arr (+ i 2)))))
    arr))

;;;;
;;;;
;;;;

(defun high-index (arr)
  (1- (first (array-dimensions arr))))

(defun calculate-bounding-box (arr)
  (let* ((minx (x (aref arr 0)))
	 (miny (y (aref arr 0)))
	 (maxx minx)
	 (maxy miny))
    (dotimes (i (1- (first (array-dimensions arr))))
      (let* ((point (aref arr i))
	     (y (y point))
	     (x (x point)))
	(cond ((< x minx) (setf minx x))
	      ((> x maxx) (setf maxx x))
	      ((< y miny) (setf miny y))
	      ((> y maxy) (setf maxy y)))))
    (values minx miny maxx maxy)))

(defun calculate-center (arr)
  (multiple-value-bind (xf yf xt yt)
      (calculate-bounding-box arr)
    (values
     (/ (+ xf xt) 2)
     (/ (+ yf yt) 2))))

;;;
;;; Abstandsfunktionen
;;;

(defmethod parallelp ((thing1 geom-thing)
		      (thing2 geom-thing))
  nil)

(defmethod parallelp ((line1 geom-line) (line2 geom-line))
  (let ((dx1 (- (x (p1 line1)) (x (p2 line1))))
	(dx2 (- (x (p1 line2)) (x (p2 line2))))
	(dy1 (- (y (p1 line1)) (y (p2 line1))))
	(dy2 (- (y (p1 line2)) (y (p2 line2)))))        
    (or (zerop (- (* dx1 dy2) (* dx2 dy1)))
        (zerop (- (* dx2 dy1) (* dx1 dy2))))))

		  
;;;
;;;
;;;

(defmethod distance-between ((point1 geom-point)
			     (point2 geom-point))
  (let ((dx (- (x point1) (x point2)))
	(dy (- (y point1) (y point2))))
    (sqrt (+ (* dx dx) (* dy dy)))))		

(defmethod distance-between ((line geom-line)
			     (point geom-point))
  (distance-between point line))


(defmethod distance-between ((point geom-point)
			     (line geom-line))
    (flet ((betw-0-and-1 (number)
	   (and (not (minusp number))
		(<= number 1.0))))
    

      (let* ((lp1 (p1 line))
	     (lp2 (p2 line))            
	     (lx1 (x lp1))
	     (ly1 (y lp1))            
	     (lx2 (x lp2))            
	     (ly2 (y lp2))            
	     (px (x point))
	     (py (y point))          
	     (ax (- lx2 lx1))
	     (ay (- ly2 ly1))            
	     (dx (- lx1 px))
	     (dy (- ly1 py))            
	     (a2 (+ (* ax ax) (* ay ay)))
	     (scalar
	      (if (zerop a2)
		  +big-int+
		(/ (+ (* ax (- dx))
		      (* ay (- dy)))
		   a2)))                         
	     (x (+ dx
		   (* scalar ax)))
	     (y  (+ dy
		    (* scalar ay))))
	
	(if (betw-0-and-1 scalar)
	    (sqrt (+ (* x x) (* y y)))
	  (min (distance-between point (p1 line))
	       (distance-between point (p2 line)))))))


(defmethod distance-between ((line1 geom-line)
			     (line2 geom-line))
    
  (let* ((d1
	  (distance-between (p1 line1) line2))
	 (d2 
	  (distance-between (p2 line1) line2))
	 (d3 
	  (distance-between (p1 line2) line1))
	 (d4
	  (distance-between (p2 line2) line1)))
    (min d1 d2 d3 d4)))



(defmethod distance-between ((poly1 geom-polygon)
			     (poly2 geom-polygon))
  (let* (
	 (line-arr1 (line-arr poly1))
	 (line-arr2 (line-arr poly2))
	 (ind1 (high-index line-arr1))
	 (ind2 (high-index line-arr2)))
   
    (loop for i from 0 to ind1 minimize
	  (loop for j from 0  to ind2 minimize
		(distance-between
		 (aref line-arr1 i)
		 (aref line-arr2 j))))))


(defmethod distance-between ((line geom-line)
			     (poly geom-polygon))
  (let (
	(line-arr (line-arr poly)))
    
    (loop for i from 0 to (high-index line-arr) minimize
	  (distance-between
	   (aref line-arr i)
	   line))))

(defmethod distance-between ((poly geom-polygon)
			     (line geom-line))
  (distance-between line poly))

(defmethod distance-between ((poly geom-polygon)
			     (point geom-point))
  (let (
	(line-arr (line-arr poly)))
    
    (loop for i from 0 to (high-index line-arr) minimize
	  (distance-between point 
			    (aref line-arr i)))))


(defmethod distance-between ((point geom-point)
			     (poly geom-polygon))
  (distance-between poly point))


;;;
;;;
;;;


(defmethod inside-s ((point1 geom-point) (point2 geom-point))
  nil)

(defmethod inside-s ((point geom-point) (line geom-line))
  nil)

(defmethod inside-s ((line geom-line) (point geom-point))
  nil)

(defmethod inside-s ((line1 geom-line) (line2 geom-line))
  nil)

(defmethod inside-s ((polygon geom-polygon) (point geom-point))
  nil)

(defmethod inside-s ((polygon geom-polygon) (line geom-line))
  nil)


(defmethod inside-s ((point geom-point) (polygon geom-polygon))

  "Nicht ganz korrekt !!! Quelle: Sedgewick, modifiziert!"
  (when (closed polygon) 
    (let* ((count1 0)
	   (count2 0)
	   (j 0)
	   (arr (point-arr polygon))
	   
	   (n (- (first (array-dimensions arr)) 2))
	   
	   (x (x point))
	   (y (y point))
	   
	   (lp (make-geom-line	      
		(make-geom-point 0 0)
		(make-geom-point 0 0)))
	   
	   (lt (make-geom-line 
		(make-geom-point x y)
		(make-geom-point +big-int+ y)))) 
      
      (dotimes (m   n)
	(let ((i (1+ m)))
	  
	  (setf (p1 lp) (aref arr i))
	  
	  (when (and  
		 (not (and (= (y (p1 lp))
			      (y (p1 lt)))
			   (>= (x (p1 lp))
			       (x (p1 lt)))))
		 (not (and (= (y (aref arr j))
			      (y (p1 lt)))
			   (>= (x (aref arr j))
			       (x (p1 lt))))))
	    
	    (setf (p2 lp) (aref arr j))
	    (setf j i)
	    
	    (when (intersects-s lp lt :dim nil)	
	      (incf count1)))))     
      
      (let ((lt (make-geom-line 
		 (make-geom-point x y)
		 (make-geom-point (- +big-int+) y))))
	
        (dotimes (m   n)
	  (let ((i (1+ m)))
	    
            (setf (p1 lp) (aref arr i))
	    
	    (when (and  
		   (not (and (= (y (p1 lp))
				(y (p1 lt)))
			     (<= (x (p1 lp))
				 (x (p1 lt)))))
		   (not (and (= (y (aref arr j))
				(y (p1 lt)))
			     (<= (x (aref arr j))
				 (x (p1 lt))))))
	      
	      (setf (p2 lp) (aref arr j))
	      (setf j i)
	      
	      (when (intersects-s lp lt :dim nil)	
		(incf count2)))))
	
	
	(or (not (zerop (mod count1 2)))
	    (not (zerop (mod count2 2))))))))
 
(defmethod inside-s ((poly1 geom-polygon) (poly2 geom-polygon))
  (when (closed poly2)
    (let* ((arr (point-arr poly1))
	   (ok
	    (dotimes (i (- (first (array-dimensions arr)) 2))
	      (when (not (inside-s (aref arr (1+ i)) poly2))	
		(return t)))))
      (not ok))))


(defmethod inside-s ((line geom-line) (poly geom-polygon))
  (and (not (intersects-s line poly :dim nil))
       (inside-s (p1 line) poly)
       (inside-s (p2 line) poly)))

;;;
;;;
;;;

(defmethod inside ((object1 geom-thing) (object2 geom-thing))
  "Fuer Aussen-Benutzung"
  (and (not (intersects-s object1 object2 :dim nil))
       (inside-s object1 object2)))


;;;
;;;
;;;

(defmethod intersects-s ((point1 geom-point) (point2 geom-point) &key &allow-other-keys)
  (values nil nil))

(defmethod intersects-s ((point geom-point) (line geom-line) &key (dim nil))
  (if
      (or
       (zerop (ccw (p1 line) (p2 line) point))
       (zerop (ccw (p2 line) (p1 line) point)))
      (if dim (values t 0) t)
    (if dim (values nil nil) nil)))
  
(defmethod intersects-s ((line geom-line) (point geom-point) &key (dim nil))
  (intersects-s point line :dim dim))
     
  
(defmethod intersects-s ((l1 geom-line) (l2 geom-line) &key (dim nil)
                                    (top-level-use t))
    "Quelle: Sedgewick"
    (let ((l1p1 (p1 l1))
          (l2p1 (p1 l2))
          (l1p2 (p2 l1))
          (l2p2 (p2 l2)))
       
       (let ((a1 (* (ccw l1p1 l1p2 l2p1)
                      (ccw l1p1 l1p2 l2p2)))
             (a2 (* (ccw l2p1 l2p2 l1p1)
                     (ccw l2p1 l2p2 l1p2))))			
          
          (if (and (<= a1
                         0)
                    (<= a2
                         0))
            (if  dim 
              (if (parallelp l1 l2)
                (let ((l1p1-on-l2 (intersects l1p1 l2 :dim nil))
                      (l1p2-on-l2 (intersects l1p2 l2 :dim nil))
                      (l2p1-on-l1 (intersects l2p1 l1 :dim nil))
                      (l2p2-on-l1 (intersects l2p2 l1 :dim nil)))
                 
                   (if (or (and l1p1-on-l2 l1p2-on-l2)
                           (and l2p1-on-l1 l2p2-on-l1)                           
                           (not (or                           
                                      (and (same-point l1p1 l2p1)
                                             l1p1-on-l2
                                             l2p1-on-l1)                           
                                      (and (same-point l1p1 l2p2)
                                             l1p1-on-l2
                                             l2p2-on-l1)                           
                                      (and (same-point l1p2 l2p1)
                                             l1p2-on-l2
                                             l2p1-on-l1)
                                      (and (same-point l1p2 l2p2)
                                             l1p2-on-l2
                                             l2p2-on-l1))))                           
                     (values t 
                         (if top-level-use 1 'par))                   
                     (values t 0)))
                (values t 0))
              t)
            (if dim
              (values nil nil)
              nil)))))
              

(defun same-point (p1 p2)
  (and (= (x p1) (x p2))
       (= (y p1) (y p2))))

(defun point< (p1 p2)
    (and (< (x p1) (x p2))
           (< (y p1) (y p2))))
	  
(defun point> (p1 p2)
    (and (> (x p1) (x p2))
           (> (y p1) (y p2))))

(defmethod intersects-s ((line geom-line) (poly geom-polygon) &key 
                                    (dim nil)
                                    (recursive-descent t)
                                    (top-level-use t))
  (let* ((larr (line-arr poly))
         (par))
     (if dim
       (let ((max-dim
                   (loop for i from 0 to (1- (first (array-dimensions larr))) 
                      maximize 
                      (multiple-value-bind (intersectsp dim)
                            (intersects-s (aref larr i) line :dim t :top-level-use nil)
                           (if intersectsp 
                             (if (numberp dim)
                               dim
                               (progn
                                  (setf par t)
                                  1))
                             -1)))))    
          (if (minusp max-dim)
            (values nil nil)
            (if (not par) 
              (progn
                 (values t (max max-dim 
                                   (if (and (closed poly) recursive-descent)
                                     (calculate-dim-of-intersect line poly)
                                       -1))))
              (values t
                  (if top-level-use 1 'par)))))
       (loop for i from 0 to (1- (first (array-dimensions larr))) 
          thereis
          (intersects-s (aref larr i) line :dim nil)))))


(defmethod intersects-s ((poly geom-polygon) (line geom-line) &key 
                                    (dim nil)
                                    (recursive-descent t)
                                    (top-level-use t))
  (intersects-s line poly 
     :dim dim
     :recursive-descent recursive-descent
     :top-level-use top-level-use))


(defmethod calculate-dim-of-intersect ((line geom-line) (poly geom-polygon))
  (if (one-part-is-inside line poly)
      1
    0))

(defconstant +epsilon+ 3)

(defmethod one-part-is-inside ((line geom-line) (poly geom-polygon))
  (let* ((p1 (p1 line))
         (p2 (p2 line))
         (d (distance-between p1 p2)))
     (cond ((and (inside-s p1 poly)
                      (inside-s p2 poly)) t)
              ((< d +epsilon+) nil)
              (t 
                 (let* ((middle-point (make-geom-point
                                                 (/ (+ (x p1) (x p2)) 2)
                                                 (/ (+ (y p1) (y p2)) 2)))
                        (l1 (make-geom-line p1 middle-point))
                        (l2 (make-geom-line middle-point p2)))
                    (or (one-part-is-inside l1 poly)
                        (one-part-is-inside l2 poly)))))))


(defmethod intersects-s ((poly1 geom-polygon) (poly2 geom-polygon) &key (dim nil)) 
    (let* ((arr (line-arr poly1))
           (counter 0)
           (col)
           (parlist))
       (if dim 
         (let ((max-dim
                     (loop for i from 0 to (1- (first (array-dimensions arr))) 
                        maximize
                        (multiple-value-bind (intersectsp dim)
                              (intersects-s (aref arr i) poly2 :dim t :top-level-use nil
                                 :recursive-descent t)                                    
                             (cond ( (eq dim 'par)                               
                                       (push i parlist)
                                       1)
                                     ( intersectsp 
                                       (push i col)
                                       (incf counter)
                                       dim)
                                      (t  -1))))))            
            (if (minusp max-dim)
              (values nil nil)
              (values t 
                  (if (and (> counter 0) (closed poly1)				    
                            (loop for line in col
                               thereis
                               (let ((dim (calculate-dim-of-intersect (aref arr line) poly2)))                         
                                  (= dim 1))))
                    2
                    max-dim))))
         
         (loop for i from 0 to (1- (first (array-dimensions arr))) 
            thereis
            (intersects-s (aref arr i) poly2 :dim nil)))))      


(defmethod intersects-s ((point geom-point) (poly geom-polygon) &key (dim nil))
  (let* ((arr (line-arr poly))
	 (ok
	  (dotimes (i (first (array-dimensions arr)))
	    (if (intersects-s point (aref arr i) :dim nil)
		(return t)))))
    (if ok
	(if dim (values t 0) t)
      (if dim (values nil nil) nil))))

(defmethod intersects-s ((poly geom-polygon) (point geom-point) &key (dim nil))
  (intersects-s point poly :dim dim))

;;;
;;;

(defmethod intersects ((thing1 geom-thing) (thing2 geom-thing) &key (dim nil))
  (if dim
      (multiple-value-bind (ip1 dim1)
	  (intersects-s thing1 thing2 :dim t)
	(if (not ip1)
	    (values nil nil)
	  (if (< dim1 2)     
	      (multiple-value-bind (ip2 dim2)
		  (intersects-s thing2 thing1 :dim t)
		(declare (ignore ip2))
		(values t (max dim1 dim2)))
	    (values t 2))))
    (intersects-s thing1 thing2 :dim nil)))

;;;
;;;
;;;

(defmethod touching-tres ((thing1 geom-thing) (thing2 geom-thing) tres)
  "Arbeitet mit dem minimalen Abstand. Fuer Aussen-Benutzung"
  (and 
   (not (intersects-s thing1 thing2 :dim nil))
   (let ((d (distance-between thing1 thing2)))
     (if (<= d tres) d))))

;;;
;;;
;;;

(defmethod covers-tres ((thing1 geom-thing) (thing2 geom-thing) tres)
  "Fuer Aussen-Benutzung"
  (and (inside-s thing2 thing1)
       (touching-tres thing2 thing1 tres)))

;;;
;;;
;;;

(defun distance-and-orientation (x1 y1 x2 y2)
  (let* (
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (d (sqrt (+ (* dx dx) (* dy dy))))
	 (phi (if (zerop dx) 
		  (if (minusp dy)
		      (- (/ pi 2))
		    (/ pi 2))                                          
		(atan (/  dy dx))))
	 (phi2 (if (minusp dx)
		   (+ phi pi)
		 phi)))
    (values d phi2)))
	  
;;;
;;; Master-Function
;;;


(defun inverse (relation)
  (case relation
    (is-inside 'contains)
    (is-covered-by 'covers)
    (contains 'is-inside)
    (covers 'is-covered-by)
    (t relation)))

(defun clear-hashtable ()
  (clrhash *hash-table*))

(defun delete-object-from-cache (thing object-liste)
  (dolist (elem object-liste)
    (unless (eq elem thing)
      (let* ((goedel1 (get-goedel thing elem))
	     (goedel2 (get-goedel elem thing)))
	(remhash goedel1 *hash-table*) 
	(remhash goedel2 *hash-table*)))))


#|
(defun get-goedel (x y)
  (unless (tickval x)
    (setf (tickval x) (incf *tick-counter*)))
  (unless (tickval y)
    (setf (tickval y) (incf *tick-counter*)))
  (let ((r
	 (* (expt 2 (tickval x))
	    (expt 3 (tickval y))))) 
    r))
|#


(defconstant +hash-table-delta+ 3000)

(defun get-goedel (x y)
  (unless (tickval x)
    (setf (tickval x) (incf *tick-counter*)))
  (unless (tickval y)
    (setf (tickval y) (incf *tick-counter*)))
  (+ (* +hash-table-delta+ (tickval x))
     (tickval y)))

;;;
;;;
;;;

(defun relate-poly-to-poly-unary-tres (thing1 thing2 tres &key (dim nil))  
  (or  (gethash (get-goedel thing1 thing2)
		*hash-table*)
       (inverse 
	(gethash (get-goedel thing2 thing1)
		 *hash-table*)) 
       
       (let ((relation 	       
	      (if (intersects thing1 thing2 :dim nil)		   
		  (if dim
		      (multiple-value-bind (intersectsp dim)
			  (intersects thing1 thing2 :dim t)
			(declare (ignore intersectsp))
			(case dim
			  (0 'intersects-0)
			  (1 'intersects-1)
			  (2 'intersects-2)))
		    'intersects)
		
		(if (inside-s thing1 thing2)    
		    
		    (let ((d (distance-between thing1 thing2)))	
		      (if (<= d tres)
			  'is-covered-by
			'is-inside))
		  
		  (if (inside-s thing2 thing1)
		      
		      (let ((d (distance-between thing2 thing1)))
			(if (<= d tres)
			    'covers					
			  'contains))
		    
		    (let ((d (distance-between thing1 thing2)))
		      (if (<= d tres)						   
			  'touches
			'is-disjoint-with)))))))
	 
	 (setf (gethash (get-goedel thing1 thing2)
			*hash-table*)
	   relation)

	 (remhash (get-goedel thing2 thing1) *hash-table*) 
	 	 
	 relation)))





