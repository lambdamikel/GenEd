(require "~/geometry/defpackage.lisp")

(in-package :geometry)

(defconstant +big-int+ 10000)

;;;
;;; Topological Relations for Polygons.
;;;
            
;;;
;;; Konstruktoren
;;;

(defclass geom-line ()
  ((p1 :initform nil :initarg :p1 :accessor p1)
   (p2 :initform nil :initarg :p2 :accessor p2)))

(defclass geom-point ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defclass geom-polygon ()
  ((point-arr :initform nil :initarg :point-arr :accessor point-arr) ; range: 1..n
   (line-arr :initform nil :initarg :line-arr :accessor line-arr) ; range: 0..n/n-1
      
   (ext-point-arr :initform nil :initarg :ext-point-arr :accessor
		  ext-point-arr) ; range : 1..n
   (ext-line-arr :initform nil :initarg :ext-line-arr :accessor
		ext-line-arr)
   
   (closed   :initform t   :initarg :closed   :accessor closed)))
       
;;;
;;; Basis-Funktionen
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
            
(defmethod intersects ((l1 geom-line) (l2 geom-line) &key 
		       &allow-other-keys)
  "Quelle: Sedgewick"
  (let ((l1p1 (p1 l1))
         (l2p1 (p1 l2))
         (l1p2 (p2 l1))
         (l2p2 (p2 l2)))
    (and (<= (* (ccw l1p1 l1p2 l2p1)
                     (ccw l1p1 l1p2 l2p2))
	     0)
	 (<= (* (ccw l2p1 l2p2 l1p1)
		(ccw l2p1 l2p2 l1p2))
	     0))))


;;;
;;; Hilfsfunktionen
;;;

             
(defun draw-lines (l1 l2 col)
  #|
    (set-foreground-color my-window col)
    (draw-line my-window
        (make-position (round (x (p1 l1))) (round (y (p1 l1))))
        (make-position (round (x (p2 l1))) (round (y (p2 l1)))))
    (draw-line my-window
        (make-position (round (x (p1 l2))) (round (y (p1 l2))))
	(make-position (round (x (p2 l2))) (round (y (p2 l2))))))        
	|# )

(defun make-geom-polygon (point-list tres
			  &key (is-closed t)) ; Fehler: wenn &key (closed t) !
  (let* ((poly 
	 (make-instance 'geom-polygon :closed is-closed)))
    (setf 
	(point-arr poly)
        (make-geom-point-arr point-list)
	
	(line-arr poly)
	(make-geom-line-arr 
	 (point-arr poly) :closed is-closed)

	(ext-point-arr poly)
	
	(make-ext-geom-point-arr 
	 (point-arr poly) tres)
	
	(ext-line-arr poly)
	(make-geom-line-arr 
	 (ext-point-arr poly) :closed is-closed))
    poly))

(defun make-geom-point (x y)
  (make-instance 'geom-point 
    :x x
    :y y))

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

(defun make-geom-line (p1 p2)
  (make-instance 'geom-line
      :p1 p1
      :p2 p2)) ; Referenzen auf tats. Punkte, keine Kopien!!!

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


(defun make-ext-geom-point-arr (point-arr tres)
  (let* ((dim (first (array-dimensions point-arr)))
	 (arr (make-array (list dim))))
    (multiple-value-bind (xc yc)
	(calculate-center point-arr)
      (dotimes (i dim)
	(let* ((point (aref point-arr i))
	       (x (x point))
	       (y (y point))
	       (dx (- x xc))
	       (dy (- y yc))
	       (d (sqrt (+ (* dx dx) (* dy dy))))
	       (phi (if (zerop dx) 
			(if (minusp dy)
			    (- (/ pi 2))
			  (/ pi 2))                                          
		      (atan (/  dy dx))))
	       (phi2 (if (minusp dx)
			 (+ phi pi)
		       phi))
	       (new-d (+ d tres))
	       (new-x (round (+ (* new-d (cos phi2)) xc)))
	       (new-y (round (+ (* new-d (sin phi2)) yc))))
	  
	  (setf (aref arr i)	      
	    (make-geom-point new-x new-y)))))
    arr))

;;;
;;; Abstandsfunktionen
;;;

(defmethod distance-between ((point geom-point)
			     (line geom-line))
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
	 (scalar (/ (+ (* ax (- dx))
		       (* ay (- dy)))
		    a2))                         
	 (x (+ dx
	       (* scalar ax)))
	 (y  (+ dy
		(* scalar ay))))
    
    (values 
     (sqrt (+ (* x x) (* y y)))
     scalar
     x y)))


(defmethod distance-between ((line1 geom-line)
			     (line2 geom-line))
  (flet ((betw-0-and-1 (number)
	   (and (not (minusp number))
		(<= number 1.0))))
    
    (multiple-value-bind (d1 s1 x1 y1) 
	(distance-between (p1 line1) line2)
      (multiple-value-bind (d2 s2 x2 y2)
	  (distance-between (p2 line1) line2)
	(multiple-value-bind (d3 s3 x3 y3)
	    (distance-between (p1 line2) line1)
	  (multiple-value-bind (d4 s4 x4 y4)
	      (distance-between (p2 line2) line1)     
	    (let* ((big-int 10000)
		   (min (min 
			 (if (betw-0-and-1 s1) d1 big-int)
			 (if (betw-0-and-1 s2) d2 big-int)
			 (if (betw-0-and-1 s3) d3 big-int)
			 (if (betw-0-and-1 s4) d4 big-int))))                              
	      (cond
	       ((= min d1) (values d1 x1 y1))
	       ((= min d2) (values d2 x2 y2))
	       ((= min d3) (values d3 x3 y3))
	       ((= min d4) (values d4 x4 y4))
	       (t (let*  ((dx1 (- (x (p1 line1)) (x (p1 line2))))
			  (dx2 (- (x (p2 line1)) (x (p1 line2))))
			  (dx3 (- (x (p1 line1)) (x (p2 line2))))
			  (dx4 (- (x (p2 line1)) (x (p2 line2))))
			  (dy1 (- (y (p1 line1)) (y (p1 line2))))
			  (dy2 (- (y (p2 line1)) (y (p1 line2))))
			  (dy3 (- (y (p1 line1)) (y (p2 line2))))
			  (dy4 (- (y (p2 line1)) (y (p2 line2))))
			  
			  (d1 (sqrt (+ (* dx1 dx1) (* dy1 dy1))))
			  (d2 (sqrt (+ (* dx2 dx2) (* dy2 dy2))))
			  (d3 (sqrt (+ (* dx3 dx3) (* dy3 dy3))))
			  (d4 (sqrt (+ (* dx4 dx4) (* dy4 dy4)))))
		    (values (min d1 d2 d3 d4) 0 0)))))))))))



(defmethod distance-between ((poly1 geom-polygon)
			     (poly2 geom-polygon))
  (let ((min +big-int+)
	(line-arr1 (line-arr poly1))
	(line-arr2 (line-arr poly2)))
    (dotimes (i (first (array-dimensions line-arr1)))
      (dotimes (j (first (array-dimensions line-arr2)))
	(let ((d (distance-between
		  (aref line-arr1 i)
		  (aref line-arr2 j))))
	  (if (< d min)
	      (setf min d)))))
    min))


;;;
;;; Relationen!
;;;


(defmethod inside ((point geom-point) (polygon geom-polygon)
		   &key (mode :normal)
                            (draw nil)
                             &allow-other-keys)
  "Nicht ganz korrekt !!! Quelle: Sedgewick, modifiziert!"
  (when (closed polygon) 
    (let* ((count1 0)
	   (count2 0)
	   (j 0)
	   (arr (ecase mode
		  (:normal
		   (point-arr polygon))
		  (:extended 
		   (point-arr polygon)))) 
	   
	   (n (- (first (array-dimensions arr)) 2))
	   
	   (x (x point))
	   (y (y point))
	   
	   (lp (make-geom-line	      
		(make-geom-point 0 0)
		(make-geom-point 0 0)))
	   
	   (lt (make-geom-line 
		(make-geom-point x y)
		(make-geom-point +big-int+ y)))) ; wegen ALIAS-Effekten!
      
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
	    
	    (when (intersects lp lt)	
	      (incf count1)
	      (if draw (draw-lines lp lt blue))))))
      
      (if draw (print count1))
      
      (let ((lt (make-geom-line 
		 (make-geom-point x y)
		 (make-geom-point (- +big-int+) y)))) ; wegen ALIAS-Effekten!
	
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
	      
	      (when (intersects lp lt)	
		(incf count2)
		(if draw (draw-lines lp lt blue))))))
	
	(if draw (print count2))
	
	(or (not (zerop (mod count1 2)))
	    (not (zerop (mod count2 2))))))))
  
#|

(defmethod inside ((point geom-point) (polygon geom-polygon)
		   &key (mode :normal)
                            (draw nil)
		   &allow-other-keys)
  "Nur fuer konvexe Polygone"
    (let* ((arr (ecase mode
                        (:normal
                         (point-arr polygon))
                        (:extended 
                         (point-arr polygon))))
	 
            (n (- (first (array-dimensions arr)) 2))
          	 	
            (ok (dotimes (m n)
                      (if (< (ccw 
                                   (aref arr m)
                                   (aref arr (1+ m))
                                   point) 0)
                         (return t)))))
       (not ok)))
|#


(defmethod inside ((poly1 geom-polygon) (poly2 geom-polygon) &key &allow-other-keys)
  (when (closed poly2)
    (let* ((arr (point-arr poly1))
	   (ok
	    (dotimes (i (- (first (array-dimensions arr)) 2))
	      (when (not (inside (aref arr (1+ i)) poly2))	
		(return t)))))
      (not ok))))


(defmethod intersects ((line geom-line) (poly geom-polygon)
		       &key (poly-mode :normal)
			    &allow-other-keys)
  (let* ((arr (ecase poly-mode
	       (:normal
		(line-arr poly))
	       (:extended
		(ext-line-arr poly))))
	(ok
	 (dotimes (i  (first (array-dimensions arr)))
	   (if (intersects (aref arr i) line)
	       (return t)))))
    ok))
	


(defmethod intersects ((poly1 geom-polygon) (poly2 geom-polygon) 
		       &key (poly-mode1 :normal)
			    (poly-mode2 :normal)
			    &allow-other-keys)
  (let* ((arr (ecase poly-mode1
	       (:normal
		(line-arr poly1))
	       (:extended 
		(ext-line-arr poly1))))
	(ok
	 (dotimes (i (first (array-dimensions arr)))
	   (if (intersects (aref arr i) poly2 :poly-mode poly-mode2)
	       (return t)))))
    ok))
  
  
(defmethod disjoint ((poly1 geom-polygon) (poly2 geom-polygon))
  (and (not
	(inside poly1 poly2))
       (not
	(intersects poly1 poly2))))


(defmethod touching ((poly1 geom-polygon) (poly2 geom-polygon)) 
  "Arbeitet mit dem unsichtbaren extended-Polygon-Rand (s. Harsleev)"
    (and (not (intersects poly1 poly2)) 
           (or (intersects poly1 poly2 :poly-mode1 :extended)
                (intersects poly1 poly2 :poly-mode2 :extended))))


(defmethod touching-tres ((poly1 geom-polygon) (poly2 geom-polygon) tres)

  "Arbeitet mit dem minimalen Abstand"
  (and 
   (not (intersects poly1 poly2)) 
   (let ((d (distance-between poly1 poly2)))
     (if (<= d tres) d))))

(defmethod covers ((poly1 geom-polygon) (poly2 geom-polygon))
    (and (inside poly2 poly1)
	 (touching poly2 poly1)))

(defmethod covers-tres ((poly1 geom-polygon) (poly2 geom-polygon) tres)
  (and (inside poly2 poly1)
       (touching-tres poly2 poly1 tres)))


;;;
;;; Master-Function
;;;

(defun relate-poly-to-poly (poly1 poly2)
#|
  (if (intersects poly1 poly2)
      'intersects
    (if (inside poly1 poly2)    
	(if (intersects poly1 poly2 :poly-mode1 :extended)
	    'covering
	  'inside)
      (if (intersects poly1 poly2 :poly-mode1 :extended)
	  'touching
	  'disjoint)))) |#
)

(defun relate-poly-to-poly-tres (poly1 poly2 tres)
  
  (if (intersects poly1 poly2)
      'intersects
    
    (if (inside poly1 poly2)    
	
	(let ((d (distance-between poly1 poly2)))
	  (if (<= d tres)
	      'is-covered-by
	    'is-inside))
      
      (if (inside poly2 poly1)    
	
	(let ((d (distance-between poly2 poly1)))
	  (if (<= d tres)
	      'covers
	    'contains))
	
	(let ((d (distance-between poly1 poly2)))
	  (if (<= d tres)
	      'touches
	    'is-disjoint-with))))))




