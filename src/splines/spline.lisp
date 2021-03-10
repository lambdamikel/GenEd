

(defclass spline-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun make-spline-point (x y)
  (make-instance 'spline-point :x x :y y))

(defun make-spline (points n)
  (let* ((m (1- (first (array-dimensions points))))
	 (first 0)
	 (newpoints))
    
    (dotimes (a (- m 3))
      (let* ((i (+ a 2))
	     (pa (aref points (- i 1)))
	     (pb (aref points i))
	     (pc (aref points (+ i 1)))
	     (pd (aref points (+ i 2)))
                   
	     (xa (x pa))
	     (ya (y pa))
	     (xb (x pb))
	     (yb (y pb))
	     (xc (x pc))
	     (yc (y pc))
	     (xd (x pd))
	     (yd (y pd))
                   
	     (a3 (/ (+ (- xa) (* 3.0 (- xb xc)) xd) 6.0))
	     (a2 (/ (+ xa (* (- 2.0) xb) xc) 2.0))
	     (a1 (/ (- xc xa) 2.0))
	     (a0 (/ (+ xa (* 4.0 xb) xc) 6.0))
	     (b3 (/ (+ (- ya) (* 3.0 (- yb yc)) yd) 6.0))
	     (b2 (/ (+ ya (* (- 2.0) yb) yc) 2.0))
	     (b1 (/ (- yc ya) 2.0))
	     (b0 (/ (+ ya (* 4.0 yb) yc) 6.0))
                   
	     (j first))
              
	(loop
	  (let* ((q (/ j n))
		 (x0 (round (+ (* (+ (* (+ (* a3 q) a2) q) a1) q) a0)))
		 (y0 (round (+ (* (+ (* (+ (* b3 q) b2) q) b1) q) b0))))
	    (incf j)
	    (push (list x0 y0) newpoints)
	    (if (= j n) (return)))))
      (setf first 0))
    (values (make-point-array newpoints) newpoints)))

(defun make-point-array (liste)
  (let ((index 0)
	(arr (make-array (length liste))))
    (mapc
     #'(lambda (elem)
	 (setf (aref arr index)
	   (make-spline-point (first elem) (second elem)))
	 (incf index))
     liste)
    arr))

          
(defun normalize-list (liste)
  (let ((first (first liste))
	(last (last liste)))
    (append 
     (list* first first first liste)
     last last last)))
  
(defun remove-dublicates (liste &optional (akku ()))
    (cond ((null liste) akku)
             (t (remove-dublicates
                    (cdr liste)
                    (if (member (first liste) akku :test #'equalp) 
                       akku
                       (cons (first liste) akku))))))




#| Bezier-Chain streichen, SPLINE nehmen!
    Struktur: Pointlist, HANDLES wie bisher, 
   jedoch f. "spatial reasoning" SPLINE-Punkte nehmen
   mit sehr hoher Granularität! (10)
  Draw-Routine: DRAW-Polygon, nimm SPLINE-Punkte.
   Neues Objekt: CREATE w. Polygon, aber
   Spline-Polygon nehmen (zuerst niedrige Granul., damit 
  schnell), dann abspeichern mit hoher Granularität!

  Klasen einführen : SPLINE-Polygon!

|#

