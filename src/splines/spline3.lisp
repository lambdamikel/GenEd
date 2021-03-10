;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPLINES; Base: 10 -*-

(in-package splines)

(defun xp (point) (first point))
(defun yp (point) (second point))

(defun make-spline (liste n &key (polygon nil))
    (let (newpoints)     
       (mapc #'(lambda (pa pb pc pd)
                      (let* ((xa (xp pa))
                             (ya (yp pa))
                             (xb (xp pb))
                             (yb (yp pb))
                             (xc (xp pc))
                             (yc (yp pc))
                             (xd (xp pd))
                             (yd (yp pd))                             
                             (a3 (/ (+ (- xa) (* 3.0 (- xb xc)) xd) 6.0))
                             (a2 (/ (+ xa (* (- 2.0) xb) xc) 2.0))
                             (a1 (/ (- xc xa) 2.0))
                             (a0 (/ (+ xa (* 4.0 xb) xc) 6.0))
                             (b3 (/ (+ (- ya) (* 3.0 (- yb yc)) yd) 6.0))
                             (b2 (/ (+ ya (* (- 2.0) yb) yc) 2.0))
                             (b1 (/ (- yc ya) 2.0))
                             (b0 (/ (+ ya (* 4.0 yb) yc) 6.0)))		                              
                         (dotimes (j (1+ n))                
                             (let* ((q (/ j n))
                                    (x0  (+ (* (+ (* (+ (* a3 q) a2) q) a1) q) a0))
                                    (y0  (+ (* (+ (* (+ (* b3 q) b2) q) b1) q) b0)))                                                             
                                (push (list x0 y0) newpoints)))))		            
            liste (cdr liste) (cddr liste) (cdddr liste))
       (let ((x (remove-duplicates newpoints :test #'equalp :from-end t)))
          (if polygon
            (butlast x)
            x))))
          
(defun make-spline-chain-list (liste)
  (let ((first (first liste))
        (last (last liste)))
    (append 
     (list* first first first liste)
     last last last)))

(defun make-spline-polygon-list (liste)
  (let* ((revlist (reverse liste))
         (last1 (first revlist))
         (last2 (second revlist))
         (last3 (third revlist)))
     (append 
         (list last3 last2 last1)
          liste)))

(defun filter-to-close-together-out (liste dist)
    (let ((last-point (first liste))
          (first-point (last liste))
          (liste (rest (butlast liste))))
              
       (append (list last-point)
           (mapcan #'(lambda (point)
                              (let* ((xn (xp point))
                                     (yn (yp point))
			       (xa (xp last-point))
                                     (ya (yp last-point))
                                     (dx (- xn xa))
                                     (dy (- yn ya)))
                                 (setf last-point point)
                                 (if (< (sqrt (+ (* dx dx) (* dy dy))) dist)
                                   nil
                                   (list  point))))
                liste)
           first-point)))

	       
