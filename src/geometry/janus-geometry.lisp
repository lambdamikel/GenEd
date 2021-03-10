;;;-*- Mode: Lisp; Package: PJ -*-

(in-package :pj)

;;;----------------------------------------------------------------------------

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun make-point (h v)
    (declare (type fixnum h v))
    (if (and (<= (abs h) #x7fff) (<= (abs v) #x7fff))
	(the fixnum (logior (ash v 16) (logand #xffff h)))
      (error "make-point called with wrong arguments: h=~D, v=~D" h v)))
  
  (defun point-h (point)
    (if (logbitp 15 point)
	(the fixnum (- (logand #x7fff point) #x8000))
	(the fixnum (logand #x7fff point))))

  (defun point-v (point)
    (ash point -16))
  
  (defun point-string (point)
    (format nil "#@(~D ~D)" (point-h point) (point-v point)))
  
  (defun subtract-points (point1 point2)
    (make-point (- (point-h point1) (point-h point2))
		(- (point-v point1) (point-v point2))))
  
  (defun add-points (point1 point2)
    (make-point (+ (point-h point1) (point-h point2))
		(+ (point-v point1) (point-v point2))))

  (defun |#@-reader| (stream char arg)
    (declare (ignore char arg))
    (let ((coords (read stream)))
      (if (and (consp coords) (= (length coords) 2))
	  (make-point (first coords) (second coords))
	(error "#@-macro used with wrong parameters: ~S" coords))))

  (set-dispatch-macro-character #\# #\@ #'|#@-reader|)
  )

;;;----------------------------------------------------------------------------

(defclass geometry-feature ()
  ())

(defgeneric feature= (feature1 feature2))

;;;----------------------------------------------------------------------------

(defclass area-feature (geometry-feature)
  ((position :accessor area-position :initarg :position)
   (size :accessor area-size :initarg :size)))

(defmethod print-object ((area area-feature) stream)
  (flet ((safe-p (x)
           (if (numberp x)
             (format nil "(~D,~D)" (point-h x) (point-v x))
             x))
         (safe-e (x)
           (if (numberp x)
             (format nil "~Dx~D" (point-h x) (point-v x))
             x)))
    (print-unreadable-object (area stream :identity t)
      (format stream "Area ~A ~A"
              (safe-p (area-position area)) (safe-e (area-size area))))))

(defun make-area (&key pos size topleft bottomright)
  (cond
   ((and pos size) (make-instance 'area-feature :position pos :size size))
   ((and topleft bottomright)
    (make-instance 'area-feature
      :position topleft :size (subtract-points bottomright topleft)))
   (t (error "Invalid parameter combination"))))

(defmethod feature= ((area1 area-feature) (area2 area-feature))
  (and (= (area-position area1) (area-position area2))
       (= (area-size area1) (area-size area2))))

(defmethod topleft ((area area-feature))
  (area-position area))

(defmethod bottomright ((area area-feature))
  (add-points (area-position area) (area-size area)))

(defmethod empty-area-p ((area area-feature))
  (slet ((size (area-size area)))
    (or (minusp (point-h size)) (minusp (point-v size)))))

(defmethod line-p ((area area-feature))
  )

(defmethod dimension ((object area-feature))
  2)

(defmethod interior ((area area-feature))
  "The interior of an area is the area itself without its bounary."
  (make-area :pos (add-points (area-position area) #@(1 1))
             :size (subtract-points (area-size area) #@(2 2))))

(defmethod boundary ((area area-feature))
  "The boundary of an area is the circular line consisting of all the
accumulation points of the area."
  (slet ((tl (topleft area)) (br (bottomright area))
         (width (make-point (point-h (area-size area)) 0)))
    (make-line (list tl (add-points tl width) br
                     (subtract-points br width))
               :circular t)))

(defmethod bounding-box ((area area-feature))
  (values (topleft area) (bottomright area)))

;;;----------------------------------------------------------------------------

(defclass line-segment ()
  ((start-point :accessor start-point :initarg :start-point :initform nil)
   (end-point :accessor end-point :initarg :end-point :initform nil)))

(defmethod print-object ((segment line-segment) stream)
  (flet ((safe-p (x)
           (if (numberp x)
             (format nil "(~D,~D)" (point-h x) (point-v x))
             x)))
    (print-unreadable-object (segment stream :identity t)
      (format stream "Segment ~A->~A"
              (safe-p (start-point segment)) (safe-p (end-point segment))))))

(defun make-segment (start end)
  (make-instance 'line-segment :start-point start :end-point end))

(defun make-segment-list (point-list circular)
  (mapcon #'(lambda (l)
              (when (second l)
                (list (make-segment (first l) (second l)))))
          (if circular
            (append point-list (list (first point-list)))
            point-list)))

(defmethod bounding-box ((segment line-segment))
  (slet ((sp (start-point segment)) (ep (end-point segment)))
    (values (make-point (min (point-h sp) (point-h ep))
                        (min (point-v sp) (point-v ep)))
            (make-point (max (point-h sp) (point-h ep))
                        (max (point-v sp) (point-v ep))))))

(defmethod intersecting-p ((seg1 line-segment) (seg2 line-segment))
  (and (multiple-value-call #'bb-intersecting-p
                            (bounding-box seg1)
                            (bounding-box seg2))
       (or (true-crossing-p seg1 seg2)
           (point-touching-p seg1 seg2)
           (collinear-overlapping-p seg1 seg2))))

(defun cross-prod (p1 p2)
  ;(print (list (point-string p1) (point-string p2)))
  (- (* (point-h p1) (point-v p2)) (* (point-h p2) (point-v p1))))

(declaim (inline cross-prod))

(defun true-crossing-p (seg1 seg2)
  "Both segments are truly crossing each other."
  (slet ((p1 (start-point seg1)) (p2 (end-point seg1))
         (p3 (start-point seg2)) (p4 (end-point seg2)))
    (and (straddle-p p1 p2 p3 p4) (straddle-p p3 p4 p1 p2))))

(defun collinear-overlapping-p (seg1 seg2)
  "Both segments are collinear and overlapping each other."
  (slet* ((p1 (start-point seg1)) (p2 (end-point seg1))
          (p3 (start-point seg2)) (p4 (end-point seg2))
          (tmp (subtract-points p2 p1)))
    (and (zerop (cross-prod (subtract-points p3 p1) tmp))
         (zerop (cross-prod (subtract-points p4 p1) tmp)))))

(defun point-touching-p (seg1 seg2)
  "At least one start/end point of one segment is touching the other one."
  (slet ((p1 (start-point seg1)) (p2 (end-point seg1))
         (p3 (start-point seg2)) (p4 (end-point seg2)))
    (or (and (straddle-touch-p p1 p2 p3 p4) (straddle-or-touch-p p3 p4 p1 p2))
        (and (straddle-touch-p p1 p2 p4 p3) (straddle-or-touch-p p4 p3 p1 p2))
        (and (straddle-touch-p p4 p3 p1 p2) (straddle-or-touch-p p1 p2 p4 p3))
        (and (straddle-touch-p p4 p3 p2 p1) (straddle-or-touch-p p2 p1 p4 p3)))
    ))

(defun straddle-p (p1 p2 p3 p4)
  "A segment (p1 p2) straddles a line (p3 p4)
if point p1 lies on one side of the line and point p2 lies on the other side."
  (slet ((tmp (subtract-points p2 p1)))
    (minusp (* (signum (cross-prod (subtract-points p3 p1) tmp))
               (signum (cross-prod (subtract-points p4 p1) tmp))))))

(defun almost-zero-p (value)
  (zerop value))
;  (< (abs value) 6))

(declaim (inline almost-zero-p))

(defun straddle-touch-p (p1 p2 p3 p4)
  "A segment (p1 p2) straddles and touches a line (p3 p4)
if additionally one end point of the line lies on the segment."
;  (print (list (point-string p1) (point-string p2)
;               (point-string p3) (point-string p4)))
  (slet ((tmp (subtract-points p2 p1)))
    (and (not (almost-zero-p (cross-prod (subtract-points p3 p1) tmp)))
         (almost-zero-p (cross-prod (subtract-points p4 p1) tmp)))))

(defun straddle-or-touch-p (p1 p2 p3 p4)
  "Test whether s segment (p1 p2) straddles or touches a line (p3 p4)."
  (or (straddle-p p1 p2 p3 p4) (straddle-touch-p p1 p2 p3 p4)))

;;;----------------------------------------------------------------------------

(defun bb-intersecting-p (tl1 br1 tl2 br2)
  "Test whether two bounding boxes are intersecting."
  (and (>= (max (point-h tl1) (point-h br1))
           (min (point-h tl2) (point-h br2)))
       (>= (max (point-h tl2) (point-h br2))
           (min (point-h tl1) (point-h br1)))
       (>= (max (point-v tl1) (point-v br1))
           (min (point-v tl2) (point-v br2)))
       (>= (max (point-v tl2) (point-v br2))
           (min (point-v tl1) (point-v br1)))))

(defun bb-distance (tl1 br1 tl2 br2)
  "Compute distance between two disjoint bounding boxes"
  (list (bb-intersecting-p tl1 br1 tl2 br2)
	(- (max (point-h tl1) (point-h br1))
           (min (point-h tl2) (point-h br2)))
	(- (max (point-h tl2) (point-h br2))
           (min (point-h tl1) (point-h br1)))
	(- (max (point-v tl1) (point-v br1))
           (min (point-v tl2) (point-v br2)))
	(- (max (point-v tl2) (point-v br2))
           (min (point-v tl1) (point-v br1)))))

(defun bb-in-bb-p (tl1 br1 tl2 br2)
  "Test whether bounding box (tl1 br1) is inside of bounding box (tl2 br2)."
  (and (point-in-bb-p tl1 tl2 br2) (point-in-bb-p br1 tl2 br2)))

(defun point-in-bb-p (point topleft bottomright)
  "Test whether point lies within this bounding box."
  (and (<= (point-h topleft) (point-h point) (point-h bottomright))
       (<= (point-v topleft) (point-v point) (point-v bottomright))))

(defun point-on-boundary-p (point topleft bottomright)
  "Test whether point lies on the boundary of this bounding box."
  ;;;(print (mapcar #'point-string (list point topleft bottomright)))
  (and (point-in-bb-p point topleft bottomright)
       (or (= (point-h topleft) (point-h point))
           (= (point-v topleft) (point-v point))
           (= (point-h bottomright) (point-h point))
           (= (point-v bottomright) (point-v point)))))

;;;----------------------------------------------------------------------------

(defclass line-feature (geometry-feature)
  ((segments :accessor line-segments :initarg :segments)
   (circular :accessor circular-p :initarg :circular :initform nil)))

(defmethod print-object ((line line-feature) stream)
  (flet ((safe-p (x) (if (numberp x)
                       (format nil "(~D,~D)" (point-h x) (point-v x))
                       x)))
    (print-unreadable-object (line stream :identity t)
      (format stream "Line:~D ~A->~A"
              (when (slot-boundp line 'segments) (length (line-segments line)))
              (safe-p (start-point line)) (safe-p (end-point line))))))

(defun make-line (point-list &key circular)
  ;(print (mapcar #'point-string point-list))
  (let ((s-list (make-segment-list point-list circular)))
    (make-instance 'line-feature
      :segments (make-array (list (length s-list))
                            :element-type t
                            :initial-contents s-list)
      :circular circular)))

(defmethod feature= ((line1 line-feature) (line2 line-feature))
  (equalp (line-segments line1) (line-segments line1)))

(defmethod start-point ((line line-feature))
  (start-point (svref (line-segments line) 0)))

(defmethod end-point ((line line-feature))
  (slet ((segments (line-segments line)))
    (end-point (svref segments (1- (length segments))))))

(defmethod (setf line-segments) :after (new-value (line line-feature))
  (declare (ignore new-value))
  (setf (circular-p line) (= (start-point line) (end-point line))))

(defmethod dimension ((line line-feature))
  1)

(defmethod interior ((line line-feature))
  "The interior of a line is equal to itself."
  line)

(defmethod boundary ((line line-feature))
  "The boundary of a line is an empty set in case of a circular line 
otherwise is the set of start and end points."
  (unless (circular-p line)
    (make-point-set (list (start-point line) (end-point line)))))

(defmethod bounding-box ((line line-feature))
  (slet ((segments (line-segments line)))
    (if (= (length segments) 1)
      (bounding-box (svref segments 0))
      (error "Can't compute bounding box for multi-segment line ~S" line))))

;;;----------------------------------------------------------------------------

(defclass point-set ()
  ((elements :accessor elements :initarg :elements :initform nil)))

(defmethod print-object ((set point-set) stream)
  (flet ((safe-p (x)
           (if (numberp x)
             (format nil "(~D,~D)" (point-h x) (point-v x))
             x)))
    (print-unreadable-object (set stream :identity t)
      (format stream "Set:~D ~A"
              (length (elements set)) (mapcar #'safe-p (elements set))))))

(defmethod start-point ((set point-set))
  (first set))

(defmethod end-point ((set point-set))
  (last set))

(defmethod empty ((set point-set))
  (null (elements set)))

(defun make-point-set (elements)
  (make-instance 'point-set :elements elements))

(defmethod dimension ((set point-set))
  "The dimension of an intermediate set is the maximum of its elements."
  (loop for elem in (elements set)
        maximize (dimension elem)))

;;;----------------------------------------------------------------------------

(defmethod feature= ((point1 fixnum) (point2 fixnum))
  (= point1 point2))

(defmethod dimension ((point fixnum))
  0)

(defmethod interior ((point fixnum))
  "The interior of a point is equal to itself."
  point)

(defmethod boundary ((point fixnum))
  "The boundary of a point is always empty."
  nil)

(defmethod intersecting-p ((point fixnum) (seg line-segment))
  (and (multiple-value-call #'point-in-bb-p point (bounding-box seg))
       (collinear-overlapping-p (make-segment point (start-point seg)) seg)))

(defmethod intersecting-p ((seg line-segment) (point fixnum))
  (intersecting-p point seg))

;;;----------------------------------------------------------------------------

(defmethod dim-of-intersect ((feature1 geometry-feature) (feature2 (eql nil)))
  nil)

(defmethod dim-of-intersect ((feature1 (eql nil)) (feature2 geometry-feature))
  nil)

(defmethod dim-of-intersect ((area1 area-feature) (area2 area-feature))
  (slet* ((tl1 (topleft area1)) (br1 (bottomright area1))
          (tl2 (topleft area2)) (br2 (bottomright area2))
          (width (- (min (point-h br1) (point-h br2))
                    (max (point-h tl1) (point-h tl2))))
          (height (- (min (point-v br1) (point-v br2))
                     (max (point-v tl1) (point-v tl2)))))
    (cond ((or (minusp width) (minusp height)) nil) ;intersection is empty
          ((and (zerop width) (zerop height)) 0)    ;point
          ((or (zerop width) (zerop height)) 1)     ;line segment
          (t 2))))

(defmethod dim-of-intersect ((area area-feature) (line line-feature))
  (when (intersecting-p area line)
    (slet ((sp-on (multiple-value-call #'point-on-boundary-p
                                       (start-point line) (bounding-box area)))
           (ep-on (multiple-value-call #'point-on-boundary-p
                                       (end-point line) (bounding-box area))))
      (if (and (or (and sp-on (not ep-on)) (and (not sp-on) ep-on))
               (loop for seg across (line-segments line)
                     thereis (not (multiple-value-call #'bb-in-bb-p
                                                       (bounding-box seg)
                                                       (bounding-box area)))))
        0
        1))))

(defmethod dim-of-intersect ((line line-feature) (area area-feature))
  (dim-of-intersect area line))

(defmethod dim-of-intersect ((line1 line-feature) (line2 line-feature))
  (when (intersecting-p line1 line2)
    (if (loop for seg1 across (line-segments line1)
              thereis (loop for seg2 across (line-segments line2)
                            thereis (collinear-overlapping-p seg1 seg2)))
      1
      0)))

(defmethod dim-of-intersect ((line line-feature) (set point-set))
  (loop for point in (elements set)
        thereis (loop for line-seg across (line-segments line)
                      for seg = (make-segment (start-point line-seg) point)
                      when (and (multiple-value-call #'bb-in-bb-p
                                                     (bounding-box seg)
                                                     (bounding-box line-seg))
                                (collinear-overlapping-p seg line-seg))
                      return 0)))

(defmethod dim-of-intersect ((set point-set) (line line-feature))
  (dim-of-intersect line set))

(defmethod dim-of-intersect ((set point-set) (feature (eql nil)))
  nil)

(defmethod dim-of-intersect ((feature (eql nil)) (set point-set))
  nil)

(defmethod dim-of-intersect ((feature geometry-feature) (point fixnum))
  (when (intersecting-p point feature)
    0))

(defmethod dim-of-intersect ((point fixnum) (feature geometry-feature))
  (dim-of-intersect feature point))

(defmethod intersect ((area1 area-feature) (area2 area-feature))
  (slet ((tl1 (topleft area1)) (br1 (bottomright area1))
         (tl2 (topleft area2)) (br2 (bottomright area2)))
    (make-area :topleft (make-point (max (point-h tl1) (point-h tl2))
                                    (max (point-v tl1) (point-v tl2)))
               :bottomright (make-point (min (point-h br1) (point-h br2))
                                        (min (point-v br1) (point-v br2))))))

(defmethod intersecting-p ((area1 area-feature) (area2 area-feature))
  (multiple-value-call #'bb-intersecting-p
                       (bounding-box area1) (bounding-box area2)))

(defmethod intersecting-p ((area area-feature) (line line-feature))
  (or (multiple-value-call #'point-in-bb-p
                                (start-point line) (bounding-box area))
           (multiple-value-call #'point-in-bb-p
                                (end-point line) (bounding-box area))
           (intersecting-p (boundary area) line)))

(defmethod intersecting-p ((line line-feature) (area area-feature))
  (intersecting-p area line))

(defmethod intersecting-p ((line1 line-feature) (line2 line-feature))
  (loop for seg1 across (line-segments line1)
        thereis (loop for seg2 across (line-segments line2)
                      thereis (intersecting-p seg1 seg2))))

(defmethod intersecting-p ((point fixnum) (area area-feature))
  (multiple-value-call #'point-in-bb-p point (bounding-box area)))

(defmethod intersecting-p ((area area-feature) (point fixnum))
  (intersecting-p point area))

(defmethod intersecting-p ((point fixnum) (line line-feature))
  (loop for seg across (line-segments line)
        thereis (intersecting-p point seg)))

(defmethod intersecting-p ((line line-feature) (point fixnum))
  (intersecting-p point line))

(defmethod intersecting-p ((point1 fixnum) (point2 fixnum))
  (= point1 point2))
