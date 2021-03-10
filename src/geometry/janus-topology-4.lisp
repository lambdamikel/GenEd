;;;-*- Mode: Lisp; Package: PJ -*-

(in-package :pj)

(defparameter *touching-threshold* 4)

;;;----------------------------------------------------------------------------
;;; Interface to subclasses: mixins

(defclass topology-mixin ()
  ()
  (:documentation "Provides access to topological, spatial and geometrical~
                   features between geometrical objects with bounding boxes~
                   describing their spatial extent."))

(defclass rect-topology-mixin (topology-mixin)
  ()
  (:documentation "Provides access to topological, spatial and geometrical~
                   features for rectangular objects."))

(defclass line-topology-mixin (rect-topology-mixin)
  ()
  (:documentation "Provides access to topological, spatial and geometrical~
                   features for line-type objects."))

(defclass point-topology-mixin (line-topology-mixin)
  ()
  (:documentation "Provides access to topological, spatial and geometrical~
                   features for point-type objects."))

;;;----------------------------------------------------------------------------
;;; Interface which has to be provided by subclasses

(defgeneric get-topleft (object)
  (:documentation "Return topleft position of minimal rectangle which encloses
this object"))

(defgeneric get-bottomright (object &optional non-empty)
  (:documentation "Return bottomright position of minimal rectangle which encloses
this object"))

(defgeneric get-start-point (line-object)
  (:documentation "Return start point of line segment"))

(defgeneric get-end-point (line-object)
  (:documentation "Return extended end point of line segment"))

(defgeneric get-extended-start-point (line-object)
  (:documentation "Return extended start point of line segment"))

(defgeneric get-extended-end-point (line-object)
  (:documentation "Return end point of line segment"))

(defgeneric get-point (point-object)
  (:documentation "Return point of corresponding line segment"))

(defgeneric get-extended-point (point-object)
  (:documentation "Return extended point of corresponding line segment"))

;;;----------------------------------------------------------------------------
;;; Interface to subclasses: primitive topological relations

(defgeneric disjoint-p (object1 object2)
  (:documentation "Two objects are disjoint if the intersection of their
*extended* regions (1 pixel to right and bottom) is empty. disjoint is symmetric
and applies to every situation."))

(defgeneric touching-p (object1 object2)
  (:documentation "Two objects are touching if only the boundaries of their
*extended* regions (1 pixel to right and bottom) are intersecting. touching is
symmetric and applies to every situation but point/point."))

(defgeneric overlapping-p (object1 object2)
  (:documentation "Two objects are overlapping if the intersection of their
*original* regions is either a line or a point which is different to both
objects. overlapping is symmetric and applies only to area/area and
line/line situations."))

(defgeneric crossing-p (object1 object2)
  (:documentation "Two lines are crossing if their intersection is an internal
point. A line crosses an area if the line is partly inside and outside of
the area of the area's *extended* region (1 pixel to right and bottom).
overlapping is symmetric and applies only to line/line and line/area
situations."))

(defgeneric equal-p (object1 object2)
  (:documentation "Objects are equal if their regions have the same
position end extent. equal is symmetric and applies to every situation."))

(defgeneric containing-p (object1 object2)
  (:documentation "An object A contains an object B if the intersection between
A's and B's extended regions (1 pixel to right and bottom) is equal to B and
and the interiors of their extended regions intersect. containing's inverse is
 inside. They apply to every situation."))

(defgeneric inside-p (object1 object2)
  (:documentation "inside is inverse of containing."))

(defgeneric query-relation (object relation candidate-list)
  (:documentation "Return every object in 'candidate-list' which is in 'relation'
with 'object'"))

(defgeneric spatial-orientation (object1 object2)
  (:documentation "Computes relative orientation between both objects.
Recognized orientations: above, below, left-of, right-of.
Second value is offset for touching."))

;;;----------------------------------------------------------------------------
;;; Interface to subclasses: composed topological relations

(defgeneric covering-p (object1 object2)
  (:documentation "An object A is covering an object B if A's region completely
contains B's region and B is touching A. covering applies only to area/area
and area/line situations."))

(defgeneric covered-by-p (object1 object2)
  (:documentation "covered-by is inverse of covering."))

;;;----------------------------------------------------------------------------
;;; Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant bb-size-correction #@(1 1)
	       "Areas are sometimes reduced by size #@(1 1)"))

(defmacro reduced (bottomright &optional (correction bb-size-correction))
  `(subtract-points ,bottomright ,correction))

(defmacro extended (bottomright &optional (correction bb-size-correction))
  `(add-points ,bottomright ,correction))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-features-decl (decl-list reduced-p)
    (mapcar #'(lambda (decl)
                `(,(first decl) (make-feature ,(second decl) ,reduced-p)))
            decl-list)))

(defmacro with-orig-features (decl-list &body body)
  `(let ,(gen-features-decl decl-list nil)
     ,@body))

(defmacro with-reduced-features (decl-list &body body)
  `(let ,(gen-features-decl decl-list t)
     ,@body))

(defvar *pj-object-features* (make-hash-table :test #'eq))

(defmethod make-feature :around ((object topology-mixin) reduced-p)
  (let ((features (or (gethash object *pj-object-features*) (list nil))))
    (prog1
      (if reduced-p
        (or (cdr features)
            (setf (cdr features) (call-next-method)))
        (or (car features)
            (setf (car features) (call-next-method))))
      (setf (gethash object *pj-object-features*) features))))

(defmethod make-feature ((object rect-topology-mixin) reduced-p)
  (slet ((br (get-bottomright object)))
    (make-area :topleft (get-topleft object)
               :bottomright (if reduced-p (reduced br) br))))

(defmethod make-feature ((object line-topology-mixin) reduced-p)
  (make-line (list (if reduced-p
                     (get-start-point object)
                     (get-extended-start-point object))
                   (if reduced-p
                     (get-end-point object)
                     (get-extended-end-point object)))))

(defmethod make-feature ((object point-topology-mixin) reduced-p)
  (if reduced-p
    (get-point object)
    (get-extended-point object)))

(defmethod make-feature ((object point-topology-mixin) reduced-p)
  (if reduced-p
    (get-point object)
    (make-area :pos (subtract-points (get-point object) #@(1 1))
               :size #@(2 2))))

;;;----------------------------------------------------------------------------
;;; Topological predicates for rectangles

(defmethod dim-of-intersect ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (with-reduced-features ((area1 o1) (area2 o2))
    (dim-of-intersect area1 area2)))

(defmethod disjoint-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (with-orig-features ((area1 o1) (area2 o2))
    (not (intersecting-p area1 area2))))

(defmethod touching-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (and (with-orig-features ((area1 o1) (area2 o2))
         (intersecting-p area1 area2))
       (with-reduced-features ((area1 o1) (area2 o2))
         (not (intersecting-p area1 area2)))))

(defmethod almost-touching-p ((o1 rect-topology-mixin)
                              (o2 rect-topology-mixin))
  (with-orig-features ((area1 o1) (area2 o2))
    (let ((dist (feature-distance area1 area2)))(print (point-string dist))
      (< (max (abs (point-h dist)) (abs (point-v dist)))
         *touching-threshold*))))

(defmethod overlapping-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (and (with-reduced-features ((area1 o1) (area2 o2))
         (intersecting-p area1 area2))
       (not (or (containing-p o1 o2) (containing-p o2 o1)))))

(defmethod crossing-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod equal-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (with-orig-features ((area1 o1) (area2 o2))
    (feature= area1 area2)))

(defmethod containing-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (with-reduced-features ((feature1 o1) (feature2 o2))
    (and (multiple-value-call #'bb-in-bb-p
                              (bounding-box feature2)
                              (bounding-box feature1))
         (not (equal-p o1 o2)))))

(defmethod inside-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (containing-p o2 o1))

;;;----------------------------------------------------------------------------
;;; Topological predicates for rectangles/lines and lines/lines

(defmethod overlapping-p ((o1 rect-topology-mixin) (o2 line-topology-mixin))
  nil)

(defmethod overlapping-p ((o1 line-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod crossing-p ((o1 rect-topology-mixin) (o2 line-topology-mixin))
  (with-orig-features ((area o1) (line o2))
      (when (intersecting-p area line)
        (slet ((sp-in
                (multiple-value-call #'point-in-bb-p
                                     (start-point line) (bounding-box area)))
               (ep-in
                (multiple-value-call #'point-in-bb-p
                                     (end-point line) (bounding-box area))))
          (not (and sp-in ep-in))))))

(defmethod crossing-p ((o1 rect-topology-mixin) (o2 line-topology-mixin))
  (with-orig-features ((area o1) (line o2))
    (and (intersecting-p area line)
         (with-reduced-features ((rarea o1) (rline o2))
           (intersecting-p rarea rline))
         (not (containing-p o1 o2))
         (= (dim-of-intersect area line)
            (1- (max (dimension area) (dimension line)))))))

(defmethod crossing-p ((o1 line-topology-mixin) (o2 rect-topology-mixin))
  (crossing-p o2 o1))

(defmethod crossing-p ((o1 line-topology-mixin) (o2 line-topology-mixin))
  (with-reduced-features ((feature1 o1) (feature2 o2))
    (eq (dim-of-intersect feature1 feature2) 0)))
  
(defmethod equal-p ((o1 rect-topology-mixin) (o2 line-topology-mixin))
  nil)

(defmethod equal-p ((o1 line-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod equal-p ((o1 line-topology-mixin) (o2 line-topology-mixin))
  (and (= (get-topleft o1) (get-topleft o2))
       (= (get-bottomright o1) (get-bottomright o2))
       (with-orig-features ((feature1 o1) (feature2 o2))
         (feature= feature1 feature2))))

(defmethod containing-p ((o1 line-topology-mixin) (o2 line-topology-mixin))
  (and (call-next-method)
       (with-orig-features ((feature1 o1) (feature2 o2))
         (= 1 (dim-of-intersect feature1 feature2)))))

(defmethod almost-touching-p ((o1 rect-topology-mixin)
                              (o2 line-topology-mixin))
  (with-reduced-features ((area1 o1) (area2 o2))
    (let ((dist (feature-distance area1 area2)))(print (point-string dist))
      (< (max (abs (point-h dist)) (abs (point-v dist)))
         *touching-threshold*))))

(defmethod almost-touching-p ((o1 line-topology-mixin)
                              (o2 rect-topology-mixin))
  (almost-touching-p o2 o1))

(defmethod almost-touching-p ((o1 line-topology-mixin)
                              (o2 line-topology-mixin))
  (with-orig-features ((feature1 o1) (feature2 o2))
    (feature-distance feature1 feature2)))

;;;----------------------------------------------------------------------------
;;; Topological predicates for point/other

(defmethod dim-of-intersect ((o1 point-topology-mixin)
                             (o2 point-topology-mixin))
  (with-reduced-features ((point1 o1) (point2 o2))
    (when (= point1 point2)
      0)))

(defmethod touching-p ((o1 point-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod overlapping-p ((o1 point-topology-mixin) (o2 line-topology-mixin))
  nil)

(defmethod overlapping-p ((o1 line-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod overlapping-p ((o1 point-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod crossing-p ((o1 point-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod crossing-p ((o1 rect-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod crossing-p ((o1 line-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod crossing-p ((o1 point-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod containing-p ((o1 point-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod containing-p ((o1 line-topology-mixin) (o2 point-topology-mixin))
  (with-reduced-features ((feature1 o1) (feature2 o2))
    (intersecting-p feature1 feature2)))

(defmethod containing-p ((o1 rect-topology-mixin) (o2 point-topology-mixin))
  (with-reduced-features ((feature1 o1) (feature2 o2))
    (intersecting-p feature1 feature2)))

(defmethod inside-p ((o1 rect-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod inside-p ((o1 line-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod inside-p ((o1 point-topology-mixin) (o2 rect-topology-mixin))
  (containing-p o2 o1))

(defmethod equal-p ((o1 point-topology-mixin) (o2 rect-topology-mixin))
  nil)

(defmethod equal-p ((o1 rect-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod equal-p ((o1 line-topology-mixin) (o2 point-topology-mixin))
  nil)

(defmethod equal-p ((o1 point-topology-mixin) (o2 point-topology-mixin))
  (with-orig-features ((point1 o1) (point2 o2))
    (feature= point1 point2)))

;;;----------------------------------------------------------------------------
;;; Composed topological predicates

(defmethod covering-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (and (containing-p o1 o2)
       (with-reduced-features ((area1 o1) (area2 o2))
         (weak-covering-p area1 area2))))

(defmethod covered-by-p ((o1 rect-topology-mixin) (o2 rect-topology-mixin))
  (covering-p o2 o1))

(defun weak-covering-p (area1 area2)
  (slet* ((barea2 (boundary area2))
          (dim (dim-of-intersect (boundary (interior area1)) barea2)))
    (and dim (= dim (dimension barea2)))))

;;;----------------------------------------------------------------------------
;;; Compute topological relations

(defun compute-topology-relation (o1 o2)
  (with-orig-features ((feature1 o1) (feature2 o2))
    (if (intersecting-p feature1 feature2)
      (if (equal-p o1 o2)
        'equal
        (with-reduced-features ((rfeature1 o1) (rfeature2 o2))
          (if (intersecting-p rfeature1 rfeature2)
            (cond
             ((containing-p o1 o2)
              (if (weak-covering-p rfeature1 rfeature2)
                'covering
                'containing))
             ((containing-p o2 o1)
              (if (weak-covering-p rfeature2 rfeature1)
                'covered-by
                'inside))
             ((= (dim-of-intersect feature1 feature2)
                 (1- (max (dimension feature1) (dimension feature2))))
              'crossing)
             (t 'overlapping))
            'touching)))
      'disjoint)))

(defun compute-all-relations (object-list)
  (maplist #'(lambda (list1) (compute-relation (first list1) (rest list1)))
           object-list))

(defmethod compute-relation ((object-list-1 list) (object-list-2 list))
  (mapcar #'(lambda (obj) (compute-relation obj (remove obj object-list-2)))
          object-list-1))

(defmethod compute-relation ((object topology-mixin) (object-list list))
  (list* object
         (apply #'nconc
                (mapcar #'(lambda (obj) (compute-relation object obj))
                        (remove object object-list)))))

(defmethod compute-relation ((object-1 point-topology-mixin)
                             (object-2 point-topology-mixin))
  nil)

(defmethod compute-relation ((object-1 topology-mixin) (object-2 topology-mixin))
  (let ((rel-dim (topology-relation object-1 object-2 nil)))
    (verify-relation (or (car rel-dim) 'disjoint) object-1 object-2)
    (when rel-dim
      (list (list (car rel-dim) object-2)
            #|(list 'intersection-dim object-2 (cdr rel-dim))|#))))

(defvar *pj-objects-in-cache* (make-hash-table :test #'eq))
(defvar *pj-relations-cache*
  (make-hash-table :test #'equal
                   :size (* 2 (hash-table-size *pj-objects-in-cache*))))

(defun clear-caches ()
  (setf *pj-object-features* (make-hash-table :test #'eq)
        *pj-objects-in-cache* (make-hash-table :test #'eq)
        *pj-relations-cache*
        (make-hash-table :test #'equal
                         :size (* 2 (hash-table-size *pj-objects-in-cache*)))))

(defun add-to-caches (key o1 o2 value)
  (push key (gethash o1 *pj-objects-in-cache*))
  (push key (gethash o2 *pj-objects-in-cache*))
  (setf (gethash key *pj-relations-cache*) value)
  value)

(defun do-remove-from-caches (object)
  (dolist (key (gethash object *pj-objects-in-cache*))
    (remhash key *pj-relations-cache*))
  (remhash object *pj-objects-in-cache*)
  (remhash object *pj-object-features*))

(defmethod remove-from-caches ((object topology-mixin))
  (do-remove-from-caches object))

(defmethod remove-from-caches ((object line-topology-mixin))
  (do-remove-from-caches object)
  (dolist (point (points object))
    (remove-from-caches point)))

(defmethod remove-from-caches ((object point-topology-mixin))
  (do-remove-from-caches object))

(defun print-caches (object)
  (pprint (list (list object (gethash object *pj-objects-in-cache*))
                (mapcar #'(lambda (o) (list o (gethash o *pj-relations-cache*)))
                        (gethash object *pj-objects-in-cache*))
                (list object (gethash object *pj-object-features*))))
  (when (slot-exists-p object 'points)
    (mapc #'print-caches (points object))))

(defun test-relations-cache ()
  (loop for key being the hash-key of *pj-relations-cache*
        using (hash-value val)
        always (relation-exclusive-p (car val) (car key) (cdr key))))

(defun topology-relation (o1 o2 complete-p)
  (let ((key (cons o1 o2)))
    (or (gethash key *pj-relations-cache*)
        (let ((rel (compute-topology-relation o1 o2)))
          (if (eq rel 'disjoint)
            (and complete-p (cons 'disjoint nil))
            (add-to-caches key o1 o2 (cons rel (dim-of-intersect o1 o2))))))))

(defmethod set-view-position :after ((view topology-mixin) h &optional v)
  (declare (ignore h v))
  (remove-from-caches view))

(defmethod set-view-size :after ((view topology-mixin) h &optional v)
  (declare (ignore h v))
  (remove-from-caches view))

(defmethod set-view-container :after ((view topology-mixin) new-container)
  (declare (ignore new-container))
  (remove-from-caches view))

(defun relationp (rel o1 o2)
  (multiple-value-bind (value found)
                       (gethash (cons o1 o2) *pj-relations-cache*)
    (print value)
    (and found (eq value rel))))

(defun print-topology-relation (o1 o2)
  (let ((rel (compute-topology-relation o1 o2))
        (inv-rel (compute-topology-relation o2 o1)))
    (and (relation-exclusive-p rel o1 o2) (relation-exclusive-p inv-rel o2 o1))
    (format t "~&Topological relation:~%~A ~A ~A" o1 rel o2)
    (format t "~%~A ~A INTERSECTION-DIM ~A~%" o1 o2 (dim-of-intersect o1 o2))
    (unless (eq rel inv-rel)
      (format t "~&Topological relation:~%~A ~A ~A~%" o2 inv-rel o1))))

(defun verify-relation (rel o1 o2)
  (or (funcall (relation-function rel) o1 o2)
      (cerror "return T"
              "Inconsistency detected for relation:~%(~A-P ~S ~S) returned NIL"
              rel o1 o2)
      t))

(defconstant relations
  '(disjoint touching inside containing overlapping crossing equal))

(defun relation-exclusive-p (rel o1 o2)
  (and (verify-relation rel o1 o2)
       (let ((result (mapcar #'(lambda (rel)
                                 (funcall (relation-function rel) o1 o2))
                             relations)))
         (or (= 1 (count t result))
             (cerror "return T"
                     "relation ~A-P not exclusive for ~S ~S !~% result: ~S"
                     rel o1 o2
                     (mapcar #'(lambda (rel pred) (list rel pred))
                             relations result))
             t))))

(defun relation-function (relation)
  (case relation
    (disjoint #'disjoint-p)
    (touching #'touching-p)
    (inside #'inside-p)
    (containing #'containing-p)
    (overlapping #'overlapping-p)
    (crossing #'crossing-p)
    (equal #'equal-p)
    (covering #'covering-p)
    (covered-by #'covered-by-p)
    (otherwise (error "Relation function of ~A undefined" relation))))
