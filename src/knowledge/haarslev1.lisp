;;;-*- Mode: Lisp; Package: PJ -*-

(in-package :pj)

(cl-startup)

(setf *krss-verbose* nil)

;----------------------------------------------------------------

(defmacro defprimitive (name expr)
  `(define-primitive-concept ,name ,expr))

(defmacro defdisjoint (name expr grouping)
  `(define-disjoint-primitive-concept ,name ,grouping ,expr))

(defmacro defconcept (name expr)
  `(define-concept ,name ,expr))

(defun cl-filler (derived-object role &optional remove-elem)
  (let ((fillers (remove remove-elem (cl-fillers derived-object role)
                         :test #'eq)))
    (when (second fillers)
      (cerror "Return only first filler"
              "Attribute ~S holds more than 1 filler for ~S: ~S"
              role derived-object fillers))
    (first fillers)))

(defun close-all-subroles (role &key exceptions
                                (inds (cl-concept-instances @pj-thing)))
  (unless (member role exceptions :test #'eq)
    (close-all-roles (cl-role-children role) inds exceptions)
    (dolist (role (cl-role-children role))
      (close-all-subroles role :exceptions exceptions :inds inds))))

(defun close-all-roles (roles
                        &optional (inds (cl-concept-instances @pj-thing))
                        (exceptions nil))
  (dolist (role roles)
    (unless (member role exceptions :test #'eq)
      (dolist (ind inds)
        (unless (cl-ind-closed-role? ind role)
          (cl-ind-close-role ind role))))))

(defun find-possible-subsumees (inds)
  (dolist (ind inds)
    (let* ((parents (cl-ind-parents ind))
           (children (delete nil (mapcar #'cl-concept-children parents))))
      (when children
        (format t "~&Individual ~S may be further specialized to ~S"
                ind children)))))

;----------------------------------------------------------------

(defprimitive geo-thing classic-thing)

(defprimitive atleast-1d geo-thing)
(defprimitive atmost-1d geo-thing)

(defdisjoint 0d-object atmost-1d geo-objects)
(defdisjoint 1d-object (and atleast-1d atmost-1d) geo-objects)
(defdisjoint 2d-object atleast-1d geo-objects)

(cl-define-primitive-role 'view :attribute t)
(cl-define-primitive-role 'position :attribute t)
(cl-define-primitive-role 'size :attribute t)

;----------------------------------------------------------------

(cl-define-primitive-role 'spatial-relation)

(cl-define-primitive-role 'touching :parent 'spatial-relation
                          :inverse 'touching)
(cl-define-primitive-role 'overlapping :parent 'spatial-relation
                          :inverse 'overlapping)
(cl-define-primitive-role 'crossing :parent 'spatial-relation
                          :inverse 'crossing)
(cl-define-primitive-role 'containing :parent 'spatial-relation
                          :inverse 'inside
                          :inverse-parent 'spatial-relation)
(cl-define-primitive-role 'directly-containing :parent 'containing
                          :inverse 'directly-inside
                          :inverse-parent 'inside)
(cl-define-primitive-role 'covering :parent 'directly-containing
                          :inverse 'covered-by
                          :inverse-parent 'directly-inside)

;----------------------------------------------------------------

(defun compute-directly-inside (ind role)
  (declare (ignore role))
  (let* ((i-fillers (cl-fillers ind @inside))
         (di-fillers (cl-fillers ind @directly-inside))
         (l (1- (length i-fillers))))
    (or di-fillers
        (if (zerop l)
          i-fillers
          (list
           (or
            (find-if #'(lambda (x) (= l (length (cl-fillers x @inside))))
                     i-fillers)
            (break "NIL as filler computed for directly-inside")))))))

(cl-add-filler-rule 'compute-directly-inside @geo-thing @directly-inside
                    #'compute-directly-inside
                    :filter '(and (at-least 1 inside)
                              (test-c cl-test-closed-roles? (inside))))

;----------------------------------------------------------------


(cl-define-primitive-role 'touching-atleast-1d :parent 'touching)
(cl-define-primitive-role 'touching-1d :parent 'touching-atleast-1d)
(cl-define-primitive-role 'touching-2d :parent 'touching-atleast-1d)

;----------------------------------------------------------------

(defconcept attached-1d
  (and 1d-object
       (at-least 2 touching)
       (at-most 4 touching)))

(defconcept end-1d
  (and attached-1d
       (at-most 3 touching)
       (at-most 1 touching-1d)
       (some touching-atleast-1d)))

(defconcept middle-1d
  (and attached-1d
       (all touching atmost-1d)
       (exactly 2 touching-1d)))

(defconcept simple-line
  (and end-1d
       (exactly 2 touching)
       (all touching 2d-object)))

(defun to-end-1d* (from next)
  (if (not next)
    from
    (to-end-1d*
     next
     (first (remove from (cl-fillers next @touching-1d))))))

(defun to-end-1d (from role)
  (declare (ignore role))
  (cond
   ((not (endp (cl-fillers from @to-end-1d)))
    nil)
   ((cl-instance? from @simple-line)
    (list from))
   (t (list (to-end-1d*
             from (first (cl-fillers from @touching-1d)))))))

(cl-define-primitive-role 'to-end-1d :inverse 'to-end-1d
                          :attribute t)
(cl-add-filler-rule 'find-end-1d @end-1d @to-end-1d
                    #'to-end-1d)

(defconcept line-handle
  (and end-1d (some to-end-1d)))

(defconcept link-handle
  (and line-handle
       (exactly 1 touching-2d)))

(defconcept docking-2d
  (and 2d-object
       (some touching-1d)
       (all touching-1d line-handle)))

(defun linked-2ds (from role)
  (declare (ignore role))
  (mapcan #'(lambda (x)
              (let ((end (cl-filler x @to-end-1d)))
                (when end
                  (let ((2d (cl-filler end @touching-2d from)))
                    (when 2d
                      (list 2d))))))
          (cl-fillers from @touching-1d)))

(cl-define-primitive-role 'linked-2ds :inverse 'linked-2ds)
(cl-add-filler-rule 'find-linked-2d @docking-2d @linked-2ds
                    #'linked-2ds)

(defconcept linked-2d
  (and 2d-object (some linked-2ds)))

(defconcept line-connector
  (and link-handle (all touching-2d linked-2d)))

#|
(cl-clear-kb)
(mapc #'(lambda (x) (cl-create-ind x '2d-object)) '(r1 r2))
(mapc #'(lambda (x) (cl-create-ind x '1d-object)) '(l1 l2 l3))
(cl-ind-add @r1 '(and 2d-object
                  (fills touching-1d l1)))
(cl-ind-add @r2 '(and 2d-object
                  (fills touching-1d l3)))
(cl-ind-add @l1
               '(and 1d-object
                 (fills touching-2d r1)
                 (fills touching-1d l2)))
(cl-ind-add @l2
               '(and 1d-object
                 (fills touching-1d l1)
                 (fills touching-1d l3)))
(cl-ind-add @l3
               '(and 1d-object
                 (fills touching-2d r2)
                 (fills touching-1d l2)))
(close-all-subroles (list @l1 @l2 @l3 @r1 @r2) @touching)
(mapcar #'(lambda (x) (list x (cl-ind-parents (cl-named-ind x))))
        '(r1 r2 l1 l2 l3))

(mapc #'(lambda (x) (cl-create-ind x '2d-object)) '(r3 r4))
(cl-create-ind 'l4 '1d-object)
(cl-ind-add @r3 '(and 2d-object (fills touching-1d l4)))
(cl-ind-add @r4 '(and 2d-object (fills touching-1d l4)))
(cl-ind-add @l4 '(and 1d-object
                  (fills touching-2d r3)
                  (fills touching-2d r4)))
(close-all-subroles (list @l4 @r3 @r4) @touching)
(mapcar #'(lambda (x) (list x (cl-ind-parents (cl-named-ind x))))
        '(r3 r4 l4))
(cl-exp-subsumes-ind @linked-2d @r1)
|#
