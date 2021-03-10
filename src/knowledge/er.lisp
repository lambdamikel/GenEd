

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

(defconcept is-inside
  (and geo-thing
       (at-least 1 inside)))

(cl-add-filler-rule 'compute-directly-inside @is-inside @directly-inside
                    #'compute-directly-inside
                    :filter '(test-c cl-test-closed-roles? (inside)))

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

;----------------------------------------------------------------

(defprimitive er-thing geo-thing)

(defprimitive er-element (and er-thing atleast-1d))

;----------------------------------------------------------------

(defun cl-inv-role-name (role)
  (let ((inv (cl-role-inverse (cl-named-role role))))
    (when inv
      (cl-role-name inv))))

(defmacro defqualifiedsubrole (role
                               qualification
                               &key
                               (name
                                (intern
                                 (concatenate 'string
                                              (string role)
                                              "-"
                                              (string qualification))))
                               (inv-name
                                (intern
                                 (concatenate 'string
                                              (string name) "-INVERSE")))

                               parent
                               (inverse-parent (or (cl-inv-role-name parent)
                                                   role))
                               break
                               no-rule)
  `(prog1
     (cl-define-primitive-role ',name :inverse ',inv-name
                               :parent ',parent
                               :inverse-parent ',inverse-parent)
     ,(unless no-rule
        `(cl-add-filler-rule ',(intern (concatenate 'string (string name)
                                                    "-INV-FILLER-RULE"))
                             (cl-named-concept ',qualification)
                             (cl-named-role ',inv-name)
                             #'(lambda (ind role)
                                 (declare (ignore role))
                                 (when ,break
                                   (break "in filler rule of ~A" ',inv-name))
                                 (or
                                  (cl-fillers ind
                                              (cl-named-role ',inverse-parent))
                                  (break "NIL as filler computed for ~A"
                                         ',inv-name)))
                             :filter
                             '(and (at-least 1 ,inverse-parent)
                               #|(test-c cl-test-closed-roles? (,role))|#)))))

;----------------------------------------------------------------

(defmacro deforconcept (concept-name parent ors)
  `(prog1
     (defprimitive ,concept-name ,parent)
     ,@(mapcar #'(lambda (or)
                   `(cl-add-rule (intern
                                 (concatenate 'string "add-" (string ',or)
                                              "-to-" (string ',concept-name)))
                                (cl-named-object ',or)
                                ',concept-name))
               ors)))

;----------------------------------------------------------------

(defmacro retrieve-rel (rel &optional (super @er-thing))
  `(loop for di in (cl-concept-instances ,super)
         for fillers = (mapcar #'cl-name (cl-fillers di ,rel))
         when fillers collect (list (cl-name di) fillers)))

;----------------------------------------------------------------

(defprimitive point (and 0d-object er-thing))

(defprimitive start-point point)

(defprimitive end-point point)

(defprimitive link (and 1d-object er-element))

(defprimitive arrow (and 1d-object er-element))

(defprimitive region (and 2d-object er-element))
(defprimitive rect-region region)
(defprimitive rounded-region region)
(defprimitive oval-region region)

(defprimitive text (and 2d-object er-element))
(cl-define-primitive-role 'text-value :attribute t)

(cl-define-primitive-role 'has-parts :inverse 'part-of
                          :inverse-attribute t)

;----------------------------------------------------------------

(defqualifiedsubrole touching point :parent touching :no-rule t)
(defqualifiedsubrole touching start-point :parent touching-point :no-rule t)
(defqualifiedsubrole touching end-point :parent touching-point :no-rule t)
(defqualifiedsubrole touching link :parent touching-1d :no-rule t)
(defqualifiedsubrole touching arrow :parent touching-1d :no-rule t)
(defqualifiedsubrole touching region :parent touching-2d :no-rule t)
(defqualifiedsubrole touching rect-region :parent touching-region :no-rule t)
(defqualifiedsubrole touching rounded-region :parent touching-region :no-rule t)
(defqualifiedsubrole touching oval-region :parent touching-region :no-rule t)
(defqualifiedsubrole touching text :parent touching-2d :no-rule t)

(defqualifiedsubrole containing region
  :parent directly-containing :inverse-parent directly-inside)
(defqualifiedsubrole containing text
  :parent directly-containing :inverse-parent directly-inside)

(defqualifiedsubrole covering region :parent covering)

(cl-role-change-canonical-name 'connected @linked-2ds)

;----------------------------------------------------------------

(defconcept relationship-entity
  (and link
       (exactly 1 crossing)
       (all crossing text)
       (exactly 2 touching)
       (all touching region)))

(defconcept cardinality
  (and text
       (exactly 1 crossing)
       (all crossing relationship-entity)
       (all text-value (set "1" "m" "n"))))

(defconcept 1-cardinality
  (and cardinality
       (is text-value "1")))

(defconcept m-cardinality
  (and cardinality
       (is text-value "m")))

(defconcept n-cardinality
  (and cardinality
       (is text-value "n")))

(defconcept 1-relationship-entity
  (and relationship-entity
       (all crossing 1-cardinality)))

(defconcept m-relationship-entity
  (and relationship-entity
       (all crossing m-cardinality)))

(defconcept n-relationship-entity
  (and relationship-entity
       (all crossing n-cardinality)))

(defconcept attribute-entity
  (and link
       (none crossing)
       (exactly 2 touching)
       (all touching region)))

(deforconcept attribute-or-relationship-entity link
  (attribute-entity relationship-entity))

(defconcept entity
  (and rect-region
       (exactly 1 containing)
       (all containing text)
       (some touching)
       (all touching attribute-or-relationship-entity)
       (some connected)))

(defconcept entity-name
  (and text
       (exactly 1 directly-inside)
       (all directly-inside entity)))

(defqualifiedsubrole touching 1-relationship-entity :parent touching-link)
(defqualifiedsubrole touching m-relationship-entity :parent touching-link)
(defqualifiedsubrole touching n-relationship-entity :parent touching-link)

(defconcept relationship
  (and rounded-region
       (exactly 1 containing)
       (all containing text)
       (exactly 2 connected)
       (all connected entity)
       (exactly 2 touching)
       (all touching relationship-entity)
       (at-most 2 touching-1-relationship-entity)
       (at-most 1 touching-m-relationship-entity)
       (at-most 1 touching-n-relationship-entity)))

(defconcept relationship-name
  (and text
       (exactly 1 directly-inside)
       (all directly-inside relationship)))

(defconcept attribute
  (and oval-region
       (exactly 1 containing)
       (all containing text)
       (exactly 1 connected)
       (all connected entity)))

(defconcept attribute-name
  (and text
       (exactly 1 directly-inside)
       (all directly-inside attribute)))

;----------------------------------------------------------------

(defun close-pj-roles ()
  (close-all-roles (list* @covering @covered-by
                          (cl-role-children @spatial-relation))
                   (cl-concept-instances @er-thing))
  (close-all-subroles @inside :inds (cl-concept-instances @er-thing))
  (close-all-subroles @containing :inds (cl-concept-instances @er-thing))
  (close-all-roles (cl-role-children @touching)
                   (cl-concept-instances @er-thing))
  (close-all-subroles @touching-atleast-1d
                      :inds (cl-concept-instances @er-thing))
  (close-all-subroles @touching-region-inverse
                      :inds (cl-concept-instances @er-thing))
  (close-all-roles (list @linked-2ds)
                   (cl-concept-instances @er-thing)))




