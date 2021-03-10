;;;-*- Mode: Lisp; Package: pj -*-

(in-package :pj)

;(load "pj:cl-geometry-db-5.lisp")

(defprimitive pj-thing geo-thing)

(defprimitive pj-element (and pj-thing atleast-1d))

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
                               break)
  `(progn
     (cl-define-primitive-role ',name :inverse ',inv-name
                               :parent ',parent
                               :inverse-parent ',inverse-parent)
     (cl-add-filler-rule ',(intern (concatenate 'string (string name)
                                                "-INV-FILLER-RULE"))
                         (cl-named-concept ',qualification)
                         (cl-named-role ',inv-name)
                         #'(lambda (ind role)
                             (declare (ignore role))
                             (when ,break
                               (break "in filler rule of ~A" ',inv-name))
                             (or
                              (cl-fillers ind (cl-named-role ',inverse-parent))
                              (break "NIL as filler computed for ~A"
                                     ',inv-name)))
                         :filter
                         '(and (at-least 1 ,inverse-parent)
                           (test-c cl-test-closed-roles? (,role))))))

;----------------------------------------------------------------

(defmacro retrieve-rel (rel)
  `(loop for di in (cl-concept-instances @pj-thing)
         for fillers = (mapcar #'cl-name (cl-fillers di ,rel))
         when fillers collect (list (cl-name di) fillers)))

;----------------------------------------------------------------

(defprimitive point (and 0d-object pj-thing))

(defprimitive start-point point)

(defprimitive end-point point)

(defprimitive link (and 1d-object pj-element))

(defprimitive arrow (and 1d-object pj-element))

(defprimitive region (and 2d-object pj-element))

(defprimitive text (and 2d-object pj-element))
(cl-define-primitive-role 'text-value :attribute t)

(cl-define-primitive-role 'has-parts :inverse 'part-of
                          :inverse-attribute t)

;----------------------------------------------------------------

(defqualifiedsubrole touching point :parent touching)
(defqualifiedsubrole touching link :parent touching-1d)
;(defqualifiedsubrole touching arrow :parent touching-1d)
(defqualifiedsubrole touching region :parent touching-2d)
(defqualifiedsubrole touching text :parent touching-2d)

(defqualifiedsubrole containing region
  :parent directly-containing :inverse-parent directly-inside)
(defqualifiedsubrole containing text
  :parent directly-containing :inverse-parent directly-inside)

(defqualifiedsubrole covering region :parent covering)

;----------------------------------------------------------------

(defun cl-ind-add-hook-vh (ind)
  (when (cl-ind-closed-role? ind @touching)
    (cl-fillers ind @touching)))

;----------------------------------------------------------------

(defconcept empty-region
  (and region (none containing)))

(defconcept port
  (and empty-region (at-most 1 touching-region)))

(defconcept term
  (and region
       (none covered-by)
       (all touching-region port)
       (all touching-1d arrow)
       (all touching-point end-point)))

(defconcept reference-port
  (and port
       (exactly 1 covered-by)
       (all covered-by term)
       (at-most 2 touching)
       (none touching-region)
       (at-most 1 touching-link)
       (at-most 1 touching-point)))

(defconcept argument-port
  (and port
       (none covered-by)
       (exactly 1 touching-region)
       (all touching-region term)))

(defconcept single-port
  (and port
       (none covered-by)
       (none touching-region)))

(defconcept empty-port
  (and argument-port (none touching-link)))

(defconcept linked-port
  (and port linked-2d))

(defconcept linked-argument-port
  (and linked-port argument-port))

(defconcept data-term
  (and term
       (exactly 1 covering)
       (all covering reference-port)
       (none touching-1d)
       (none touching-point)))

(defconcept constant
  (and data-term
       (at-most 1 containing-region)
       (exactly 1 containing-text)
       (none touching)))

(defconcept term-string
  (and text
       (some directly-inside)
       (all directly-inside term)))

(defconcept value-text
  (and term-string
       (exactly 1 directly-inside)
       (all directly-inside data-term)))

(defconcept term-list
  (and data-term
       (at-most 2 touching-region)
       (at-most 1 containing-text)
       (all containing-text value-text)
       (at-most 1 containing-region)))

(defconcept empty-term-list
  (and term-list
       (none touching-region)
       (none containing-text)))

(defconcept label
  (and region
       (exactly 1 containing)
       (all containing text)
       (none touching)))

(defconcept label-text
  (and text
       (some directly-inside)
       (all directly-inside label)))

(defqualifiedsubrole containing label :parent containing-region)

(defconcept labeled-term
  (and term
       (exactly 1 containing-label)
       (some touching-region)
       (all touching-region argument-port)))

(defconcept rule-term
  (and term (none covering-region)))

(defconcept primitive-relation
  (and rule-term
       labeled-term
       (exactly 2 containing)))

(defconcept rule-body
  (and rule-term
       (all touching-region argument-port)
       (some touching-region)))

(defconcept body-arg-port
  (and argument-port
       (all touching-region rule-body)))

(defconcept rule
  (and rule-body
       (exactly 1 inside)
       (all inside rule-body)
       #|(all has-asks asker-port)|#))

(defconcept function-term
  (and data-term
       labeled-term
       (exactly 3 containing)
       (exactly 1 directly-inside)
       (all directly-inside rule)))

(defqualifiedsubrole crossing rule :parent crossing)

(defconcept call-arrow
  (and arrow
       (some crossing-rule)))

(defconcept start-of-call-arrow
  (and start-point
       (some part-of)
       (all part-of call-arrow)))

(defconcept agent-call
  (and rule-body
       (exactly 1 directly-inside)
       (all directly-inside rule)
       (exactly 1 crossing)
       (all crossing call-arrow)
       (exactly 1 containing)
       (all containing start-of-call-arrow)))

(defqualifiedsubrole containing rule :parent containing-region)

(defconcept agent
  (and rule-body
       (some containing-rule)
       (none inside)
       (at-most 1 touching-text)))

(defconcept agent-name
  (and text
       (exactly 1 touching)
       (all touching agent)))

(defconcept guard-test
  (and primitive-relation
       (exactly 1 inside)
       (all inside agent)))

(defconcept recursive-call-arrow
  (and call-arrow
       (exactly 1 covered-by)
       (all covered-by agent)))

(defconcept other-call-arrow
  (and call-arrow
       (exactly 1 touching)
       (all touching agent)))

(defqualifiedsubrole touching argument-port
  :name touching-arg-port :parent touching-region)

(defconcept channel
  (and arrow
       (exactly 2 touching)
       (all touching port)
       (some touching-arg-port)
       (all touching-arg-port argument-port)))

(defconcept end-of-channel
  (and end-point
       (some part-of)
       (all part-of channel)))

(defconcept attached-port
  (and port
       (some touching-point)
       (all touching-point end-of-channel)))


;----------------------------------------------------------------

(defun reaching-to* (from next &optional (fillers nil))
  (cond
   ((cl-instance? next @reference-port)
    (or
     (mapcan #'(lambda (arg) (reaching-to* next arg (list* next fillers)))
             (cl-fillers (cl-filler next @covered-by) @touching-region))
     (list* next fillers)))))

(defun reaching-to (from role)
  (declare (ignore role))
  (let ((linked-ports (cl-fillers from @linked-2ds)))
    (mapcan #'(lambda (next) (reaching-to* from next)) linked-ports)))

#|  (cond
   ((and (not (rest linked-ports))
         (cl-instance? (first linked-ports) @reference-port)
   ((cl-instance? from @simple-line)
    (list from))
   (t (list (to-end-1d*
             from (first (cl-fillers from @touching-1d)))))))|#

(cl-define-primitive-role 'reaching-to :inverse 'reachable-from)
(cl-add-filler-rule 'find-reaching-to @linked-argument-port @reaching-to
                    #'reaching-to)

;----------------------------------------------------------------

(defconcept reachable-ref-port
  (and linked-port
       reference-port
       (exactly 1 reachable-from)
       (all reachable-from body-arg-port)))

(defqualifiedsubrole touching attached-port :parent touching-region)

(defconcept ask-channel
  (and channel
       (some touching-arg-port)
       (all touching-arg-port argument-port)
       (some touching-attached-port)
       (all touching-attached-port attached-port)))

(defprimitive attached-or-linked-port port)
(cl-add-rule 'add-attached-or-port @attached-port 'attached-or-linked-port)
(cl-add-rule 'add-linked-or-port @linked-port 'attached-or-linked-port)

(defconcept ask-data-term
  (and data-term
       (exactly 1 inside)
       (all inside rule-body)
       (all covering reachable-ref-port)
       (all touching-region attached-or-linked-port)))

(defconcept data-asking-arg-port
  (and linked-port
       argument-port
       (some reaching-to)
       (all reaching-to reachable-ref-port)))

(defconcept start-of-channel
  (and start-point
       (some part-of)
       (all part-of channel)))

(defconcept tell-asking-arg-port
  (and argument-port
       (some touching-point)
       (all touching-point start-of-channel)))

(defprimitive asking-arg-port argument-port)
(cl-add-rule 'add-data-asking-arg-port
             @data-asking-arg-port 'asking-arg-port)
(cl-add-rule 'add-tell-asking-arg-port
             @tell-asking-arg-port 'asking-arg-port)

;----------------------------------------------------------------

(defun close-pj-roles ()
  (close-all-roles (list* @covering @covered-by
                          (cl-role-children @spatial-relation)))
  (close-all-subroles @inside :exceptions (list @containing-rule-inverse))
  (close-all-subroles @containing :exceptions (list @containing-rule))
  (close-all-roles (cl-role-children @touching))
  (close-all-subroles @touching-atleast-1d
                      :exceptions (list @touching-attached-port))
  (close-all-subroles @touching-region-inverse
                      :exceptions (list @touching-attached-port-inverse))
  (close-all-roles (list @touching-attached-port
                         @touching-attached-port-inverse))
  (close-all-subroles @crossing)
  (close-all-roles (list @containing-rule @containing-rule-inverse))
  (close-all-roles (list @reachable-from @reaching-to)))

#|
(defun set-subroles1 ()
  (compute-directly-inside-fillers)
  (dolist (ind (cl-concept-instances @pj-thing))
    (set-touching-subroles ind (cl-fillers ind @touching))
    (set-containing-subroles ind (cl-fillers ind @directly-containing))
    (set-covering-subroles ind (cl-fillers ind @covering))))

(defun compute-directly-inside-fillers ()
  (dolist (ind (cl-concept-instances
                (cl-normalize-concept
                 (cl-parse-expression '(and pj-thing (at-least 1 inside))))))
    (let ((fillers (cl-fillers ind @inside)))
      (unless (cl-fillers ind @directly-inside)
        (add-directly-inside
         (sort (list* ind (copy-list fillers)) #'>
               :key #'(lambda (x) (length (cl-fillers x @inside)))))))))

(defun add-directly-inside (chain)
  (mapl #'(lambda (l)
            (let ((a (first l))
                  (b (second l)))
              (when (and b (not (cl-fillers a @directly-inside)))
                (classic:cl-ind-add
                 a `(fills directly-inside ,(classic:cl-ind-name b))))))
        chain))

(defun set-touching-subroles (ind fillers)
  (dolist (filler fillers)
    (let ((fname (cl-ind-name filler)))
      (cond ((cl-instance? filler @point)
             (cl-ind-add ind `(fills touching-point ,fname)))
            ((cl-instance? filler @link)
             (cl-ind-add ind `(fills touching-link ,fname)))
            ((cl-instance? filler @region)
             (cl-ind-add ind `(fills touching-region ,fname))
             (when (cl-instance? filler @argument-port)
               (cl-ind-add ind `(fills touching-arg-port ,fname)))
             (when (cl-instance? filler @attached-port)
               (cl-ind-add ind `(fills touching-attached-port ,fname))))
            ((cl-instance? filler @text)
             (cl-ind-add ind `(fills touching-text ,fname)))))))

(defun set-containing-subroles (ind fillers)
  (dolist (filler fillers)
    (let ((fname (cl-ind-name filler)))
      (cond ((cl-instance? filler @region)
             (cl-ind-add ind `(fills containing-region ,fname)))
            ((cl-instance? filler @text)
             (cl-ind-add ind `(fills containing-text ,fname)))
            ((cl-instance? filler @label)
             (cl-ind-add ind `(fills containing-label ,fname)))))))

(defun set-covering-subroles (ind fillers)
  (dolist (filler fillers)
    (let ((fname (cl-ind-name filler)))
      (cond ((cl-instance? filler @region)
             (cl-ind-add ind `(fills covering-region ,fname)))))))

(defun set-subroles2 ()
  (dolist (ind (cl-concept-instances @pj-thing))
    (set-containing-subroles2 ind (cl-fillers ind @directly-containing))
    (set-crossing-subroles ind (cl-fillers ind @crossing))))

(defun set-containing-subroles2 (ind fillers)
  (dolist (filler fillers)
    (let ((fname (cl-ind-name filler)))
      (cond ((cl-instance? filler @rule)
             (cl-ind-add ind `(fills containing-rule ,fname)))))))

(defun set-crossing-subroles (ind fillers)
  (dolist (filler fillers)
    (let ((fname (cl-ind-name filler)))
      (cond ((cl-instance? filler @rule)
             (cl-ind-add ind `(fills crossing-rule ,fname)))))))
|#

