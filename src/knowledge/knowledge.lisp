;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: Knowledge; Base: 10 -*-

(in-package knowledge)


(cl-reader-init)


(cl-startup)

(setf *krss-verbose* nil)

(cl-set-classic-warn-mode t)

;----------------------------------------------------------------

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


;;;
;;;
;;;

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

                               (parent role)
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

;;;
;;;
;;;
;----------------------------------------------------------------


(cl-define-primitive-role 'spatial-relation)

(cl-define-primitive-role 'in-relation-with-objects 
			  :parent 'spatial-relation)			 

(cl-define-primitive-role 'disjoint-with
			  :parent 'spatial-relation)

(cl-define-primitive-role 'touching-objects :inverse 'touching-objects 
			  :parent 'in-relation-with-objects)

(cl-define-primitive-role 'intersects-objects :inverse 'intersects-objects
			  :parent 'in-relation-with-objects)

;;;                     spatial-relation
;;;                       /          \
;;;                     in-rel.w.    disj.w.
;;;                     /  |    \
;;;                    /   |     \
;;;                   /    |      \
;;;              touching inters. contains
;;;                                  |
;;;                                direc.contains
;;;                                  |
;;;                                covers

(cl-define-primitive-role 'contains-objects :inverse 'contained-in-objects 
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

(cl-define-primitive-role 'directly-contains-objects :inverse 'directly-contained-by-object
			  :parent 'contains-objects
			  :inverse-parent  'contained-in-objects)

(cl-define-primitive-role 'covers-objects :inverse 'covered-by-object
			  :parent 'directly-contains-objects
			  :inverse-parent 'directly-contained-by-object)

;;;
;;; In diesen Rollen steht, mit wem das Ind. verbunden ist, und ueber welches Ende des Pfeiles:
;;;
;;;             linked-over-with
;;;               /          \
;;;         start-link.o.w.  end-link.o.w.    0 ----> # : start-l.o.w.(0,#),
;;;                                                         end-l.o.w.(#,0),
;;;                                                             l.o.w.(#,0) /\ l.o.w(0,#). 


(cl-define-primitive-role 'linked-over-with :inverse 'linked-over-with
			  :parent 'in-relation-with-objects)

(cl-define-primitive-role 'start-linked-over-with :inverse 'end-linked-over-with
			  :parent 'linked-over-with
			  :inverse-parent 'linked-over-with)

;;;
;;; Hier werden die Linker eingetragen:   0 -----> #   : linker-objects(A,L), linker-objects(B,L).
;;;                                       A    L   B

(cl-define-primitive-role 'linker-objects :inverse 'points-related-with
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

(cl-define-primitive-role 'start-linker-objects :inverse 'startpoint-related-with
			  :parent 'linker-objects
			  :inverse-parent 'points-related-with)

(cl-define-primitive-role 'end-linker-objects :inverse 'endpoint-related-with
			  :parent 'linker-objects
			  :inverse-parent 'points-related-with)

;;;
;;;



;;;
;;; Rollen eines Linkers: points-related-with
;;;                          /           \                 0 ----> # : s.r.w.(L,A), e.r.w.(L,B)
;;;                  startpoint-rel.-w.  endpoint-rel.-w.  A   L   B 

#|
(cl-define-primitive-role 'points-related-with :inverse 'linker-objects
			  :parent 'in-relation-with-objects)		       

(cl-define-primitive-role 'startpoint-related-with :inverse 'endpoint-related-with
			  :parent 'points-related-with
			  :inverse-parent 'points-related-with)
|#

;;;
;;; Fuer Composite-Things:
;;;

(cl-define-primitive-role 'has-parts :inverse 'part-of
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

;;;
;;;
;;;

(cl-define-primitive-role 'filled-bool :attribute t)
(cl-define-primitive-role 'radius-real :attribute t)
(cl-define-primitive-role 'text-string :attribute t)

;;;
;;;
;;;


(cl-define-primitive-concept 'gened-thing 'classic-thing)

(cl-define-disjoint-primitive-concept 'basic-thing 'gened-thing 'basic-or-comp)
(cl-define-disjoint-primitive-concept 'composite-thing 'gened-thing 'basic-or-comp)

(cl-define-primitive-concept '0d 'gened-thing)
(cl-define-primitive-concept '1d 'gened-thing)
(cl-define-primitive-concept '2d 'gened-thing)

(cl-define-primitive-concept 'at-least-1d '(and 1d 2d))
(cl-define-primitive-concept 'at-most-1d  '(and 0d 1d))

;;;
;;;
;;;

(cl-define-primitive-concept 'directed-element '(and 1d basic-thing))

(cl-define-disjoint-primitive-concept 'g-circle '(and 2d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-line '(and 1d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-arrow 'directed-element 'type)

(cl-define-disjoint-primitive-concept 'g-rectangle '(and 2d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-chain '(and 1d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-directed-chain 'directed-element 'type)

(cl-define-disjoint-primitive-concept 'g-polygon '(and 2d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-spline-polygon '(and 2d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-spline-chain '(and 1d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-directed-spline-chain 'directed-element 'type)

(cl-define-disjoint-primitive-concept 'g-text '(and 2d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-point '(and 0d basic-thing) 'type)

;;;
;;;
;;;

(defqualifiedsubrole linked-over-with g-circle
  :parent linked-over-with :inverse-parent linked-over-with)

(defqualifiedsubrole linked-over-with g-rectangle
  :parent linked-over-with :inverse-parent linked-over-with)


(defqualifiedsubrole has-parts g-circle
  :parent has-parts :inverse-parent part-of)


(defqualifiedsubrole has-parts g-rectangle
  :parent has-parts :inverse-parent part-of)

(defqualifiedsubrole has-parts g-arrow
  :parent has-parts :inverse-parent part-of)

;;;
;;; Diese Konzepte sind wohl von generellem Nutzen:
;;;

(define-concept nur-beruehrende-objekte
    (and 
     basic-thing					      
     (none contained-in-objects)				   
     (some touching-objects)					      
     (none has-parts)))

(define-concept verbinder
    (and 
     directed-element		     
     (exactly 2 points-related-with)))


(defun subset? (ind sym1 sym2)
  (let* ((role1 (cl-named-role sym1))
	 (role2 (cl-named-role sym2))
	 (fillers1 (cl-fillers ind role1))
	 (fillers2 (cl-fillers ind role2)))
	 
    (if (and (cl-ind-closed-role? ind role1)
	     (cl-ind-closed-role? ind role2))
	(null (set-difference
	       fillers2 fillers1))
      '?)))

(defun member? (ind sym1 sym2)
  (let* ((role1 (cl-named-role sym1))
	 (role2 (cl-named-role sym2))
	 (fillers1 (cl-fillers ind role1))
	 (fillers2 (cl-fillers ind role2)))
	 
    (if (and (cl-ind-closed-role? ind role1)
	     (cl-ind-closed-role? ind role2))
	(if (> (length fillers1) 1)
	    nil
	  (member (first fillers1) fillers2))
      '?)))


(defun not-member? (ind sym1 sym2)
  (let ((member (member? ind sym1 sym2)))
    (case member
      (? '?)
      (t nil)
      (nil t))))


(cl-define-concept 'tt-verbinder
		   '(and 
		     verbinder
		     (test-c member? 
		      startpoint-related-with
		      touching-objects)
		     (test-c member?
		      endpoint-related-with
		      touching-objects)))

(cl-define-concept 'it-verbinder
    '(and 
      verbinder
      (test-c member?
       startpoint-related-with
       intersects-objects)
      (test-c member?
       endpoint-related-with
       touching-objects)))

(cl-define-concept 'ti-verbinder
    '(and
      verbinder
      (test-c member?
       startpoint-related-with
       touching-objects)
      (test-c member?
       endpoint-related-with
       intersects-objects)))

(cl-define-concept 'ii-verbinder
		   '(and 
		     verbinder
		     (test-c member?
		      startpoint-related-with intersects-objects)
		     (test-c member?
		      endpoint-related-with intersects-objects)))

(cl-define-concept 'ct-verbinder
		   '(and
		     verbinder
		     (test-c member?
		      startpoint-related-with covered-by-object)
		     (test-c member?
		      endpoint-related-with touching-objects)))
		      

(cl-define-concept 'tc-verbinder
		   '(and
		     verbinder
		     (test-c member?
		      startpoint-related-with touching-objects)
		     (test-c member?
		      endpoint-related-with covered-by-object)))
		      

(cl-define-concept 'ci-verbinder
		   '(and 
		     verbinder
		     (test-c member? 
		      startpoint-related-with covered-by-object)
		     (test-c member?
		      endpoint-related-with intersects-objects)
		     (test-c not-member?
		      endpoint-related-with touching-objects)))

(cl-define-concept 'ic-verbinder
		   '(and
		     verbinder
		     (test-c member?
		      startpoint-related-with intersects-objects)
		     (test-c member?
		      endpoint-related-with covered-by-object)))
		  

;;;;
;;;;
;;;;



(define-concept stelle-oder-transition
    (and 
     nur-beruehrende-objekte
     (all touching-objects verbinder)))

(define-concept stelle*
    (and 
     g-circle
     stelle-oder-transition))

(define-concept transition*
    (and 
     g-rectangle
     stelle-oder-transition))

(define-concept stelle
    (and 
     stelle*
     (all linked-over-with transition*)))

(define-concept konflikt-stelle
    (and 
     stelle
     (at-least 2 end-linked-over-with)))

(define-concept start-stelle
    (and 
     stelle
     (none start-linked-over-with)))

(define-concept end-stelle
    (and
     stelle
     (none end-linked-over-with)))

(define-concept normale-stelle
    (and 
     stelle
     (some start-linked-over-with)
     (some end-linked-over-with)))

(define-concept stelle-mit-kapazitaet 
    (and
     stelle
     (exactly 1 intersects-objects)
     (all intersects-objects g-text)))

(define-concept marke
    (and
     g-circle
     (some filled-bool)))

(define-concept stelle-mit-marken
    (and 
     stelle
     (some contains-objects)
     (all contains-objects marke)))

(define-concept transition
    (and 
     transition*
     (at-least 2 linked-over-with)
     (all linked-over-with stelle*)))

(define-concept netz-kante
    (and 
     verbinder
     (all touching-objects stelle-oder-transition)))

(define-concept netz-kante-mit-kapazitaet
    (and 
     netz-kante
     (exactly 1 intersects-objects)
     (all intersects-objects g-text)))

(define-concept petri-netz
    (and
     composite-thing
     (at-least 5 has-parts)		; kleinstes Netz: 0 -> # -> 0
     (all has-parts-g-circle stelle)
     (all has-parts-g-rectangle transition)
     (all has-parts-g-arrow verbinder)))

;;;
;;;
;;;

(defconstant +known-concepts+
    '(verbinder 
      tt-verbinder
      it-verbinder
      ti-verbinder
      ii-verbinder
      ci-verbinder
      ic-verbinder
      ct-verbinder
      tc-verbinder
      
      stelle transition netz-kante
      netz-kante-mit-kapazitaet
      konflikt-stelle
      start-stelle end-stelle normale-stelle
      stelle-mit-kapazitaet
      petri-netz
      g-circle
      g-line
      g-rectangle
      g-arrow
      g-polygon
      g-spline-chain
      g-directed-spline-chain
      g-spline-polygon
      g-chain
      g-directed-chain
      g-text
      g-point
      ))

   
(defconstant +known-roles+
    '(
      spatial-relation
      in-relation-with-objects
      disjoint-with
      touching-objects
      intersects-objects
      covers-objects
      covered-by-object
      contains-objects
      contained-in-objects
      directly-contains-objects
      directly-contained-by-object
      
      linked-over-with
      
      start-linked-over-with
      end-linked-over-with
      
      points-related-with
      startpoint-related-with
      endpoint-related-with
      
      linker-objects
      start-linker-objects
      end-linker-objects
      
      has-parts
      has-parts-g-circle
      has-parts-g-rectangle
      has-parts-g-arrow
      part-of
      
      filled-bool
      radius-real
      text-string
      ))
	
