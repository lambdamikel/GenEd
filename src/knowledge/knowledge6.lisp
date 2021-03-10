 ;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: Knowledge; Base: 10 -*-

(in-package knowledge)

(cl-reader-init)

(cl-clear-kb)
(cl-startup)

(setf *krss-verbose* nil)

(cl-set-classic-warn-mode t)

;----------------------------------------------------------------


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

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun cl-inv-role-name (role)
    (print role)
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
				 (specialized-concept t)		
				 break)
  `(progn
     (cl-define-primitive-role ',name :inverse ',inv-name
                               :parent ',parent
                               :inverse-parent ',inverse-parent)
     (cl-add-filler-rule ',(intern (concatenate 'string (string name)
                                                "-INV-FILLER-RULE"))
			 ,(if specialized-concept 
			      `(cl-named-concept ',qualification)
			    '(cl-named-concept 'classic-thing))
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

(defmacro def-or-concept (concept1 concept2)
  (let ((or-name
	 (intern (concatenate 'string
		   (write-to-string concept1)
		   "-OR-"
		   (write-to-string concept2)))))
    `(progn
       (cl-define-primitive-concept
	',or-name 
	'basic-thing)
		  
       (cl-add-rule ',(gensym)
		    (cl-named-concept ',concept1)
		    ',or-name)
       
       (cl-add-rule ',(gensym)
		    (cl-named-concept ',concept2)
		    ',or-name))))

;----------------------------------------------------------------

;;; selbstinverse Basisrelationen

(cl-define-primitive-role 'spatial-relation :inverse 'spatial-relation)

(cl-define-primitive-role 'in-relation-with-objects :inverse 'in-relation-with-objects 
			  :parent 'spatial-relation
			  :inverse-parent 'spatial-relation)

(cl-define-primitive-role 'disjoint-with :inverse 'disjoint-with			  			  :parent 'spatial-relation
			  :inverse-parent 'spatial-relation)

(cl-define-primitive-role 'touching-objects :inverse 'touching-objects 
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

(cl-define-primitive-role 'intersects-objects :inverse 'intersects-objects
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

(cl-define-primitive-role 'intersects-0-objects :inverse 'intersects-0-objects
			  :parent 'intersects-objects
			  :inverse-parent 'intersects-objects)

(cl-define-primitive-role 'intersects-1-objects :inverse 'intersects-1-objects
			  :parent 'intersects-objects
			  :inverse-parent 'intersects-objects)

(cl-define-primitive-role 'intersects-2-objects :inverse 'intersects-2-objects
			  :parent 'intersects-objects
			  :inverse-parent 'intersects-objects)

;;;
;;;                     spatial-relation
;;;                       /          \
;;;                     in-rel.w.    disj.w.
;;;                     /  |    \
;;;                    /   |     \
;;;                   /    |      \
;;;              touching inters. contains
;;;                        /|\       |
;;;                       / | \    direc.contains
;;;                      0  1  2     |
;;;                                covers

;----------------------------------------------------------------

;;; nicht-selbstinverse Basisrelationen

(cl-define-primitive-role 'contains-objects :inverse 'contained-in-objects 
			  :parent 'in-relation-with-objects
			  :inverse-parent  'in-relation-with-objects)

(cl-define-primitive-role 'directly-contains-objects :inverse 'directly-contained-by-object
			  :parent 'contains-objects
			  :inverse-parent 'contained-in-objects)

(cl-define-primitive-role 'covers-objects :inverse 'covered-by-object
			  :parent 'directly-contains-objects
			  :inverse-parent 'directly-contained-by-object)

;----------------------------------------------------------------

;;;
;;; In diesen Rollen steht, mit wem das Ind. verbunden ist, und ueber welches Ende des Pfeiles:
;;;
;;;             linked-over-with
;;;               /          \
;;;         start-link.o.w.  end-link.o.w.    0 ----> # : start-l.o.w.(0,#),
;;;                                                         end-l.o.w.(#,0),
;;;                                                             l.o.w.(#,0) /\ l.o.w(0,#). 


(cl-define-primitive-role 'linked-over-with :inverse 'linked-over-with
			  :parent 'in-relation-with-objects
			  :inverse-parent 'linked-over-with)

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
;;; Rollen eines Linkers: points-related-with
;;;                          /           \                 0 ----> # : s.r.w.(L,A), e.r.w.(L,B)
;;;                  startpoint-rel.-w.  endpoint-rel.-w.  A   L   B 

;;;
;;; Fuer Composite-Things:
;;;

(cl-define-primitive-role 'has-parts :inverse 'part-of
			  :parent 'in-relation-with-objects
			  :inverse-parent 'in-relation-with-objects)

;;;
;;; Fuer Directed-Elements:
;;;

(cl-define-primitive-role 'has-points :inverse 'point-part-of
			  :parent 'has-parts 
			  :inverse-parent 'part-of)

(cl-define-primitive-role 'has-startpoint :inverse 'startpoint-part-of-directed-element
			  :parent 'has-points
			  :inverse-parent 'point-part-of)


(cl-define-primitive-role 'has-endpoint :inverse 'endpoint-part-of-directed-element
			  :parent 'has-points
			  :inverse-parent 'point-part-of)

;----------------------------------------------------------------

;;;
;;;
;;;

(cl-define-primitive-role 'filled-bool :attribute t)
(cl-define-primitive-role 'radius-real :attribute t)
(cl-define-primitive-role 'text-string :attribute t)
(cl-define-primitive-role 'xtrans-integer :attribute t)
(cl-define-primitive-role 'ytrans-integer :attribute t)
(cl-define-primitive-role 'belongs-to-clos-object :attribute t)
 
;----------------------------------------------------------------

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

(cl-define-disjoint-primitive-concept 'info-point '(and 0d basic-thing) 'type)

(cl-define-disjoint-primitive-concept 'g-diamond '(and 2d basic-thing) 'type)

)

;;;
;;;
;;;

(defqualifiedsubrole intersects-objects g-text)

(defqualifiedsubrole touching-objects g-line) 

(defqualifiedsubrole touching-objects g-rectangle)

(defqualifiedsubrole touching-objects g-diamond)

(defqualifiedsubrole touching-objects g-circle)

(def-or-concept g-line g-spline-chain)

(defqualifiedsubrole touching-objects g-line-or-g-spline-chain)

;;;
;;; Diese Konzepte sind wohl von generellem Nutzen:
;;;

(define-concept linker-point
    (and
     info-point
     (exactly 1 point-part-of)))

(define-concept t-linker-point
    (and
     linker-point
     (exactly 1 touching-objects)))

(define-concept linker
    (and 
     g-line-or-g-spline-chain
     (exactly 2 points-related-with)))

(define-concept tt-linker
    (and 
     linker
     (all has-points t-linker-point)))

;;;
;;;
;;;

(def-or-concept g-rectangle g-diamond)

(defqualifiedsubrole touching-objects g-rectangle-or-g-diamond)

(cl-define-concept 'relationship-entity
		   '(and linker
		     (at-least 2 touching-objects)
		     (at-most  2 touching-objects)
		     
		     (at-least 1 intersects-objects-g-text)
		     (at-most  1 intersects-objects-g-text)
		     
		     (at-least 2 touching-objects-g-rectangle-or-g-diamond)
		     (at-most  2 touching-objects-g-rectangle-or-g-diamond)
		     
		     (at-least 1 touching-objects-g-rectangle)
		     (at-most  1 touching-objects-g-rectangle)
		     
		     (at-least 1 touching-objects-g-diamond)
		     (at-most  1 touching-objects-g-diamond)))

(defqualifiedsubrole touching-objects relationship-entity)

(cl-define-concept 'cardinality
		   '(and g-text
		     (at-least 1 intersects-objects)
		     (all intersects-objects relationship-entity)
		     (all text-string
		      (one-of "1" "m" "n"))))

(def-or-concept g-circle g-rectangle)

(cl-define-concept 'attribute-entity
		   '(and linker
		     (at-least 2 touching-objects)
		     (at-most 2 touching-objects)
		     
		     (at-most 0 intersects-objects-g-text)		     
		     
		     (all touching-objects g-circle-or-g-rectangle)
		     
		     (at-least 1 touching-objects-g-rectangle)
		     (at-most 1 touching-objects-g-rectangle)
		     
		     (at-least 1 touching-objects-g-circle)
		     (at-most 1 touching-objects-g-circle)))

(def-or-concept attribute-entity relationship-entity)

(cl-define-concept 'named-region 
		   '(and 2d basic-thing
		     (at-least 1 contains-objects)
		     (at-most 1 contains-objects)
		     
		     (all contains-objects g-text)))

(defqualifiedsubrole linked-over-with g-diamond)    

(def-or-concept g-circle g-diamond)

(cl-define-concept 'entity
		   '(and g-rectangle named-region
		     (at-least 1 touching-objects-relationship-entity)
		     (at-most 1 touching-objects-relationship-entity)
		     
		     (all touching-objects-g-line-or-g-spline-chain
		      attribute-entity-or-relationship-entity)
		     		      
		     (at-least 1 linked-over-with-g-diamond)
		     (at-most 1  linked-over-with-g-diamond)
		     
		     (all linked-over-with g-circle-or-g-diamond)))

(cl-define-concept '1-cardinality
		   '(and cardinality
		     (all text-string (one-of "1"))))

(cl-define-concept 'm-cardinality
		   '(and cardinality
		     (all text-string (one-of "m"))))

(cl-define-concept 'n-cardinality
		   '(and cardinality
		     (all text-string (one-of "n"))))

(cl-define-concept '1-relationship-entity
		   '(and relationship-entity
		     (all intersects-objects 1-cardinality)))

(cl-define-concept 'm-relationship-entity
		   '(and relationship-entity
		     (all intersects-objects m-cardinality)))

(cl-define-concept 'n-relationship-entity
		   '(and relationship-entity
		     (all intersects-objects n-cardinality)))

(defqualifiedsubrole touching-objects 1-relationship-entity)

(defqualifiedsubrole touching-objects m-relationship-entity)

(defqualifiedsubrole touching-objects n-relationship-entity)
  
(cl-define-concept 'relationship
		   '(and g-diamond named-region
		     (at-least 2 linked-over-with)
		     (at-most 2 linked-over-with)
		     
		     (all linked-over-with entity)
		     
		     (at-least 2 touching-objects-g-line-or-g-spline-chain)
		     (at-most 2 touching-objects-g-line-or-g-spline-chain)
		     (all touching-objects-g-line-or-g-spline-chain relationship-entity)
		     
		     (at-most 2 touching-objects-1-relationship-entity)
		     (at-most 1 touching-objects-m-relationship-entity)
		     (at-most 1 touching-objects-n-relationship-entity)))

(cl-define-concept 'attribute 
		   '(and g-circle named-region
		     (at-least 1 linked-over-with)
		     (at-most 1 linked-over-with)
		     (all linked-over-with entity)))
		    

;;;
;;;
;;;

(defconstant +known-concepts+
    '(linker 
      
      tt-linker
      
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
      g-diamond
      composite-thing
      
      info-point
      
      relationship-entity
      cardinality
      attribute-entity
      named-region
      entity
      1-cardinality
      m-cardinality
      n-cardinality
      1-relationship-entity
      m-relationship-entity
      n-relationship-entity
      relationship
      attribute
      ))

(defconstant +library-concepts+
    `(g-diamond
            
      1-cardinality
      m-cardinality
      n-cardinality
      
      relationship
      attribute
      entity
      
      ))
      
(defconstant +known-roles+
    '((             
       spatial-relation ; keine Inverse
       
       in-relation-with-objects ; keine Inverse
       
       disjoint-with ; selbstinvers
      
       touching-objects ; selbstinvers
       
       intersects-objects ; selbstinvers
       intersects-0-objects ; selbstinvers
       intersects-1-objects ; selbstinvers
       intersects-2-objects ; selbstinvers
       
       contains-objects 
       contained-in-objects
       
       directly-contains-objects
       directly-contained-by-object
       
       covers-objects
       covered-by-object       
       
       linked-over-with ; selbstinvers
       
       start-linked-over-with 
       end-linked-over-with
       
       linker-objects
       points-related-with
       
       start-linker-objects
       startpoint-related-with
              
       end-linker-objects
       endpoint-related-with
       
       has-parts
       part-of
       
       has-points
       point-part-of
       
       has-startpoint
       startpoint-part-of-directed-element
       
       has-endpoint
       endpoint-part-of-directed-element       

       ;;; Attribute
       
       belongs-to-clos-object
       filled-bool
       radius-real
       text-string
       xtrans-integer
       ytrans-integer)
      
      (
       intersects-objects-g-text
       touching-objects-g-line
       touching-objects-g-rectangle
       touching-objects-g-diamond
       touching-objects-g-circle)
      
      (touching-objects-g-line-or-g-spline-chain
       touching-objects-g-rectangle-or-g-diamond
       linked-over-with-g-diamond)
                  
      (
       touching-objects-relationship-entity
       touching-objects-1-relationship-entity
       touching-objects-m-relationship-entity
       touching-objects-n-relationship-entity
       )))

	
