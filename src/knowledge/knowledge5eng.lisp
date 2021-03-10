;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: Knowledge; Base: 10 -*-

(in-package knowledge)


(cl-reader-init)

(cl-clear-kb)
(cl-startup)

(setf *krss-verbose* nil)

(cl-set-classic-warn-mode t)

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

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  
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

;----------------------------------------------------------------

;;; selbstinverse Basisrelationen

(cl-define-primitive-role 'spatial-relation :inverse 'spatial-relation)

(cl-define-primitive-role 'in-relation-with-objects :inverse 'in-relation-with-objects 
			  :parent 'spatial-relation
			  :inverse-parent 'spatial-relation)

(cl-define-primitive-role 'disjoint-with :inverse 'disjoint-with			  
			  :parent 'spatial-relation
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

)
;;;
;;;
;;;

(defqualifiedsubrole has-parts g-rectangle)

(defqualifiedsubrole has-parts directed-element)

(defqualifiedsubrole touching-objects directed-element)

;;;
;;; Diese Konzepte sind wohl von generellem Nutzen:
;;;

(define-concept only-touching-objects
    (and 
     basic-thing					      
     (none contained-in-objects)				   
     (some touching-objects)					      
     (none has-parts)))

(define-concept linker-point
    (and
     info-point
     (exactly 1 point-part-of)))

(define-concept t-linker-point
    (and
     linker-point
     (at-least 1 touching-objects))) ;eigentlich (exactly 1 touching-objects) -> Error in Splines!

(define-concept c-linker-point
    (and
     linker-point
     (exactly 1 covered-by-object)))

(define-concept i-linker-point
    (and 
     linker-point
     (exactly 1 directly-contained-by-object)))

(define-concept linker
    (and 
     directed-element
     (exactly 2 points-related-with)))

(define-concept tt-linker
    (and 
     linker
     (all has-points t-linker-point)))

(define-concept ii-linker
    (and 
     linker
     (all has-points i-linker-point)))

(define-concept it-linker
    (and 
     linker
     (all has-startpoint i-linker-point)
     (all has-endpoint t-linker-point)))

(define-concept ti-linker
    (and
     linker
     (all has-startpoint t-linker-point)
     (all has-endpoint i-linker-point)))

(define-concept ct-linker
    (and 
     linker
     (all has-startpoint c-linker-point)
     (all has-endpoint t-linker-point)))

(define-concept tc-linker
    (and 
     linker
     (all has-startpoint t-linker-point)
     (all has-endpoint c-linker-point)))

(define-concept ci-linker
    (and 
     linker
     (all has-startpoint c-linker-point)
     (all has-endpoint i-linker-point)))


(define-concept ic-linker
    (and 
     linker
     (all has-startpoint i-linker-point)
     (all has-endpoint c-linker-point)))

;;;;
;;;;
;;;;

(defun string-is-integer-p (string)
  (integerp
   (read-from-string string)))

(cl-define-concept 'capacity-label 
		   '(and
		     g-text
		     (at-least 1 intersects-objects)
		     (all text-string
		      (and (test-h string-is-integer-p)))))


(defqualifiedsubrole intersects-objects capacity-label)

;;;
;;;
;;;

(define-concept place-or-transition
    (and 
     only-touching-objects
     (all touching-objects-directed-element tt-linker)))

(define-concept place?
    (and 
     g-circle
     place-or-transition))

(define-concept transition?
    (and 
     g-rectangle
     place-or-transition))

;;;
;;;
;;;

(define-concept place
    (and 
     place?
     (all linked-over-with transition?)))

(defqualifiedsubrole has-parts place)

(define-concept conflict-place
    (and 
     place
     (at-least 2 end-linked-over-with)))

(define-concept start-place
    (and 
     place
     (none start-linked-over-with)))

(define-concept end-place
    (and
     place
     (none end-linked-over-with)))

(define-concept normal-place
    (and 
     place
     (some start-linked-over-with)
     (some end-linked-over-with)))

(define-concept place-with-capacity 
    (and
     place
     (exactly 1 intersects-objects-capacity-label)))

(cl-define-concept 'token
    '(and
      g-circle
      (all contained-in-objects place)
      (fills filled-bool t)
      (all radius-real
       (and number
	(min 2.0)
	(max 10.0)))))

(define-concept place-with-tokens
    (and 
     place
     (some contains-objects)
     (all contains-objects token)))


(defqualifiedsubrole has-parts place-with-tokens)

(defun overfilled-place? (ind)
  (let* ((tokens-rolle (cl-named-role 'contains-objects))
	 (tokens (length (cl-fillers ind tokens-rolle)))
	 (label-rolle (cl-named-role 'intersects-objects-capacity-label))
	 (text-ind (first (cl-fillers ind label-rolle)))
	 (number (read-from-string
		  (first 
		   (cl-fillers 
		    text-ind
		    (cl-named-role 'text-string))))))
    (> tokens number)))		      


(defun label-kleiner-1? (ind)
  (let* ((rolle (cl-named-role 'intersects-objects-capacity-label))
	 (text-ind (first (cl-fillers ind rolle)))
	 
	 (kap
	  (if text-ind
	      (read-from-string
	       (first 
		(cl-fillers 
		 text-ind
		 (cl-named-role 'text-string))))
	    1)))
    (<= kap 1)))

(cl-define-concept 'overfilled-place!!!
    '(and
     place-with-tokens
     place-with-capacity
     (test-c cl-test-closed-roles? (contains-objects intersects-objects-capacity-label))
     (test-c overfilled-place?)))

(cl-define-concept 'c/e-place
		   '(and
		     place
		     (test-c cl-test-closed-roles? 
		      (intersects-objects-capacity-label))
		     (test-c label-kleiner-1?)))

;;;
;;;
;;;

(define-concept transition
    (and 
     transition?
     (at-least 2 linked-over-with)
     (at-least 1 start-linker-objects)
     (at-least 1 end-linker-objects)
     (all linked-over-with place?)))

(define-concept edge
    (and 
     tt-linker
     (all touching-objects place-or-transition)))

(define-concept edge-with-capacity
    (and 
     edge
     (exactly 1 intersects-objects-capacity-label)))


(cl-define-concept 'c/e-edge
		   '(and
		     edge
		     (test-c cl-test-closed-roles? (intersects-objects-capacity-label))
		     (test-c label-kleiner-1?)))

;;;
;;;
;;;

(defun genug-input-tokens? (ind)
  (let* ((end-linker (cl-named-role 'end-linker-objects))
	 (flag nil))
	 
    (or 
     (every #'(lambda (end-linker)
		(let* ((text-ind
		       (first 
			(cl-fillers
			 end-linker
			 (cl-named-role 'intersects-objects-capacity-label))))
		      
		      (place 
		       (first 
			(cl-fillers
			 end-linker
			 (cl-named-role 'startpoint-related-with))))
		      
		       (kap
			(if text-ind
			    (read-from-string
			     (first 
			      (cl-fillers 
			       text-ind
			       (cl-named-role 'text-string))))
			  1))		; Kante hat sonst Kap. 1 (wie bei BE-Neten)
		      
		       (tokens
			(length
			 (cl-fillers 
			  place
			  (cl-named-role 'contains-objects)))))
		  
		  (if (and tokens kap)
		      (not (minusp (- tokens kap)))
		    (progn 
		      (setf flag '?)
		      nil))))
	    (cl-fillers ind end-linker))
     flag)))
     


(defun genug-output-tokens? (ind)
  (let* ((start-linker (cl-named-role 'start-linker-objects))
	 (flag nil))
	     
    (or 
     (every #'(lambda (start-linker)
	      (let* ((text-ind
		      (first 
		       (cl-fillers
			start-linker
			(cl-named-role 'intersects-objects-capacity-label))))
		     
		     (place 
		      (first 
		       (cl-fillers
			start-linker
			(cl-named-role 'endpoint-related-with))))
		     
		     (kap
		      (if text-ind
			  (read-from-string
			   (first 
			    (cl-fillers 
			     text-ind
			     (cl-named-role 'text-string))))
			1))
			       
		     (tokens
		      (length
		       (cl-fillers 
			place
			(cl-named-role 'contains-objects))))
		     
		     (placen-text-ind
		      (first 
		       (cl-fillers
			place
			(cl-named-role 'intersects-objects-capacity-label))))
		     
		     (placen-kap
		      (if placen-text-ind
			  (read-from-string
			   (first 
			    (cl-fillers 
			     placen-text-ind
			     (cl-named-role 'text-string))))
			10000)))		; kein Label -> Kap. unbegrenzt ! (w)
			
		(if (and placen-kap kap tokens)		    
		    (<= (+ tokens kap) placen-kap)
		  (progn 
		    (setf flag '?)
		    nil))))

	    (cl-fillers ind start-linker))
     flag)))


(cl-define-concept 'l-activated-transition 
		   '(and
		     transition
		     (all start-linked-over-with 
		       place-with-tokens)
		     (all end-linker-objects edge)
		     (test-c cl-test-closed-roles? (end-linker-objects start-linked-over-with))
		     (test-c genug-input-tokens?)))


(cl-define-concept 'r-activated-transition 
		   '(and
		     transition
		     (all end-linked-over-with 
		      place)
		     (all start-linker-objects edge)		     
		     (test-c cl-test-closed-roles? (start-linker-objects end-linked-over-with))
		     (test-c genug-output-tokens?)))


(cl-define-concept 'activated-transition
		   '(and
		     l-activated-transition
		     r-activated-transition))

;;;
;;;
;;;

(define-concept petri-net
    (and
     composite-thing
     (at-least 5 has-parts)	  
     (some has-parts-place)
     (all has-parts-g-rectangle transition)
     (all has-parts-directed-element edge)))

(define-concept p/t-petri-net
    (and
     petri-net
     (some has-parts-place-with-tokens)))

(define-concept c/e-petri-net
    (and
     p/t-petri-net
     (all has-parts-place c/e-place)
     (all has-parts-directed-element c/e-edge)))


;;;
;;;
;;;

(defconstant +known-concepts+
    '(linker 
      
      tt-linker
      ii-linker
      ti-linker
      it-linker
      ci-linker
      ic-linker
      tc-linker
      ct-linker
      
      place transition edge

      l-activated-transition
      r-activated-transition
      activated-transition
      
      edge-with-capacity
      token
      conflict-place

      overfilled-place!!!
      start-place end-place normal-place
      c/e-place
      c/e-edge
      
      capacity-label
      place-with-capacity
      place-with-tokens
      petri-net
      p/t-petri-net
      c/e-petri-net
      
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
      composite-thing
      
      info-point
      ))

(defconstant +library-concepts+
    `(
      capacity-label
      token
      
      place
      place-with-tokens
      place-with-capacity            
      
      transition))
      
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
      
      (has-parts-g-rectangle
       has-parts-g-rectangle-inverse
       
       has-parts-directed-element
       has-parts-directed-element-inverse
       
       touching-objects-directed-element
       touching-objects-directed-element-inverse
       
       intersects-objects-capacity-label
       intersects-objects-capacity-label-inverse
       
       has-parts-place
       has-parts-place-inverse 
       
       has-parts-place-with-tokens
       has-parts-place-with-tokens-inverse)))
	
