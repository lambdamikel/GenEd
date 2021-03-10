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
			       (specialized-concept t) ;;; funktioniert eigentlich nur bei nil korrekt! (default t ?)
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
						role)))

  `(progn
     (cl-define-primitive-role ',name :inverse ',inv-name
                               :parent ',parent
                               :inverse-parent ',inverse-parent)
     
     
     (cl-add-filler-rule ',(intern (concatenate 'string (string name)
                                                "-FILLER-RULE"))
			 ,(if specialized-concept 
			      `(cl-named-concept ',qualification)
			    '(cl-named-concept 'classic-thing))
                         (cl-named-role ',name) ; Rolle, die gefuellt wird
                         #'(lambda (ind role)
			     (remove-if-not #'(lambda (ind)
						(cl-instance? ind 
							      (cl-named-concept ',qualification)))
					    (cl-fillers ind (cl-named-role ',parent))))

                         :filter
                         '(and 
			   (at-least 1 ,parent)
                           (test-c cl-test-closed-roles? (,role))))))

#|

;;; Haarslev: 

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

|#

;;;
;;;
;;;
;----------------------------------------------------------------


(cl-define-primitive-role 'spatial-relation)

(cl-define-primitive-role 'in-relation-with-objects :inverse 'in-relation-with-objects 
			  :parent 'spatial-relation)			 

(cl-define-primitive-role 'disjoint-with :inverse 'disjoint-with			  
			  :parent 'spatial-relation)

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
;;; Fuer Directed-Elements:
;;;

(cl-define-primitive-role 'has-points 
			  :parent 'has-parts 
			  :inverse 'point-part-of
			  :inverse-parent 'part-of)

(cl-define-primitive-role 'has-startpoint 
			  :parent 'has-points
			  :inverse 'startpoint-part-of-directed-element
			  :inverse-parent 'point-part-of)


(cl-define-primitive-role 'has-endpoint
			  :parent 'has-points
			  :inverse 'endpoint-part-of-directed-element
			  :inverse-parent 'point-part-of)

;;;
;;;
;;;

(cl-define-primitive-role 'filled-bool :attribute t)
(cl-define-primitive-role 'radius-real :attribute t)
(cl-define-primitive-role 'text-string :attribute t)
(cl-define-primitive-role 'xtrans-integer :attribute t)
(cl-define-primitive-role 'ytrans-integer :attribute t)

(cl-define-primitive-role 'belongs-to-clos-object :attribute t)
 
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

;;;
;;;
;;;

(defqualifiedsubrole linked-over-with g-circle
  :parent linked-over-with :inverse-parent linked-over-with)

(defqualifiedsubrole linked-over-with g-rectangle
  :parent linked-over-with :inverse-parent linked-over-with)

;;; diese Rolle laesst sich nur definieren, wenn die Regel auf gened-thing feuert,
;;; da stelle noch gar nicht definiert ist! ansonsten muesste sie spaeter nach stelle definiert werden!
;;; allgemeiner Regeltrigger -> weniger DAG-Abhaengigkeiten
;;; deswegen: specialized-concept nil (default ist t)

(defqualifiedsubrole has-parts stelle
  :parent has-parts :inverse-parent part-of :specialized-concept nil)

;;;
;;; das gleiche gilt fuer diese
;;;

(defqualifiedsubrole has-parts stelle-mit-marken
  :parent has-parts :inverse-parent part-of :specialized-concept nil)

;;;
;;; diese sind unkritisch, da die Konzepte bereits definiert sind
;;;

(defqualifiedsubrole has-parts g-rectangle
  :parent has-parts :inverse-parent part-of)

(defqualifiedsubrole has-parts directed-element
  :parent has-parts :inverse-parent part-of)

(defqualifiedsubrole touching-objects directed-element
  :parent touching-objects :inverse-parent touching-objects)

;;;
;;; Diese Konzepte sind wohl von generellem Nutzen:
;;;

(define-concept nur-beruehrende-objekte
    (and 
     basic-thing					      
     (none contained-in-objects)				   
     (some touching-objects)					      
     (none has-parts)))

(define-concept verbinder-punkt
    (and
     info-point
     (exactly 1 point-part-of)))

(define-concept t-verbinder-punkt
    (and
     verbinder-punkt
     (at-least 1 touching-objects))) ;eigentlich (exactly 1 touching-objects) -> Error in Splines!

(define-concept c-verbinder-punkt
    (and
     verbinder-punkt
     (exactly 1 covered-by-object)))

(define-concept i-verbinder-punkt
    (and 
     verbinder-punkt
     (exactly 1 directly-contained-by-object)))

(define-concept verbinder
    (and 
     directed-element
     (exactly 2 points-related-with)))

(define-concept tt-verbinder
    (and 
     verbinder
     (all has-points t-verbinder-punkt)))

(define-concept ii-verbinder
    (and 
     verbinder
     (all has-points i-verbinder-punkt)))

(define-concept it-verbinder
    (and 
     verbinder
     (all has-startpoint i-verbinder-punkt)
     (all has-endpoint t-verbinder-punkt)))

(define-concept ti-verbinder
    (and
     verbinder
     (all has-startpoint t-verbinder-punkt)
     (all has-endpoint i-verbinder-punkt)))

(define-concept ct-verbinder
    (and 
     verbinder
     (all has-startpoint c-verbinder-punkt)
     (all has-endpoint t-verbinder-punkt)))

(define-concept tc-verbinder
    (and 
     verbinder
     (all has-startpoint t-verbinder-punkt)
     (all has-endpoint c-verbinder-punkt)))

(define-concept ci-verbinder
    (and 
     verbinder
     (all has-startpoint c-verbinder-punkt)
     (all has-endpoint i-verbinder-punkt)))


(define-concept ic-verbinder
    (and 
     verbinder
     (all has-startpoint i-verbinder-punkt)
     (all has-endpoint c-verbinder-punkt)))

;;;;
;;;;
;;;;

(defun string-is-integer-p (string)
  (integerp
   (read-from-string string)))

(cl-define-concept 'kapazitaets-label 
		   '(and
		     g-text
		     (at-least 1 intersects-objects)
		     (all text-string
		      (and (test-h string-is-integer-p)))))


;;; diese Regel feuert auf Konzept, das bereits definiert ist und wird auch
;;; nicht vorher verwendet -> specialized-concept t (default)

(defqualifiedsubrole intersects-objects kapazitaets-label
  #| :inv-name intersects-objects-kapazitaets-label |#
  :parent intersects-objects :inverse-parent intersects-objects)

;;;
;;;
;;;

(define-concept stelle-oder-transition
    (and 
     nur-beruehrende-objekte
     (all touching-objects-directed-element tt-verbinder)))

(define-concept stelle?
    (and 
     g-circle
     stelle-oder-transition))

(define-concept transition?
    (and 
     g-rectangle
     stelle-oder-transition))

;;;
;;;
;;;

(define-concept stelle
    (and 
     stelle?
     (all linked-over-with transition?)))

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
     (exactly 1 intersects-objects-kapazitaets-label)))

(cl-define-concept 'marke
    '(and
      g-circle
      (all contained-in-objects stelle)
      (fills filled-bool t)
      (all radius-real
       (and number
	(min 2.0)
	(max 10.0)))))

(define-concept stelle-mit-marken
    (and 
     stelle
     (some contains-objects)
     (all contains-objects marke)))


(defun ueberlaufene-stelle? (ind)
  (let* ((marken-rolle (cl-named-role 'contains-objects))
	 (marken (length (cl-fillers ind marken-rolle)))
	 (label-rolle (cl-named-role 'intersects-objects-kapazitaets-label))
	 (text-ind (first (cl-fillers ind label-rolle)))
	 (number (read-from-string
		  (first 
		   (cl-fillers 
		    text-ind
		    (cl-named-role 'text-string))))))
    (> marken number)))		      


(defun label-kleiner-1? (ind)
  (let* ((rolle (cl-named-role 'intersects-objects-kapazitaets-label))
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

(cl-define-concept 'ueberlaufene-stelle!!!
    '(and
     stelle-mit-marken
     stelle-mit-kapazitaet
     (test-c cl-test-closed-roles? (contains-objects intersects-objects-kapazitaets-label))
     (test-c ueberlaufene-stelle?)))

(cl-define-concept 'b/e-stelle
		   '(and
		     stelle
		     (test-c cl-test-closed-roles? 
		      (intersects-objects-kapazitaets-label))
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
     (all linked-over-with stelle?)))

(define-concept netz-kante
    (and 
     tt-verbinder
     (all touching-objects stelle-oder-transition)))

(define-concept netz-kante-mit-kapazitaet
    (and 
     netz-kante
     (exactly 1 intersects-objects-kapazitaets-label)))


(cl-define-concept 'b/e-netz-kante
		   '(and
		     netz-kante
		     (test-c cl-test-closed-roles? (intersects-objects-kapazitaets-label))
		     (test-c label-kleiner-1?)))

;;;
;;;
;;;

(defun genug-input-marken? (ind)
  (let* ((end-linker (cl-named-role 'end-linker-objects))
	 (flag nil))
	 
    (or 
     (every #'(lambda (end-linker)
		(let* ((text-ind
		       (first 
			(cl-fillers
			 end-linker
			 (cl-named-role 'intersects-objects-kapazitaets-label))))
		      
		      (stelle 
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
			  1))		; Kante hat sonst Kap. 1 (wie bei BE-Netzen)
		      
		       (marken
			(length
			 (cl-fillers 
			  stelle
			  (cl-named-role 'contains-objects)))))
		  
		  (if (and marken kap)
		      (not (minusp (- marken kap)))
		    (progn 
		      (setf flag '?)
		      nil))))
	    (cl-fillers ind end-linker))
     flag)))
     


(defun genug-output-marken? (ind)
  (let* ((start-linker (cl-named-role 'start-linker-objects))
	 (flag nil))
	     
    (or 
     (every #'(lambda (start-linker)
	      (let* ((text-ind
		      (first 
		       (cl-fillers
			start-linker
			(cl-named-role 'intersects-objects-kapazitaets-label))))
		     
		     (stelle 
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
			       
		     (marken
		      (length
		       (cl-fillers 
			stelle
			(cl-named-role 'contains-objects))))
		     
		     (stellen-text-ind
		      (first 
		       (cl-fillers
			stelle
			(cl-named-role 'intersects-objects-kapazitaets-label))))
		     
		     (stellen-kap
		      (if stellen-text-ind
			  (read-from-string
			   (first 
			    (cl-fillers 
			     stellen-text-ind
			     (cl-named-role 'text-string))))
			10000)))		; kein Label -> Kap. unbegrenzt ! (w)
			
		(if (and stellen-kap kap marken)		    
		    (<= (+ marken kap) stellen-kap)
		  (progn 
		    (setf flag '?)
		    nil))))

	    (cl-fillers ind start-linker))
     flag)))


(cl-define-concept 'l-aktive-transition 
		   '(and
		     transition
		     (all start-linked-over-with 
		       stelle-mit-marken)
		     (all end-linker-objects netz-kante)
		     (test-c cl-test-closed-roles? (end-linker-objects start-linked-over-with))
		     (test-c genug-input-marken?)))


(cl-define-concept 'r-aktive-transition 
		   '(and
		     transition
		     (all end-linked-over-with 
		      stelle)
		     (all start-linker-objects netz-kante)		     
		     (test-c cl-test-closed-roles? (start-linker-objects end-linked-over-with))
		     (test-c genug-output-marken?)))


(cl-define-concept 'aktive-transition
		   '(and
		     l-aktive-transition
		     r-aktive-transition))

;;;
;;;
;;;

(define-concept petri-netz
    (and
     composite-thing
     (at-least 5 has-parts)	  
     (some has-parts-stelle)
     (all has-parts-g-rectangle transition)
     (all has-parts-directed-element netz-kante)))

(define-concept s/t-petri-netz
    (and
     petri-netz
     (some has-parts-stelle-mit-marken)))

(define-concept b/e-petri-netz
    (and
     s/t-petri-netz
     (all has-parts-stelle b/e-stelle)
     (all has-parts-directed-element b/e-netz-kante)))


;;;
;;;
;;;

(defconstant +known-concepts+
    '(verbinder 
      
      tt-verbinder
      ii-verbinder
      ti-verbinder
      it-verbinder
      ci-verbinder
      ic-verbinder
      tc-verbinder
      ct-verbinder
      
      stelle transition netz-kante

      l-aktive-transition
      r-aktive-transition
      aktive-transition
      
      netz-kante-mit-kapazitaet
      marke
      konflikt-stelle

      ueberlaufene-stelle!!!
      start-stelle end-stelle normale-stelle
      b/e-stelle
      b/e-netz-kante
      
      kapazitaets-label
      stelle-mit-kapazitaet
      stelle-mit-marken
      petri-netz
      s/t-petri-netz
      b/e-petri-netz
      
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
    `(verbinder

      kapazitaets-label
      stelle
      start-stelle
      normale-stelle
      end-stelle
      stelle-mit-marken
      stelle-mit-kapazitaet
      
      marke
      transition))
      
(defconstant +known-roles+
    '((
       
       belongs-to-clos-object
       
       spatial-relation
       
       in-relation-with-objects
       disjoint-with
      
       touching-objects
       
       intersects-objects
       intersects-0-objects
       intersects-1-objects
       intersects-2-objects
       
       contains-objects
       contained-in-objects
       directly-contains-objects
       directly-contained-by-object
       covers-objects
       covered-by-object
       
       
       linked-over-with      
       start-linked-over-with
       end-linked-over-with
       
       points-related-with
       startpoint-related-with
       endpoint-related-with
       
       linker-objects
       start-linker-objects
       end-linker-objects
       
       has-points
       
       has-startpoint
       has-endpoint
       
       has-parts
       part-of
      
       point-part-of
       startpoint-part-of-directed-element
       endpoint-part-of-directed-element
       
       filled-bool
       radius-real
       text-string
       xtrans-integer
       ytrans-integer)
      
      (
       has-parts-stelle
       has-parts-g-rectangle
       has-parts-directed-element
       has-parts-stelle-mit-marken
       
       touching-objects-directed-element-inverse
       touching-objects-directed-element
       intersects-objects-kapazitaets-label
      )))
	
