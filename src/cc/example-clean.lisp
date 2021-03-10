;;; -*- Syntax: ANSI-Common-Lisp; Package: krss-classic -*-

(in-package krss-classic)
(classic:cl-startup)

;;; ----------------------------------------------------------------------
;;;
;;; Domain Model
;;;

(define-primitive-concept person classic-thing)

(define-primitive-concept loadable-object classic-thing)
(define-primitive-concept container loadable-object)
(define-primitive-concept passenger (and person loadable-object))

(define-primitive-role has-ship nil)
(define-primitive-concept captain person)

(define-primitive-role has-loadable-object nil)
(define-primitive-role has-captain nil)
(define-primitive-role has-position nil)

(define-primitive-concept pos classic-thing)
(define-primitive-role has-x-coordinate nil)
(define-primitive-role has-y-coordinate nil)

(define-primitive-concept ship
    (and (all has-loadable-object loadable-object)
	 (at-least 1 has-position)
	 (at-most 1 has-position)
	 (all has-position pos)
	 (at-least 1 has-captain)
	 (at-most 1 has-captain)
	 (all has-captain captain)))

(define-concept passenger-ship
    (and ship 
	 (all has-loadable-object passenger)))

(define-concept container-ship
    (and ship
	 (all has-loadable-object container)))

;;; ----------------------------------------------------------------------
;;;
;;; Functional interface to a knowledge base
;;;

(define-accessors captain
    (has-ship captains-ship :single-value-p t))

(define-accessors ship
    (has-loadable-object ship-loadable-objects)
  (has-position ship-position :single-value-p t :error-if-null t)
  (has-captain ship-captain :single-value-p t :error-if-null t))

(define-method initialize-individual :after ((ind ship) &rest initargs)
  (let ((captain (ship-captain ind)))
    (setf (captains-ship captain) ind)
    (setf (ship-position ind) 
      (create-individual 'pos (gensym "POS")
			 'has-x-coordinate 0
			 'has-y-coordinate 0))))

(define-accessors pos
    (has-x-coordinate position-x :single-value-p t :error-if-null t)
  (has-y-coordinate position-y :single-value-p t :error-if-null t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-generic-function ship-position-xy ((ship :classic)))

  (define-generic-function print-information-about-domain-object ((ind :classic)
								(stream :clos))
  (:documentation "Demonstration function for method dispatch."))
)


(define-method ship-position-xy ((ind ship))
  (let ((pos (ship-position ind)))
    (values (position-x pos)
	    (position-y pos))))


;;; ----------------------------------------------------------------------
;;;
;;; Procedural code
;;;

  
(define-method print-information-about-domain-object ((ind ship) stream)
  (format stream "~%Information about the ship ~S." ind)
  (classic:cl-print-object ind))

(define-method print-information-about-domain-object ((ind container-ship) stream)
  (format stream
	  "~%Ah, a ship has been classified to a container ship ~
           and the advantages of CLASSIC over CLOS become clear.")
  (call-next-method))

(define-method print-information-about-domain-object ((ind passenger-ship) stream)
  (format stream
	  "~%Ah, a ship has been classified to a passenger ship ~
           and the advantages of CLASSIC over CLOS become clear.")
  (call-next-method))


;;; ----------------------------------------------------------------------
;;;
;;; Individual creation
;;;
#|
(create-individual 'captain 'c1)
(create-individual 'ship 's1 'has-captain (list @c1))
(print-information-about-domain-object @s1 *standard-output*)
(state (instance s1 (all has-loadable-object passenger)))
(print-information-about-domain-object @s1 *standard-output*)
(ship-position @s1)
(ship-position-xy @s1)
|#

