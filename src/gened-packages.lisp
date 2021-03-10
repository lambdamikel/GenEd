;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-User; Base: 10 -*-

(cl:in-package cl-user)

(defpackage geometry
  (:use common-lisp)
  (:export :make-geom-polygon
	   :make-geom-point
	   :make-geom-line
	   
	   :get-points
	   
	   :intersects
	   
	   :intersects-0
	   :intersects-1
	   :intersects-2	   
	   
	   :distance-between
	   :inside
	   
	   :covers
	   :covers-tres
	   :touching-tres
	   :inverse
	   
	   :distance-and-orientation
	   :clear-hashtable
	   :delete-object-from-cache
	   :relate-poly-to-poly-unary-tres

	   :tickval
	   
	   :is-disjoint-with
	   :touches
	   :is-inside
	   :contains
	   :is-covered-by))

(defpackage splines
  (:use common-lisp)
  (:export :make-spline
	   :make-spline-chain-list
	   :make-spline-polygon-list
	   :filter-to-close-together-out))

(defpackage knowledge
  (:use 
   #+:classic classic
   #+:classic krss-classic
   common-lisp)
  (:export 
   
   #| Konzepte |#

   :gened-thing
   :g-circle
   :g-rectangle
   :g-arrow
   :g-line
   :g-chain
   :g-directed-chain
   :g-spline-chain
   :g-directed-spline-chain
   :g-polygon
   :g-text
   :g-spline-polygon
   :g-point
   
   :info-point
   
   :basic-thing
   :composite-thing
   
   #| Rollen |#   
   
   :spatial-relation			; keine Inverse
       
   :in-relation-with-objects		; keine Inverse
   
   :disjoint-with			; selbstinvers
   
   :touching-objects			; selbstinvers
   
   :intersects-objects			; selbstinvers
   :intersects-0-objects		; selbstinvers
   :intersects-1-objects		; selbstinvers
   :intersects-2-objects		; selbstinvers
       
   :contains-objects 
   :contained-in-objects
       
   :directly-contains-objects
   :directly-contained-by-object
   
   :covers-objects
   :covered-by-object       
   
   :linked-over-with			; selbstinvers
   
   :start-linked-over-with 
   :end-linked-over-with
   
   :linker-objects
   :points-related-with
   
   :start-linker-objects
   :startpoint-related-with
   
   :end-linker-objects
   :endpoint-related-with
   
   :has-parts
   :part-of
   
   :has-points
   :point-part-of
   
   :has-startpoint
   :startpoint-part-of-directed-element
   
   :has-endpoint
   :endpoint-part-of-directed-element       
   
       ;;; Attribute
   
   :belongs-to-clos-object
   :filled-bool
   :radius-real
   :text-string
   :xtrans-integer
   :ytrans-integer
   
   #| Bibliothek |#

   :+known-concepts+
   :+library-concepts+
   :+known-roles+))

(defpackage gened
  (:use clim-lisp
	clim
	splines
	knowledge
	#+:classic classic
	#+:classic krss-classic
	geometry)
  
  #+:classic 
  (:shadowing-import-from classic
   pathname)
  ;(:shadowing-import-from cl-user
  ; output-record
  )
    
