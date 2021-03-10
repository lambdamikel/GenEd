;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-User; Base: 10 -*-

(cl:in-package cl-user)

(define-system geometry
    (:default-pathname "gened:geometry;"
	:subsystem t)
  (:serial "newgeo13"))

(define-system splines
    (:default-pathname "gened:splines;"
	:subsystem t)
  (:serial "spline3"))

(define-system knowledge
    (:default-pathname "gened:knowledge;"
	:subsystem t)
  (:serial "knowledge7"))

(define-system mac-gened
  (:default-pathname "gened:sources;")
  (:serial "gened-packages"
	   "values"

	   "classesxx"
	   "comtable"
	   "mac-frame"     

	   (:parallel splines geometry)

	   "helpaux"
	   "polyrep"	   
	   "transfor"
	   "draw"
	   "creator"
	   "cluster"
	   "handles"
	   "inout"
	   "copy"
	   "spatial5"
	   "inspect"	   
	   "delete"
	   "undo"
	   "main"	   
	   "rel-copy2"
	   "interface-to-classic3"

	   knowledge
	   
	   "concepts"	   
	   "delta2"	   
           "mac-init"))
