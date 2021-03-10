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

#| (load-system-definition 'classic-extensions) |#

(define-system knowledge
    (:default-pathname "gened:knowledge;"
	:subsystem t)
  (:serial #| classic-extensions |# "knowledge8"))

(define-system specific-commands
    (:default-pathname "gened:specific-commands;"
	:subsystem t)
  (:serial "speccom"))

(define-system gened-fs
  (:default-pathname "gened:sources;")
  (:serial "gened-packages"
	   "values"

	   "classesxx"
	   "comtable"
	   	   
	   "frame"     

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
	   #| specific-commands |#
	   
	   "concepts"	   
	   "delta2"	   
           "init2"))
