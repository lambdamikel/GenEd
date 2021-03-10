;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-User; Base: 10 -*-

(cl:in-package cl-user)

(require "clim") 

(load "~/define-system.lisp")

(setf (logical-pathname-translations "base")
      '(("**;*.*" "~/**/*.*")))

(setf (logical-pathname-translations "gened")
      (list  '("sources;**;*.*" "base:gened;**;*.*")
             '("**;*.*" "base:gened;**;*.*")))
             

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

(define-system gened
  (:default-pathname "gened:sources;")
  (:serial "gened-packages"
	   "values"

	   "classesxx"
	   "comtable"
	   "frame2"     

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
	   #+:classic
           "interface-to-classic3"

	   #+:classic 
           knowledge
	   
	   "concepts"	   
	   "delta2"	   
           "init2"))


(load-system 'gened
             :force-p t)

(princ "(gened::gened)")


