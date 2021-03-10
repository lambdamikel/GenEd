;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

;;;
;;;
;;;

(defconstant +objects-dir+ "gened:objects;")

(defconstant +scenes-dir+ "gened:scenes;")

(defconstant +libraries-dir+ "gened:libraries;")

(defconstant +prints-dir+ "gened:prints;")

;;;
;;;
;;;

(defconstant +labels-text-style+
    (parse-text-style '(:sans-serif nil :tiny)))

(defconstant +labels-text-style2+
    (parse-text-style '(:sans-serif :italic :tiny)))

(defconstant +labels-text-height+ 10)

;;;
;;; 
;;;

(defparameter *handles-symbol-list* '(normal start end))
(defparameter *bounding-boxes* t)
(defparameter *origins* t)
(defparameter *concept-labels* '(ids))
(defparameter *info* t)
(defparameter *global-scaling* 1)
(defparameter *classic-warnings* t)
(defparameter *pretty-hl* t)
 

(defparameter *global-display-options* '(visible primitive composite))
(defparameter *global-filled* t)
(defparameter *global-border* t)

(defparameter *current-file* "NONAME")

;;;
;;;
;;;

(defparameter *handle-space* 3)
(defparameter *org-space* 2)

(defparameter *id-number* 1)

;;; Undo-Buffer

(defconstant +buffer-size+ 10)

;;; Touching-Treshold und Granularitaet der Circle-Poly.Repr.

(defconstant +tres-touch+ 4.0)

(defconstant +circle-granularity+ 20)

;;; best. Strich-Muster

(defconstant +line-pattern-list+
    `((:solid nil)
      (:pattern1 (1 1))
      (:pattern2 (2 2))
      (:pattern3 (4 1))))

;;; Bounding-Rectangle-Constants

(defconstant +br-space+ 2)
(defconstant +br-fix-inc+ 5)
(defconstant +br-ink+ +black+)
(defconstant +br-line-thickness+ 1)
(defconstant +br-line-dashes+ '(1 3))
(defconstant +br-line-dashes2+ '(1 6))

(defconstant +br-highlight-thickness+ 2)

(defconstant +printing-inc+ 0.3)

;;; Origin-Muster  

(defconstant +origin-dashes+ '(1 1))

;;; 

(defconstant +mr-line-thickness+ 1)

;;; 

(defconstant +dash-pattern+ '(1 1))
