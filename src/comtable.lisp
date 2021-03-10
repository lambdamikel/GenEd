;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GENED; Base: 10 -*-

(in-package gened)

(define-command-table concept-table
    :menu (("Select Concept-Visualisation" :command (com-gened-select-concept-visualisation))
	   
	   ("divide1" :divider nil)

	   ("Assign Concept To Object" :command (com-gened-assign-to-concept))
	   ("Remove Concept From Object" :command (com-gened-remove-concept))
	   
	   ("divide2" :divider nil)
	   
	   ("Only Basic Concept For Object" :command (com-gened-only-basic-concept))
	   ("Only Basic Concept For All Objects" :command (com-gened-only-basic-concept-all-objects))	   
	   ("Only One Concept For Object" :command (com-gened-only-one-concept))
	   	   
	   ("divide3" :divider nil)
	   
	   ("Store Object Into Library" :command (com-gened-store-into-library))	   
	   ("Remove Object From Library" :command (com-gened-remove-from-library))
	   
	   ("divide4" :divider nil)
	   
	   ("Clear Library" :command (com-gened-clear-library))))

(define-command-table reasoning-table
    :menu (("Show Topological Relations" :command (com-gened-show-relations))
	   ("Calculate Topological Relations" :command (com-gened-calc-relations))
	   
	   ("divide1" :divider nil)
	   
	   ("Close All Roles For All Objects" :command (com-gened-close-all-roles))
	   
	   ("divide2" :divider nil)
	   
	   ("Open All Roles For All Objects" :command (com-gened-open-all-roles))
	   
	   ("divide3" :divider nil)
	   
	   ("Clear Knowledge-Base" :command (com-gened-clear-kb))
	   ("Classic, Classify!" :command (com-gened-classify))))

(define-command-table file-table
    :menu (("Load Scene" :command (com-gened-load-scene))
	   ("Save Scene" :command (com-gened-save-scene))
	   ("Print Scene" :command (com-gened-print-scene))
	   ("Save Scene As Postscript" :command (com-gened-save-postscript))
	   ("divide1" :divider nil)
	   ("Load Object" :command (com-gened-load-object))
	   ("Save Object" :command (com-gened-save-object))
	   ("divide2" :divider nil)
	   ("Load Library" :command (com-gened-load-library))
	   ("Save Library" :command (com-gened-save-library))
	   ("divide3" :divider nil)
	   ("New Scene" :command (com-gened-new))
	   ("Exit Gened" :command (com-gened-exit))))

(define-command-table manipulate-table
    :menu (("UNDO NOTHING" :command (com-gened-undo))
	   ("divide1" :divider nil)
	   ("Move Object" :command (com-gened-move-object))
	   ("Scale Object" :command (com-gened-scale-object))
	   ("Rotate Object" :command (com-gened-rotate-object))
	   ("divide2" :divider nil)
	   ("Delete Object" :command (com-gened-delete-object))
	   ("Copy Object" :command (com-gened-copy-object))
	   ("divide3" :divider nil)
	   ("Inspect Object" :command (com-gened-inspect-object))))
	   

(define-command-table tool-table
    :menu (("Compose Composite Object" :command (com-gened-build-cluster))
	   ("Decompose Composite Object" :command (com-gened-atomize-cluster))
	   ("divide1" :divider nil)
	   ("Move Object-Handle" :command (com-gened-move-object-handle))
	   ("Fix Object-Handle" :command (com-gened-fix-handle))
	   ("Free Object-Handle" :command (com-gened-free-handle))
	   ("divide2" :divider nil)
	   ("Hide Object" :command (com-gened-hide-object))
	   ("Show Object" :command (com-gened-show-object))
	   ("divide3" :divider nil)
	   ("Bring Object To Front" :command (com-gened-to-front))
	   ("Bring Object To Back" :command (com-gened-to-back))
	   ("divide4" :divider nil)
	   ("Find Object" :command (com-gened-find-object))
	   ("Unmark All Objects" :command (com-gened-unmark-all-objects))
	   ("Clear Info-Window" :command (clear-info-pane))
	   ("Redraw All" :command (refresh))))

(define-command-table spatial-table
    :menu (("Show Touching Objects" :command (com-gened-show-touching))
	   ("divide1" :divider nil)
	   ("Show Intersecting Objects" :command (com-gened-show-intersecting))
	   ("Show Intersecting Objects, DIM = 0" :command (com-gened-show-intersecting-dim=0))
	   ("Show Intersecting Objects, DIM = 1" :command (com-gened-show-intersecting-dim=1))
	   ("Show Intersecting Objects, DIM = 2" :command (com-gened-show-intersecting-dim=2))	   
	   ("divide2" :divider nil)
	   ("Show Contained In Objects" :command (com-gened-show-contained))
	   ("Show Containing Objects" :command (com-gened-show-containing))
	   ("divide3" :divider nil)
	   ("Show Directly Contained In Object" :command (com-gened-show-directly-contained))
	   ("Show Directly Containing Objects" :command (com-gened-show-directly-containing))
	   ("divide4" :divider nil)
	   ("Show Covered By Objects" :command (com-gened-show-covered))
	   ("Show Covering Objects" :command (com-gened-show-covering))
	   ("divide5" :divider nil)
	   ("Show Disjoint Objects" :command (com-gened-show-disjoint))
	   ("Show Related Objects" :command (com-gened-show-related))
	   ("divide6" :divider nil)
	   ("Show Linker Objects" :command (com-gened-show-linker))
	   ("Show Start-Linker Objects" :command (com-gened-show-start-linker))
	   ("Show End-Linker Objects" :command (com-gened-show-end-linker))	   
	   ("divide7" :divider nil)
	   ("Show Linked Objects" :command (com-gened-show-linked))
	   ("Show Start-Linked Object" :command (com-gened-show-start-linked))
	   ("Show End-Linked Object" :command (com-gened-show-end-linked))
	   ("divide8" :divider nil)
	   ("Show Opposite Linked" :command (com-gened-show-opposite))))
	    
