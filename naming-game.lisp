(load "world")

(defpackage :AIPP
	(:documentation "Package for the course AIPP")
	(:use :common-lisp :world))

(in-package :AIPP)

;;;;;;;;;;;;;;
;; Run File ;;
;;;;;;;;;;;;;;
;; An instance of World is created, with all required parameters, and a number of games is played.

(defparameter w (world::make-instance 'world::World 
									  :agent-count 5
	                                  :meaning-count 5
	                                  :agent-flux 0
	                                  :meaning-flux 0
	                                  :delta 0.1))
(world::play-n-games w 5)

