(load "agent")

(defpackage :game
	(:documentation "Package for the game")
	(:use :common-lisp :agent))

(in-package :game)

;;;;;;;;;;
;; Game ;;
;;;;;;;;;;
;; This class represents the flow of a single game. A Game has
;; a hearer, a speaker, a context (meanings), a topic (game-meaning)
;; and a delta-value.
(defclass Game ()
	((speaker
		:reader speaker
		:initarg :speaker
		:initform (error "Provide a speaker when intialising a Game")
		:type 'Agent)
	(hearer
		:reader hearer
		:initarg :hearer
		:initform (error "Provide a hearer when intialising a Game")
		:type 'Agent)
	(meanings
		:reader meanings
		:initarg :meanings
		:initform (error "Provide a subset of meanings when initialising a Game"))
	(game-meaning
		:accessor game-meaning
		:initform nil
		:type 'string)
	(delta
		:reader delta
		:initarg :delta
		:initform (error "Provide a delta when intialising a Game")
		:type float)))

(defgeneric play (game)
	(:documentation "Plays a single game"))

(defmethod play :before ((game Game))
	(let ((rs (make-random-state t))
		  (total (list-length (meanings game))))
		(setf (game-meaning game) (nth (random total rs) (meanings game)))))

(defmethod play ((game Game))
	(let* ((speaker (speaker game))
		   (hearer (hearer game))
		   (game-meaning (game-meaning game))
		   (speaker-form (agent::has-form speaker game-meaning))
		   (hearer-meaning (agent::has-meaning hearer speaker-form (meanings game)))
		   (outcome nil))
		(if (not hearer-meaning)
			(progn 
				(agent::receive-meaning hearer game-meaning speaker-form)
				(setf outcome 0))
			(let ((correct (agent::check-meaning speaker game-meaning hearer-meaning)))
				(if correct
					(progn
						(agent::increment-speaker speaker game-meaning speaker-form (delta game))
						(agent::increment-hearer hearer game-meaning speaker-form (delta game))
						(setf outcome 1))
					(progn
						(agent::decrement-speaker speaker game-meaning speaker-form (delta game))
						(agent::decrement-hearer hearer hearer-meaning speaker-form (delta game))
						(setf outcome 0)))))
		(agent::update-age speaker)
		(agent::update-age hearer)
		outcome))