(defpackage :queue
	(:documentation "Package for a simple queue")
	(:use :common-lisp))

(in-package :queue)

;;;;;;;;;;;;;;;;
;; Game Queue ;;
;;;;;;;;;;;;;;;;
(defclass Queue ()
	((queue-size
		:reader queue-size
		:initarg :queue-size
		:initform (error "Provide a queue-size when initialising a Queue")
		:type number)
	(elements
		:accessor elements)
	(put-ptr
		:accessor put-ptr
		:initform 0
		:type number)
	(get-ptr
		:accessor get-ptr
		:initform 0
		:type number)))

(defmethod initialize-instance :after ((q Queue) &rest initargs)
	(with-slots (queue-size elements) q
		(setf elements (make-array queue-size :initial-element nil))))

(defgeneric enqueue (elm q)
	(:documentation "Enqueue an element in the queue"))
(defgeneric dequeue (q)
	(:documentation "Dequeue an element from the queue"))
(defgeneric log-game (outcome q)
	(:documentation "Log the outcome of the game in the queue"))

(defmethod enqueue ((elm number) (q Queue))
	(if (= (mod (+ 1 (put-ptr q)) (queue-size q)) (get-ptr q))
		(progn
			(dequeue q)
			(enqueue elm q))
		(progn
			(setf (elt (elements q) (put-ptr q)) elm)
			(setf (put-ptr q) (mod (+ (put-ptr q) 1) (queue-size q))))))

(defmethod dequeue ((q Queue))
	(if (= (get-ptr q) (put-ptr q))
		nil
		(let ((elm (elt (elements q) (get-ptr q))))
			(setf (get-ptr q) (mod (- (get-ptr q) 1) (queue-size q)))
			elm)))

(defmethod log-game ((outcome number) (q Queue))
	(enqueue outcome q))