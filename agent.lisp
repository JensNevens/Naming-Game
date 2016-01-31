(defpackage :agent
	(:documentation "Package for the agent. Contains FMTriple, Dictionary, Memory and Agent classes")
	(:use :common-lisp))

(in-package :agent)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form-Meaning Triple ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triple that contains meaning, form and score
(defclass FMTriple ()
	((meaning
		:accessor meaning
		:initarg :meaning
		:initform (error "Provide a meaning when intialising a FMTriple")
		:type string)
	(form
		:accessor form
		:initarg :form
		:initform (error "Provide a form when intialising a FMTriple")
		:type string)
	(score
		:accessor score
		:initform 0.5
		:type float)))

(defgeneric triple-print (FMTriple)
	(:documentation "Pretty print an FMTriple"))

(defmethod triple-print ((triple FMTriple))
	(let ((meaning (meaning triple))
		  (form (form triple))
		  (score (score triple)))
		(format t "(~A, ~A, ~A)" meaning form score)))

;;;;;;;;;;;;;;;;
;; Dictionary ;;
;;;;;;;;;;;;;;;;
;; The Dictionary is responsible for creating new words
(defclass Dictionary () ())

(defgeneric new-word (Dictionary)
	(:documentation "Creates a new random word"))

(defmethod new-word ((dict Dictionary))
	(let* ((rs (make-random-state t))
		   (length (+ 2 (random 3 rs)))
		   (vowels (list "a" "e" "i" "o" "u" "y"))
		   (consonants (list "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "z"))
		   (letters '()))
	    (dotimes (idx length)
			when (oddp idx)
				do (setf letters (append (list (nth (random (- (list-length vowels) 1) rs) vowels)) letters))
			when (evenp idx)
			  	do (setf letters (append (list (nth (random (- (list-length consonants) 1) rs) consonants)) letters)))
		(let ((new-word (concatString (reverse letters))))
			new-word)))

(defun concatString (list)
  (if (listp list)
      (with-output-to-string (s)
         (dolist (item list)
           (if (stringp item)
             (format s "~a" item))))))


;;;;;;;;;;;;
;; Memory ;;
;;;;;;;;;;;;
;; The Memory of an Agent. This is designed as follows:
;; the memory consists of 2 hash tables, one where the keys are forms
;; and one where the keys are meanings. The values of both of these
;; hash tables are lists of FMTriples, sorted on their score value.
;; This design makes it easy to search all form-meaning combinations
;; on meaning and on form.
(defclass Memory ()
	((meaning-hash
		:accessor meaning-hash
		:initform (make-hash-table :test 'equal)
		:type 'hash-table)
	(form-hash
		:accessor form-hash
		:initform (make-hash-table :test 'equal)
		:type 'hash-table)))

(defgeneric insert (memory meaning form)
	(:documentation "Create a FMTriple and insert in both hashtables"))

(defgeneric find-meanings (memory meaning)
	(:documentation "Find all FMtriples with given meaning"))
(defgeneric find-forms (memory form)
	(:documentation "Find all FMtriples with given form"))
(defgeneric find-best-meaning (memory meaning)
	(:documentation "Find FMTriple with highest score corresponding to meaning"))
(defgeneric find-best-form (memory form)
	(:documentation "Find FMTriple with highest score corresponding to form"))
(defgeneric find-meaning-form (memory meaning form)
	(:documentation "Find the triple with form in meaning hash table"))
(defgeneric find-form-meaning (memory form meaning)
	(:documentation "Find the triple with meaning in form hash table"))

(defgeneric increment-score-meaning (memory meaning form delta)
	(:documentation "Increment the score of the meaning with given form with +delta, decrement rest with -delta"))
(defgeneric increment-score-form (memory form meaning delta)
	(:documentation "Increment the score of the form with given meaning with +delta, decrement rest with -delta"))
(defgeneric decrement-score-meaning (memory meaning form delta)
	(:documentation "Decrement the score of the meaning with given form with delta"))
(defgeneric decrement-score-form (memory form meaning delta)
	(:documentation "Decrement the score of the form with given meaning with delta"))

(defgeneric pprint-memory (memory)
	(:documentation "Pretty prints the memory hash tables"))

(defmethod insert ((mem Memory) (meaning string) (form string))
	;;Look if meaning is in hash, if yes->insert sorted, if no->create list and insert
	;;Look if form is in hash, if yes->insert sorted, if no->create list and insert
	(let ((meaning-hash (meaning-hash mem))
		  (form-hash (form-hash mem))
		  (triple (make-instance 'FMTriple :meaning meaning :form form)))
		(if (gethash meaning meaning-hash)
			(setf (gethash meaning meaning-hash) (insert-sorted triple (gethash meaning meaning-hash)))
			(create-meaning triple meaning-hash))
		(if (gethash form form-hash)
			(setf (gethash form form-hash) (insert-sorted triple (gethash form form-hash)))
			(create-form triple form-hash))
		triple))

(defun create-meaning (triple meaning-hash)
	(let ((meaning (meaning triple)))
		(setf (gethash meaning meaning-hash) (list triple))))

(defun create-form (triple form-hash)
	(let ((form (form triple)))
		(setf (gethash form form-hash) (list triple))))

(defun insert-sorted (triple lst)
	(progn
		(setf lst (cons triple lst))
		(sort lst #'> :key (lambda (trip) (score trip)))
		lst))

(defmethod find-meanings ((mem Memory) (meaning string))
	(let* ((meaning-hash (meaning-hash mem))
		   (meaning-lst (gethash meaning meaning-hash)))
		meaning-lst))

(defmethod find-forms ((mem Memory) (form string))
	(let* ((form-hash (form-hash mem))
		   (form-lst (gethash form form-hash)))
		form-lst))

(defmethod find-best-meaning ((mem Memory) (meaning string))
	(let* ((meaning-hash (meaning-hash mem))
		   (meaning-lst (gethash meaning meaning-hash)))
		(if meaning-lst
			(first meaning-lst)
			nil)))

(defmethod find-best-form ((mem Memory) (form string))
	(let* ((form-hash (form-hash mem))
		   (form-lst (gethash form form-hash)))
		(if form-lst
			(first form-lst)
			nil)))

(defmethod find-meaning-form ((mem Memory) (meaning string) (form string))
	(let* ((meaning-hash (meaning-hash mem))
		   (meaning-lst (gethash meaning meaning-hash)))
		(when (not (null meaning-lst))
			(loop for triple in meaning-lst
				  when (equal (form triple) form)
				  	do (return triple)))))

(defmethod find-form-meaning ((mem Memory) (form string) (meaning string))
	(let* ((form-hash (form-hash mem))
		   (form-lst (gethash form form-hash)))
		(when (not (null form-lst))
			(loop for triple in form-lst
				  when (equal (meaning triple) meaning)
				  	do (return triple)))))

(defmethod increment-score-meaning ((mem Memory) (meaning string) (form string) (delta float))
	(let* ((meaning-hash (meaning-hash mem))
		   (meaning-lst (gethash meaning meaning-hash)))
		(when (not (null meaning-lst))
			(loop for triple in meaning-lst
				  if (equal (form triple) form)
				  	if (> (+ (score triple) delta) 1.0)
				  		do (setf (score triple) 1.0)
				  	else
				  		do (incf (score triple) delta)
				  else
				  	if (< (- (score triple) delta) 0.0)
				  		do (setf (score triple) 0.0)
				  	else
				  		do (decf (score triple) delta))
			(setf (gethash meaning meaning-hash) (sort (gethash meaning meaning-hash) #'> :key (lambda (trip) (score trip)))))))

(defmethod increment-score-form ((mem Memory) (form string) (meaning string) (delta float))
	(let* ((form-hash (form-hash mem))
		   (form-lst (gethash form form-hash)))
		(when (not (null form-lst))
			(loop for triple in form-lst
				  if (equal (form triple) form)
				  	if (> (+ (score triple) delta) 1.0)
				  		do (setf (score triple) 1.0)
				  	else
				  		do (incf (score triple) delta)
				  else
				  	if (< (- (score triple) delta) 0.0)
				  		do (setf (score triple) 0.0)
				  	else
				  		do (decf (score triple) delta))
			(setf (gethash form form-hash) (sort (gethash form form-hash) #'> :key (lambda (trip) (score trip)))))))

(defmethod decrement-score-meaning ((mem Memory) (meaning string) (form string) (delta float))
	(let* ((meaning-hash (meaning-hash mem))
		   (meaning-lst (gethash meaning meaning-hash)))
		(when (not (null meaning-lst))
			(loop for triple in meaning-lst
				  when (equal (form triple) form)
				  	if (< (- (score triple) delta) 0.0)
				  		do (setf (score triple) 0.0)
				  	else
				  		do (decf (score triple) delta))
			(setf (gethash meaning meaning-hash) (sort (gethash meaning meaning-hash) #'> :key (lambda (trip) (score trip)))))))

(defmethod decrement-score-form ((mem Memory) (form string) (meaning string) (delta float))
	(let* ((form-hash (form-hash mem))
		   (form-lst (gethash form form-hash)))
		(when (not (null form-lst))
			(loop for triple in form-lst
				  when (equal (meaning triple) meaning)
				  	if (< (- (score triple) delta) 0.0)
				  		do (setf (score triple) 0.0)
				  	else
				  		do (decf (score triple) delta))
			(setf (gethash form form-hash) (sort (gethash form form-hash) #'> :key (lambda (trip) (score trip)))))))

(defmethod pprint-memory ((mem Memory))
	(let ((meaning-hash (meaning-hash mem))
		  (form-hash (form-hash mem)))
		(format t "MEANINGS:~C~C" #\return #\linefeed)
		(maphash (lambda (key value)
					(progn
						(format t "  ~A: " key)
						(loop for triple in value
							  do (triple-print triple))
						(format t "~C~C" #\return #\linefeed)))
				meaning-hash)
		(format t "FORMS:~C~C" #\return #\linefeed)
		(maphash (lambda (key value)
					(progn
						(format t "  ~A: " key)
						(loop for triple in value
							  do (triple-print triple))
						(format t "~C~C" #\return #\linefeed)))
				form-hash)))

;;;;;;;;;;;
;; Agent ;;
;;;;;;;;;;;
;; The Agent has an age, a memory and access to a Dictionary (shared by all agents)
;; Most of the work is done in the Memory class
(defclass Agent ()
	((age
		:accessor agent-age
		:initform 0
		:type number)
	(memory
		:accessor agent-memory
		:initform (make-instance 'Memory)
		:type 'Memory)
	(dictionary
		:accessor dictionary
		:initform (make-instance 'Dictionary)
		:type 'Dictionary
		:allocation :class)))

(defgeneric update-age (agent)
	(:documentation "Increment the age counter of the agent"))
(defgeneric invent-word (agent)
	(:documentation "Invent a new word"))

(defgeneric has-form (agent meaning)
	(:documentation "Speaker checks if it has a form for the given meaning"))
(defgeneric has-meaning (agent form meanings)
	(:documentation "Hearer checks if it has a meaning for the given form"))
(defgeneric receive-meaning (agent meaning form)
	(:documentation "Hearer receives the meaning for a given form"))
(defgeneric check-meaning (agent game-meaning hearer-meaning)
	(:documentation "Speaker checks if returned meaning is correct"))

(defgeneric increment-speaker (agent meaning form delta)
	(:documentation "Speaker increments the score for the form of the given meaning"))
(defgeneric increment-hearer (agent meaning form delta)
	(:documentation "Hearer increments the score for the meaning of the given form"))
(defgeneric decrement-speaker (agent meaning form delta)
	(:documentation "Speaker decrements the score for the form of the given meaning"))
(defgeneric decrement-hearer (agent meaning form delta)
	(:documentation "Hearer decrements the score for the meaning of the given form"))

(defgeneric best-form (agent meaning)
	(:documentation "Return the best form for the given meaning"))
(defgeneric all-forms (agent meaning)
	(:documentation "Return all forms for the given meaning"))

(defmethod update-age ((agent Agent))
	(incf (agent-age agent)))

(defmethod invent-word ((agent Agent))
	(let ((dict (dictionary agent)))
		(new-word dict)))

(defmethod has-form ((agent Agent) (meaning string))
	(let* ((memory (agent-memory agent))
		   (triple (find-best-meaning memory meaning)))
		(if triple
			(form triple)
			(let* ((new-form (invent-word agent))
				   (triple (insert memory meaning new-form)))
				(form triple)))))

(defmethod has-meaning ((agent Agent) (form string) (meanings list))
	(let ((memory (agent-memory agent))
		  (result nil))
		(loop for meaning in meanings
			  for triple = (find-form-meaning memory form meaning)
			  when triple
			  	do (setf result (cons triple result)))
		(if result
			(progn
				(setf result (sort result #'> :key (lambda (triple) (score triple))))
				(meaning (first result)))
			nil)))

(defmethod receive-meaning ((agent Agent) (meaning string) (form string))
	(let ((memory (agent-memory agent)))
		(insert memory meaning form)))

(defmethod check-meaning ((agent Agent) (game-meaning string) (hearer-meaning string))
	(equal game-meaning hearer-meaning))

(defmethod increment-speaker ((agent Agent) (meaning string) (form string) (delta float))
	(let ((memory (agent-memory agent)))
		(increment-score-meaning memory meaning form delta)))

(defmethod increment-hearer ((agent Agent) (meaning string) (form string) (delta float))
	(let ((memory (agent-memory agent)))
		(increment-score-form memory form meaning delta)))

(defmethod decrement-speaker ((agent Agent) (meaning string) (form string) (delta float))
	(let ((memory (agent-memory agent)))
		(decrement-score-meaning memory meaning form delta)))

(defmethod decrement-hearer ((agent Agent) (meaning string) (form string) (delta float))
	(let ((memory (agent-memory agent)))
		(decrement-score-form memory form meaning delta)))

(defmethod best-form ((agent Agent) (meaning string))
	(let* ((memory (agent-memory agent))
		   (triple (find-best-meaning memory meaning)))
		(if triple
			(form triple)
			nil)))

(defmethod all-forms ((agent Agent) (meaning string))
	(let* ((memory (agent-memory agent))
		   (triples (find-meanings memory meaning)))
		(if triples
			(loop for triple in triples
				  collect (form triple) into result
				  do (return result))
			nil)))