(load "queue")
(load "game")

(defpackage :world
	(:documentation "Package for the world")
	(:use :common-lisp :queue :game))

(in-package :world)

;;;;;;;;;;;
;; World ;;
;;;;;;;;;;;
;; This class creates the world in which games are played and keeps
;; statistics about the games. The world has all agents, all meanings,
;; regulates the influx of agents and meaning, counts the number of 
;; games and stores the outcomes in the queue. 
(defclass World ()
	((agent-count
		:reader agent-count
		:initarg :agent-count
		:initform (error "Provide a number of intial agents")
		:type number)
	(agents
		:accessor agents
		:initform '())
	(meaning-count
		:accessor meaning-count
		:initarg :meaning-count
		:initform (error "Provide a number of initial meanings"))
	(meanings
		:accessor meanings
		:initform '())
	(agent-flux
		:accessor agent-flux
		:initarg :agent-flux
		:initform 0
		:type number)
	(meaning-flux
		:accessor meaning-flux
		:initarg :meaning-flux
		:initform 0
		:type number)
	(delta
		:accessor get-delta
		:initarg :delta
		:initform 0.0
		:type float)
	(game-counter
		:accessor game-counter
		:initform 0
		:type number)
	(game-queue
		:accessor game-queue
		:initform (make-instance 'queue::Queue :queue-size 25))))

(defgeneric play-n-games (world games)
	(:documentation "Plays a series of N games"))

(defmethod play-n-games :before ((world World) (games number))
	(let ((agents (make-agents (agent-count world)))
		  (meanings (make-meanings (meaning-count world))))
		(setf (agents world) agents)
		(setf (meanings world) meanings)))

(defun make-agents (N)
	(loop for i upto (- N 1)
		  collect (make-instance 'agent::Agent)))

(defun make-meanings (N)
	(loop for i upto (- N 1)
		  collect (concatenate 'string "MEANING-" (write-to-string i))))

(defmethod play-n-games ((world World) (games number))
	(let ((stats '()))
		(loop for i upto (- games 1)
			  do (let* ((rs (make-random-state t))
			   			(speaker-idx (random (list-length (agents world)) rs))
			   			(hearer-idx (case (random 2 rs)
			  							(0 (mod (- speaker-idx 1) (list-length (agents world))))
			  							(1 (mod (+ speaker-idx 1) (list-length (agents world))))))
			   			(speaker (nth speaker-idx (agents world)))
			  			(hearer (nth hearer-idx (agents world)))
			   			(meanings (select-game-meanings (meanings world)))
			   			(game (make-instance 'game::Game :speaker speaker :hearer hearer :meanings meanings :delta (get-delta world)))
			   			(outcome (game::play game)))
			  		;;log outcome to game-queue
			  		(pprint outcome)
			  		(queue::enqueue outcome (game-queue world))

			  		;;Increase game-counter
			  		(incf (game-counter world))

			  		;;Sort the agents on age
			  		(setf (agents world) (sort (agents world) #'> :key (lambda (agent) (agent::agent-age agent))))

			  		;;Check if game is multiple of agent-flux; if yes->remove oldest agent and add virgin agent
			  		;;Check if game is multiple of meaning-flux; if yes->add new meaning
			  		(when (not (= 0 (agent-flux world)))
			  			(when (integerp (/ (game-counter world) (agent-flux world)))
			  				(replace-agent world)))
			  		(when (not (= 0 (meaning-flux world)))
			  			(when (integerp (/ (game-counter world) (meaning-flux world)))
			  				(add-new-meaning world)))

			  		;;Compute communicative success
			  		;;Compute lexical coherence
			  		;;Compute lexicon size
			  		(let ((success-rate (success-rate (game-queue world)))
			  			  (lexical-coherence (lexical-coherence world))
			  			  (lexicon-size (lexicon-size world)))
			  			(setf stats (cons (list success-rate lexical-coherence lexicon-size) stats)))))
		(write-data (reverse stats))))

(defun select-game-meanings (meanings)
	(let* ((rs (make-random-state t))
		   (total (list-length meanings))
		   (sze (+ 1 (random total rs)))
		   (result '()))
		(loop for i upto sze
			  for m = (nth (random total rs) meanings)
			  when (not (member m result :test 'equal))
			  	do (setf result (cons m result)))
		result))

(defun replace-agent (world)
	(let ((virgin (make-instance 'agent::Agent)))
		(setf (first (agents world)) virgin)))

(defun add-new-meaning (world)
	(let* ((meaning-count (meaning-count world))
		   (new-meaning (concatenate 'string "MEANING-" (write-to-string meaning-count))))
		(incf (meaning-count world))
		(setf (meanings world) (append (meanings world) (list new-meaning)))))

(defun success-rate (game-queue)
	(let* ((elements (queue::elements game-queue))
		   (success (count 1 elements)))
		(when (not (find nil elements))
			  (float (/ success (length elements))))))

(defun lexical-coherence (world)
	(let ((meanings (meanings world))
		  (agents (agents world))
		  (highest '()))
		(loop for meaning in meanings
			  do (let ((forms nil))
			  		(loop for agent in agents
			  	          ;for all-forms = (all-forms agent meaning)
			  	          for best-form = (agent::best-form agent meaning)
			  	          when best-form
			  	          	do (setf forms (cons best-form forms)))
			  	          ;when all-forms
			  	          ;	do (loop for form in all-forms
			  	          ;	         do (setf forms (cons form forms))))
			  		(if forms
				  		(let* ((freqs (count-freqs forms '()))
				  			   (max-freq (apply #'max freqs))
				  			   (sum-freq (apply #'+ freqs))
				  			   (res (float (/ max-freq sum-freq))))
				  			(setf highest (cons res highest))))))
		(float (/ (apply #'+ highest) (list-length highest)))))
			        
(defun count-freqs (forms freqs)
	(if (null forms)
		freqs
		(let* ((f (first forms))
			   (c (count f forms :test 'equal)))
			(count-freqs (delete-if (lambda (x) (equal x f)) forms) (cons c freqs)))))

(defun lexicon-size (world)
	(let ((agents (agents world))
		  (sizes '()))
		(loop for agent in agents
			  for memory = (agent::agent-memory agent)
			  for forms = (agent::form-hash memory)
			  for size = (hash-table-count forms)
			  do (loop for lst being the hash-values of forms
			  	       when (= 1 (list-length lst))
			  	       		when (= (agent::score (first lst)) 0.0)
			  	       			do (decf size))
			  do (setf sizes (cons size sizes)))
		(float (/ (apply #'+ sizes) (list-length sizes)))))

(defun write-data (data)
	(let ((stream (open "./data.csv" :direction :output :if-exists :supersede)))
		;;R=Success Rate, C=Lexical Coherence, S=Lexion Size
		(write-line "R,C,S" stream)
		(loop for lst in data
			  when lst
			  	do (write-line (format nil "~A,~A,~A" (first lst) (second lst) (third lst)) stream)
			  finally (close stream))))