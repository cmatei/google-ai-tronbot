
;; does this break me ?!
(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)
(defparameter +defeat+ -5000)

(defparameter +draw+       0)

(defparameter +time-available+ 0.95)

(defparameter *players-separated* nil)

(defparameter *fixed-depth* nil)
(defparameter *time-expired* nil)

(setq *verbose* nil)

(define-condition out-of-time () ())

;; a queue
(defstruct q
  (last nil)
  (elements nil))

(declaim (inline enqueue dequeue))
(defun enqueue (q item)
  (cond ((null (q-elements q))
	 (setf (q-last q) (cons item nil)
	       (q-elements q) (q-last q)))
	 (t
	  (setf (cdr (q-last q)) (cons item nil)
		(q-last q) (cdr (q-last q))))))

(defun dequeue (q)
  (pop (q-elements q)))

(declaim (inline neighbors-of))
(defun neighbors-of (tron pos)
  (let ((x (car pos))
	(y (cadr pos))
	(map (tron-map tron))
	(lst nil))
    (declare (type fixnum x y)
	     (type (simple-array character (* *)) map))

    (if (char= (aref map (1- x) y) #\space)
	(setf lst (cons (list (the fixnum (1- x)) y) lst)))

    (if (char= (aref map x (1- y)) #\space)
	(setf lst (cons (list x (1- y)) lst)))

    (if (char= (aref map (1+ x) y) #\space)
	(setf lst (cons (list (1+ x) y) lst)))

    (if (char= (aref map x (1+ y)) #\space)
	(setf lst (cons (list x (1+ y)) lst)))

    lst))


(defparameter +tile-tie+ 100000)

(declaim (inline tile-not-visited))
(defun tile-not-visited (scoreboard x y who)
  (declare (type fixnum x y who)
	   (type (simple-array fixnum (* *)) scoreboard))
  (let ((val (aref scoreboard x y)))
    (declare (type fixnum val))
    (or (zerop val)
	(= val (- who)))))

(declaim (inline tile-mark-visited))
(defun tile-mark-visited (scoreboard x y who)
  (declare (type fixnum x y who)
	   (type (simple-array fixnum (* *)) scoreboard))
  (if (zerop (aref scoreboard x y))
      (setf (aref scoreboard x y) who)
      (setf (aref scoreboard x y) +tile-tie+)))

(defun node-value (tron)
  (let ((q (make-q))
	(scoreboard (make-array (array-dimensions (tron-map tron))
				:element-type 'fixnum
				:initial-element 0))
	(val 0))

    (declare (type fixnum val))

    (enqueue q (cons  1 (tron-p1 tron)))
    (enqueue q (cons -1 (tron-p2 tron)))

    (loop
       as f fixnum = 0 then (incf f)
       with l fixnum = 1
       until (> f l)
       do (let* ((pos (dequeue q))
		 (dist (car pos)))

	    (declare (type fixnum dist))

	    (if (> dist 0)
		(incf dist)
		(decf dist))

	    (loop
	       for ppos in (neighbors-of tron (cdr pos))
;	       do (format t "neighbors-of ~a: ~a~%" pos ppos)
	       when (tile-not-visited scoreboard (car ppos) (cadr ppos) dist)
	       do (progn
		    (tile-mark-visited scoreboard (car ppos) (cadr ppos) dist)
		    (if (> dist 0)
			(incf val)
			(decf val))
		    (enqueue q (cons dist ppos))
		    (incf l)))))

    val))

(defun dir-and-score (moves)
  (mapcar #'(lambda (x) (list (car x) (cadr x))) moves))

(defun negamax (node depth alpha beta color)
  (declare (type fixnum depth alpha beta color))
  (labels ((nmax-loop (node depth color alpha beta)
	     (declare (type fixnum depth alpha beta color))
	     (if (zerop depth)
		 (node-value node)
		 (loop
		    named nmax
		    for move in '(:left :up :right :down)
		    as child = (make-child-tron node move color)
		    when child do
		      (let ((cval 0)
			    (nval (negamax child (1- depth) (- beta) (- alpha) (- color))))
			(declare (type fixnum nval)
				 (type fixnum cval))

			(setq cval (- nval))

			(when (> cval alpha)
			  (setq alpha cval))

			(when (>= cval beta)
			  (return-from nmax alpha)))
		    finally (return-from nmax alpha)))))

    (when *time-expired*
      (signal 'out-of-time))
    
    (let ((val (if (= color 1)
		   (cond				; i am to move and ...
		     
		     ((equal (tron-p1 node)		; we're in the same spot
			     (tron-p2 node))
		      +draw+)

		     ((player-stuck-p node 1)		; i am stuck
		      (if (player-stuck-p node -1)
			  +draw+			; ... but he is too
			  +defeat+))			; ... or not :-(

		     ((player-stuck-p node -1)	; we've got him stuck :-)
		      +victory+)

		     (t				; got work to do
		      (nmax-loop node depth color alpha beta)))

					; opponent reply move
		   (nmax-loop node depth color alpha beta))))
	  val)))

(defun negamax-toplevel (node depth)
  (declare (type fixnum depth))
  (let ((nm nil))
    (loop
       for move in '(:left :up :right :down)
       as child = (make-child-tron node move 1)
       when child do
	 (let ((nval (negamax child (the fixnum (1- depth)) +defeat+  +victory+ -1)))
	   (declare (type fixnum nval))
	       (setq nm (cons (list move (the fixnum (- nval))) nm))))
    (sort nm #'> :key (lambda (x) (cadr x)))))


(defun iterative-deepening (node start-depth step proc)
  (declare (type fixnum start-depth step))
  (let ((moves nil)
	(depth-reached 0))
    (declare (type fixnum depth-reached))
    
    (handler-case
	(loop
	   for depth fixnum = start-depth then (incf depth step)
	   do (setq moves (funcall proc node depth)
		    depth-reached depth)
	   until (or *fixed-depth*
		     *time-expired*
		     ;(end-of-game moves)
		     (> depth 5000))) ;; this is to catch the above until a fix
      (out-of-time ()))

    (logmsg "DEPTH: " depth-reached ", moves " (dir-and-score moves) "~%")
    moves))
(defun decide-move (tron)
  (let* ((start-time (get-internal-real-time))
	 (moves)
	 (end-time))

    (sb-ext:schedule-timer
     (sb-ext:make-timer #'(lambda () (setq *time-expired* t)))
     +time-available+)

    (setq *time-expired* nil)

    (setq moves (iterative-deepening tron 4 2 #'negamax-toplevel))

    (when moves
      ;; keep only the best moves
      (let ((best-move (cadar moves)))
	(setq moves (remove-if-not #'(lambda (move) (= (cadr move) best-move)) moves)))

      ;; break ties
;      (setq moves (break-ties tron moves #'distance-to-opponent))
;      (logmsg "attack moves: " moves "~%")
      )

;    (when (null moves)
;      (error "why ?"))

    (setq end-time (get-internal-real-time))
    (logmsg "Took " (- end-time start-time) " for " moves "~%")
    (if moves (caar moves) ':left)))


;;; driver loop

(defun main ()
  (logmsg "== New match!~%~%")
  (loop
     with tron = (make-tron)
     while (peek-char nil *input* nil nil)
     for move from 0
     do
       (read-tron tron)
       (make-move (decide-move tron))))

