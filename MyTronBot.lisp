
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

(setq *verbose* t)

;; a queue
(defstruct q
  (last nil)
  (elements nil))

(defun enqueue (q item)
  (cond ((or (null (q-last q)) (null (q-elements q)))
	 (setf (q-last q) (cons item nil)
	       (q-elements q) (q-last q)))
	 (t
	  (setf (cdr (q-last q)) (cons item nil)
		(q-last q) (cdr (q-last q))))))

(defun dequeue (q)
  (pop (q-elements q)))

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
			(declare (type (or symbol fixnum) nval)
				 (type fixnum cval))

			(when (eq nval ':aborted)
			  (return-from nmax nval))

			(setq cval (- nval))

			(when (> cval alpha)
			  (setq alpha cval))

			(when (>= cval beta)
			  (return-from nmax alpha)))
		    finally (return-from nmax alpha)))))

    (if *time-expired*
	':aborted
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
	  val))))

(defun negamax-toplevel (node depth)
  (declare (type fixnum depth))
  (let ((nm nil))
    (loop
       for move in '(:left :up :right :down)
       as child = (make-child-tron node move 1)
       when child do
	 (let ((nval (negamax child (the fixnum (1- depth)) +defeat+  +victory+ -1)))
	   (declare (type (or symbol fixnum) nval))
	   (if (eq nval ':aborted)
	       (return-from negamax-toplevel ':aborted)
	       (setq nm (cons (list move (the fixnum (- nval))) nm)))))
    (sort nm #'> :key (lambda (x) (cadr x)))))


(defun count-reachables (tron color)
  (declare (optimize (speed 3) (safety 3) (debug 0))
	   (type fixnum color))
  (let ((map (copy-tron-map (tron-map tron)))
	(x (x-of tron color))
	(y (y-of tron color)))
    (declare (type (simple-array character (* *)) map)
	     (type fixnum x y))

    (labels ((ff-area (x y)
	       (declare (type fixnum x y)
			(ftype (function (fixnum fixnum) fixnum) ff-area))

	       (if (not (empty-square-p map x y))
		   0
		   (progn
		     (setf (aref map x y) #\#)
		     (the fixnum (+ 1
				    (ff-area (1- x) y)
				    (ff-area x (1- y))
				    (ff-area (1+ x) y)
				    (ff-area x (1+ y))))))))

      (the fixnum (+ (ff-area (1- x) y)
		     (ff-area x (1- y))
		     (ff-area (1+ x) y)
		     (ff-area x (1+ y)))))))

(defun flood-fill (node depth)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum depth)
	   (ftype (function (tron fixnum) (or symbol fixnum)) flood-fill))
  (let ((best 0))
    (declare (type fixnum best))
    (cond (*time-expired*
	   ':aborted)
	  ((zerop depth)
	   (count-reachables node 1))
	  (t
	   (loop
	      for move in '(:left :up :right :down)
	      as child = (make-child-tron node move 1)
	      when child
	      do (let ((val (flood-fill child (1- depth))))
		   (declare (type (or symbol fixnum) val))
		   (when (eq val ':aborted)
		     (return-from flood-fill ':aborted))

		   (when (> val best)
		     (setq best val))

;		   (logmsg move ", val " val ", best " best "~%")
		   ))
	   (incf best)
	   best))))

(defun fill-toplevel (node depth)
  (declare (type fixnum depth)
	   (ftype (function (tron fixnum) (or symbol fixnum)) fill-toplevel))
  (let ((fm nil))
    (loop
       for move in '(:left :up :right :down)
       as child = (make-child-tron node move 1)
       when child do
	 (let ((val (flood-fill child (1- depth))))
	   (declare (type (or symbol fixnum) val))
	   (logmsg "toplevel fill: move " move ", val " val ", depth " depth "~%")
	   (if (eq val ':aborted)
	       (return-from fill-toplevel ':aborted)
	       (setq fm (cons (list move val) fm)))))
    (sort fm #'> :key (lambda (x) (cadr x)))))


(defun iterative-deepening (node start-depth step proc)
  (declare (type fixnum start-depth step))
  (let ((moves nil))
    (loop
       for depth fixnum = start-depth then (incf depth step)
       as newmoves = (funcall proc node depth)
       when (not (eq newmoves ':aborted)) do (setq moves newmoves)
       until (or *fixed-depth*
		 *time-expired*
;		 (end-of-game moves)
		 (> depth 5000)) ;; this is to catch the above until a fix
       finally (logmsg "DEPTH: " depth ", moves " (dir-and-score moves) "~%"))
    moves))


(defun players-separated-p (tron)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((map (copy-tron-map (tron-map tron)))
	(x (x-of tron 1))
	(y (y-of tron 1)))
    (declare (type (simple-array character (* *)) map)
	     (type fixnum x y))

    (labels ((ff-area (x y)
	       (declare (type fixnum x y)
			(ftype (function (fixnum fixnum) fixnum) ff-area))

	       (cond ((char= (aref map x y) #\2)
		      (return-from players-separated-p nil))
		     ((not (empty-square-p map x y))
		      nil)
		     (t
		      (setf (aref map x y) #\#)
		      (or
		       (ff-area (1- x) y)
		       (ff-area x (1- y))
		       (ff-area (1+ x) y)
		       (ff-area x (1+ y)))))))

      (not (or (ff-area (1- x) y)
	       (ff-area x (1- y))
	       (ff-area (1+ x) y)
	       (ff-area x (1+ y)))))))

(defun distance-to-opponent (tron)
  (let ((x1 (x-of tron 1))
	(y1 (y-of tron 1))
	(x2 (x-of tron -1))
	(y2 (y-of tron -1)))
    (declare (type fixnum x1 x2 y1 y2))
    (truncate (sqrt (+ (expt (- x1 x2) 2)
		       (expt (- y1 y2) 2))))))

(defun break-ties (tron moves metric)
  (let ((m (loop
	      for move in moves
	      as child = (make-child-tron tron (car move) 1)
	      collecting (list (funcall metric child) move))))

    (setq m (sort m #'> :key #'(lambda (x) (car x))))
    (mapcar #'(lambda (x) (cadr x)) m)))

(defun decide-move (tron)
  (let* ((start-time (get-internal-real-time))
	 (moves)
	 (end-time))

    (sb-ext:schedule-timer
     (sb-ext:make-timer #'(lambda () (setq *time-expired* t)))
     +time-available+)

    (setq *time-expired* nil)
    (setq *players-separated* (players-separated-p tron))

;    (setq moves
;	  (if *players-separated*
;	      (let ((*fixed-depth* nil))
;		(logmsg "separated!~%")
;		(iterative-deepening tron 1 1 #'fill-toplevel))
;	      (iterative-deepening tron 4 2 #'negamax-toplevel)))

    (setq moves (iterative-deepening tron 4 2 #'negamax-toplevel))

    (when moves
      ;; keep only the best moves
      (let ((best-move (cadar moves)))
	(setq moves (remove-if-not #'(lambda (move) (= (cadr move) best-move)) moves)))

      ;; break ties
 ;     (setq moves (break-ties tron moves #'distance-to-opponent))
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

