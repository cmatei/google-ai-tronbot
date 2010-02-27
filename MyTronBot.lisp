
;; does this break me ?!
(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))

(defparameter +victory+ 5000)
(defparameter +defeat+ -5000)
(defparameter +draw+       0)

(defparameter +time-available+ 0.95)

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
(defun neighbors-of (tron x y)
  (let ((map (tron-map tron))
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

    (enqueue q (list  1 (tron-x1 tron) (tron-y1 tron)))
    (enqueue q (list -1 (tron-x2 tron) (tron-y2 tron)))

    (loop
       as f fixnum = 0 then (incf f)
       with l fixnum = 1
       until (> f l)
       do (destructuring-bind (dist x y) (dequeue q)
	    (declare (type fixnum dist x y))

	    (if (> dist 0)
		(incf dist)
		(decf dist))

	    (loop
	       for ppos in (neighbors-of tron x y)
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

(defun negamax (node depth alpha beta color)
  (declare (type fixnum depth alpha beta color)
	   (ftype (function (tron fixnum fixnum fixnum fixnum) (values fixnum fixnum)) negamax))

  (when *time-expired*
    (signal 'out-of-time))

  ;; we detect final positions only when i am to move
  (when (= color 1)
    (cond ((and (= (tron-x1 node) (tron-x2 node)) ; we're in the same spot
		(= (tron-y1 node) (tron-y2 node)))
	   (return-from negamax (values +draw+ depth)))

	  ((player-stuck-p node 1)	         ; i am stuck
	   (if (player-stuck-p node -1)
	       (return-from negamax (values +draw+ depth))     ; ... but he is too
	       (return-from negamax (values +defeat+ depth)))) ; ... or not :-(

	  ((player-stuck-p node -1)	         ; we've got him stuck :-)
	   (return-from negamax (values +victory+ depth)))))

  (when (zerop depth)
    (return-from negamax (values (node-value node) 0)))

  (let ((reached-depth depth))
    (declare (type fixnum reached-depth))
    
    (values 
     (loop
	named nmax
	for move in '(:left :up :right :down)
	when (make-tron-movement node move color)
	do (multiple-value-bind (nval ndepth)
	       (negamax node (1- depth) (- beta) (- alpha) (- color))
	     
	     (declare (type fixnum nval ndepth))
	    
	     (when (> (- nval) alpha)
	       (setq reached-depth (1+ ndepth))
	       (setq alpha (- nval)))
	    
	     (when (>= (- nval) beta)
	       (setq reached-depth (1+ ndepth))
	       (undo-tron-movement node move color)
	       (return-from nmax alpha))

	     (undo-tron-movement node move color))
		      
	finally (return-from nmax alpha))
     reached-depth)))


(defun negamax-toplevel (node depth)
  (declare (type fixnum depth))
  (let ((nm nil))

    (loop
       for move in '(:left :up :right :down)
       when (make-tron-movement node move 1)
       do (multiple-value-bind (nval ndepth)
	      (negamax node (the fixnum (1- depth)) +defeat+ +victory+ -1)

	    (declare (type fixnum nval ndepth))

	    (setq nm (cons (list move
				 (the fixnum (- nval))
				 (the fixnum (1+ ndepth)))
			   nm))
	    (undo-tron-movement node move 1)))

    (sort nm #'> :key (lambda (x) (cadr x)))))

(defun floodfill (node depth)
  (declare (type fixnum depth))
  (cond (*time-expired*
	 (signal 'out-of-time))
	((zerop depth)
	 (count-reachables node 1))
	(t
	 (let ((best-fill +defeat+))
	   (loop
	      for move in '(:left :up :right :down)
	      when (make-tron-movement node move 1)
	      do (let ((cval (floodfill node (1- depth))))
		   (declare (type fixnum cval))
		   (when (> cval best-fill)
		     (setq best-fill cval))
		   (undo-tron-movement node move 1)))
	   (1+ best-fill)))))

(defun floodfill-toplevel (node depth)
  (declare (type fixnum depth))
  (let ((fm nil))
    (loop
       for move in '(:left :up :right :down)
       when (make-tron-movement node move 1)
       do (progn
	    (setq fm (cons (list move (floodfill node (1- depth))) fm))
	    (undo-tron-movement node move 1)))

    (sort fm #'> :key (lambda (x) (cadr x)))))

(defun dir-and-score (moves)
  (mapcar #'(lambda (x) (list (car x) (cadr x))) moves))

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




(defun copy-tron-map (tron)
  (let ((cmap (make-array (array-dimensions (tron-map tron))
			  :element-type 'character
			  :adjustable nil
			  :fill-pointer nil)))
    
    (loop
       for y fixnum from 0 below (tron-height tron)
       do (loop
	     for x fixnum from 0 below (tron-width tron)
	     do (setf (aref cmap x y)
		      (aref (tron-map tron) x y))))

    cmap))


(defun count-reachables (tron color)
  (declare (optimize (speed 3) (safety 3) (debug 0))
	   (type fixnum color))
  (let ((map (copy-tron-map  tron))
	(x (if (= color 1) (tron-x1 tron) (tron-x2 tron)))
	(y (if (= color 1) (tron-y1 tron) (tron-y2 tron))))
    
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

(defun fill-looking (map x y)
  (cond ((char= (aref map x y) #\2) t)
	((char= (aref map x y) #\#) nil)
	(t
	 (setf (aref map x y) #\#)
	 (or (fill-looking map (1- x) y)
	     (fill-looking map (1+ x) y)
	     (fill-looking map x (1- y))
	     (fill-looking map x (1+ y))))))
	     
	 

(defun players-separated-p (tron)
  (let ((map (copy-tron-map tron)))
    (not (or (fill-looking map (1- (tron-x1 tron)) (tron-y1 tron))
	     (fill-looking map (1+ (tron-x1 tron)) (tron-y1 tron))
	     (fill-looking map (tron-x1 tron) (1- (tron-y1 tron)))
	     (fill-looking map (tron-x1 tron) (1+ (tron-y1 tron)))))))


(defun monte-carlo-fill (node val)
  (declare (type fixnum val))
  (let ((neigh (neighbors-of node (tron-x1 node) (tron-y1 node))))
    (cond ((null neigh)
	   val)
	  (t
	   (let ((newpos (elt neigh (random (length neigh)))))
	     (setf (tron-x1 node) (car newpos)
		   (tron-y1 node) (cadr newpos))
	     (setf (aref (tron-map node) (tron-x1 node) (tron-y1 node)) #\1)
	     (monte-carlo-fill node (1+ val)))))))

(defun monte-carlo-toplevel (tron)
  (let ((choices nil)
	(choice-count 0)
	(val 0))
    (declare (type fixnum choice-count))
    
    (loop
       for move in '(:left :right :up :down)
       when (make-tron-movement tron move 1)
       do (progn
	    (setq choices (cons (list move 0 0) choices))
	    (undo-tron-movement tron move 1)))

    (setq choice-count (length choices))

    (if (zerop choice-count)
	(return-from monte-carlo-toplevel (list '(:left 0 0))))

    (if (= choice-count 1)
	(return-from monte-carlo-toplevel choices))

    (loop
       for movepick fixnum = (random choice-count)
       as node = (make-child-tron tron (car (elt choices movepick)) 1)
       do (let ((mc (monte-carlo-fill node 1)))
;	    (format t "movepick ~a, node ~a, mc ~a~%" movepick node mc)
	    (incf val)

	    (incf (cadr (elt choices movepick)) mc)
	    
	    (incf (caddr (elt choices movepick)))
;	    (setf (cadr (elt choices movepick))
;		  (truncate (/ (cadr (elt choices movepick)) 2))))
	    )
       until *time-expired*)

;    (format t "copied ~a times, got ~a~%" val choices)

    (logmsg "choices " choices "~%")
    
    (sort choices #'> :key (lambda (x) (cadr x)))))

(defun decide-move (tron)
  (let ((start-time (get-internal-real-time))
	 (moves)
	 (end-time))

    (sb-ext:schedule-timer
     (sb-ext:make-timer #'(lambda () (setq *time-expired* t)))
     +time-available+)

    (setq *time-expired* nil)


    (if (players-separated-p tron)
	(setq moves (iterative-deepening tron 4 1 #'floodfill-toplevel))
;	(setq moves (monte-carlo-toplevel tron))
	(setq moves (iterative-deepening tron 4 2 #'negamax-toplevel)))

    (when moves

      ;; keep only the best moves
      (let ((best-move (cadar moves)))
	(setq moves (remove-if-not #'(lambda (move)
				       (eq (cadr move) best-move)) moves)))

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
       (emit-move (decide-move tron))))



