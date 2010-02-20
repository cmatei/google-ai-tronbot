
;; does this break me ?!
(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)
(defparameter +defeat+ -5000)
(defparameter +draw+       0)


(defparameter +time-available+ 0.9)

(defparameter *players-separated* nil)

(defparameter *fixed-depth* nil)
(defparameter *time-expired* nil)

(setq *verbose* nil)

(defstruct q
  (last nil)
  (elements nil))

(defun queue-empty-p (q)
  (null (q-elements q)))

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

;    (format t "pos ~a~%" pos)
    
    (if (char= (aref map (1- x) y) #\space)
	(setf lst (cons (list (1- x) y) lst)))

    (if (char= (aref map x (1- y)) #\space)
	(setf lst (cons (list x (1- y)) lst)))

    (if (char= (aref map (1+ x) y) #\space)
	(setf lst (cons (list (1+ x) y) lst)))

    (if (char= (aref map x (1+ y)) #\space)
	(setf lst (cons (list x (1+ y)) lst)))

    lst))



;;; FIXME: this is not correct. When a square is marked by p1 but is
;;; also reachable by p2, it should be counted. FIXME!!!

(defun the-fills (tron)
  (let ((q (make-q))
	(scoreboard (make-array (array-dimensions (tron-map tron)) :initial-element 0))
	(val 0))

    (enqueue q (cons  1 (tron-p1 tron)))
    (enqueue q (cons -1 (tron-p2 tron)))

    (loop
       as f fixnum = 0 then (incf f)
       with l fixnum = 1
       until (> f l)
       do (let* ((pos (dequeue q)))
	    (loop
	       for ppos in (neighbors-of tron (cdr pos))
;	       do (format t "neighbors-of ~a: ~a~%" pos ppos)
	       when (zerop (aref scoreboard (cadr ppos) (car ppos)))
	       do (progn
		    (setf (aref scoreboard (cadr ppos) (car ppos)) (car pos))
		    (incf val (car pos))
		    (enqueue q (cons (car pos) ppos))
		    (incf l)))))

;    scoreboard))
    val))

(declaim (inline final-value-p))
(defun final-value-p (value)
  (or (= value +victory+)
      (= value +defeat+)
      (= value +draw+)))

(declaim (inline end-of-game))
(defun end-of-game (moves)
  (or (null moves)
      (= +victory+ (cadar moves))
      (every (lambda (move) (final-value-p (cadr move))) moves)))

(defun dir-and-score (moves)
  (mapcar #'(lambda (x) (list (car x) (cadr x))) moves))

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




;(defun node-value (node)
;  (declare (ftype (function (tron) fixnum) node-value))
;  (let ((reach1 (the fixnum (count-reachables node 1)))
;	(reach2 (the fixnum (count-reachables node -1))))
;    (the fixnum (- reach1 reach2))))

(defun node-value (node)
  (the-fills node))

(defun negamax (node depth alpha beta color)
  (labels ((nmax-loop (node depth color alpha beta)
	     (if (zerop depth)
		 (node-value node)
		 (loop
		    named nmax
		    for move in '(:left :up :right :down)
		    as child = (make-child-tron node move color)
		    when child do
		      (let ((cval)
			    (nval (negamax child (1- depth) (- beta) (- alpha) (- color))))

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
  (let ((nm nil))
    (loop
       for move in '(:left :up :right :down)
       as child = (make-child-tron node move 1)
       when child do
	 (let ((nval (negamax child (1- depth) +defeat+  +victory+ -1)))
	   (if (eq nval ':aborted)
	       (return-from negamax-toplevel ':aborted)
	       (setq nm (cons (list move (- nval)) nm)))))
    (sort nm #'> :key (lambda (x) (cadr x)))))


;; broken: fix the values

;; (defun rec-fill-move (node depth)
;;   (cond (*time-expired* ':aborted)
;; 	((player-stuck-p node 1)
;; 	 0)
;; 	((zerop depth)
;; 	 (count-reachables node 1))
;; 	(t
;; 	 (let ((best +victory+))
;; 	   (loop
;; 	      for move in '(:left :up :right :down)
;; 	      as child = (make-child-tron node move 1)
;; 	      when child
;; 	      do (let ((val (rec-fill-move child (1- depth))))
;; 		   (when (eq val ':aborted)
;; 		     (return-from rec-fill-move ':aborted))
;; 		   (when (< val best)
;; 		     (setq best val))))
;; 	   best))))
;;
;; (defun fill-toplevel (node depth)
;;   (let ((nm nil))
;;     (loop
;;        for move in '(:left :up :right :down)
;;        as child = (make-child-tron node move 1)
;;        when child do
;; 	 (let ((nval (rec-fill-move node (1- depth))))
;; 	   (if (eq nval ':aborted)
;; 	       (return-from fill-toplevel ':aborted)
;; 	       (setq nm (cons (list move nval) nm)))))
;;     (sort nm #'> :key (lambda (x) (cadr x)))))

(defun iterative-deepening (node start-depth step proc)
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

(defun decide-move (tron)
  (let* ((start-time (get-internal-real-time))
	 (moves)
	 (end-time))

    (sb-ext:schedule-timer
     (sb-ext:make-timer #'(lambda () (setq *time-expired* t)))
     +time-available+)

    (setq *time-expired* nil)
;    (setq *players-separated* (players-separated-p tron))
        
    (setq moves (iterative-deepening tron 4 2 #'negamax-toplevel))

    (when moves
      ;; keep only the best moves
      (let ((best-move (cadar moves)))
	(setq moves (remove-if-not #'(lambda (move) (= (cadr move) best-move)) moves)))

      ;; wallhug  ...
      )

    (when (null moves)
      (error "why ?"))

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

