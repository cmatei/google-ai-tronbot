
(proclaim '(optimize (speed 3) (safety 3) (debug 3)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)

;; ok this is weird, but it bumped me up 50 places
(defparameter +defeat+ -5000)

(defparameter +draw+   0)

(declaim (type fixnum +minimax-depth+))
;; this is safe
;;(defparameter +minimax-depth+ 9)
(defparameter +minimax-depth+ 9)

(defparameter +max-time+ 500)

(setq *verbose* t)

(defun count-reachables (tron color)
  (declare (optimize (speed 3) (safety 0) (debug 0))
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

      (the fixnum (+ (the fixnum (ff-area (1- x) y))
		     (ff-area x (1- y))
		     (ff-area (1+ x) y)
		     (ff-area x (1+ y)))))))


(defun node-value (node)
  (declare (ftype (function (tron) fixnum) node-value))
  (let ((reach1 (the fixnum (count-reachables node 1)))
	(reach2 (the fixnum (count-reachables node -1))))
    (the fixnum (- reach1 reach2))))

(defun negamax (node depth alpha beta color)
  (declare (type fixnum depth alpha beta color))

  (cond
    ((and (= color 1)
	  (or (equal (tron-p1 node) (tron-p2 node))
	      (and (player-stuck-p node 1)
		   (player-stuck-p node -1))))
	  
     +draw+)

;;    ((and (= color 1)
;;	  (player-stuck-p node -1))
;;     +victory+)

;;    ((and (= color 1)
;;	  (player-stuck-p node 1))
;;     +defeat+)

    ((zerop depth)
     (* color (node-value node)))
    
    (t
     (loop
	named nmax
	for move in '(:left :up :right :down)
	as child = (make-child-tron node move color)
;	if (not child) do
;	  (logmsg "DEPTH: " depth ", " color " can't move " move "~%")
;	else do
	when child do
	  (let ((cval (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
	    (when (= depth 8)
	      (logmsg "-----------~%~%")
	      (logmsg "At depth " depth ", alpha " alpha ", beta " beta ", color " color ", cval " cval "~%")
	      (logmsg child)
	      )

	    (when (> cval alpha)
	      (setq alpha cval))
	    (when (>= cval beta)
	      (return-from nmax alpha)))
	finally (return-from nmax alpha)))))

(defun find-negamax-moves (node)
  (let ((start-depth 4)
	(start-time (get-internal-real-time)))
    (labels ((time-remaining ()
	       (< (- (get-internal-real-time) start-time)
		  +max-time+))

	     (find-moves (depth)
	       (let ((nm nil))
		 (setq nm
		       (loop
			  for move in '(:left :up :right :down)
			  as child = (make-child-tron node move 1)
;			  when child do
;			    (logmsg "find-negamax-moves: " move "~%")	       
;			  and collect
			  when child collect 
			    (list move
				  (- (negamax child (1- depth) +defeat+  +victory+ -1))
				  child)))

		 (sort nm #'> :key (lambda (x) (cadr x))))))

      (loop
	 for depth = start-depth then (incf depth)
	 as val = (find-moves depth)
	 do (logmsg "DEPTH: " depth "~%")
	 until (not (time-remaining))
	 finally (return val)))))
    

(defun decide-move (tron)
  (let ((moves nil))

;    (time (setq moves (find-negamax-moves tron)))
    (setq moves (find-negamax-moves tron))
    
    (mapcar (lambda (x) (logmsg (car x) " " (cadr x) ",  ")) moves)
    (logmsg "~%")
;;     
;;     ;; from equivalent moves, choose the one that brings us closer
    (setq moves (remove-if-not (lambda (x) (equal (cadr x) (cadar moves)))
 			       moves))
;; 
     (mapcar (lambda (x) (logmsg (car x) " " (cadr x) ",  ")) moves)
     (logmsg "~%~%")
;;     
    (setq moves (sort moves #'< :key (lambda (x) (player-distance (caddr x)))))

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

