
(proclaim '(optimize (speed 3) (safety 3) (debug 3)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)
(defparameter +defeat+ -5000)
(defparameter +draw+   -2500)

(declaim (type fixnum +minimax-depth+))
(defparameter +minimax-depth+ 8)


(setq *verbose* nil)

(defun count-reachables (tron color)
  (let ((map (copy-tron-map (tron-map tron)))
	(x (x-of tron color))
	(y (y-of tron color)))
    (declare (type (simple-array character (* *)) map)
	     (type fixnum x y))

    (labels ((ff-area (x y)
	       (declare (type fixnum x y))
	       (if (not (empty-square-p map x y))
		   0
		   (progn
		     (setf (aref map x y) #\#)
		     (+ 1
			(ff-area (1- x) y)
			(ff-area x (1- y))
			(ff-area (1+ x) y)
			(ff-area x (1+ y)))))))

      (+ (ff-area (1- x) y)
	 (ff-area x (1- y))
	 (ff-area (1+ x) y)
	 (ff-area x (1+ y))))))

(defun node-value (node)
  (let ((reach1 (count-reachables node 1))
	(reach2 (count-reachables node -1)))
    (let ((val (- reach1 reach2)))
      (truncate val))))

(defun negamax (node depth alpha beta color)
  (declare (type fixnum depth alpha beta color))
  (cond
    ((victory-p node)
     (* color +victory+))
    
    ((defeat-p node)
     (* color +defeat+))
    
    ((draw-p node)
     (* color +draw+))
    
    ((zerop depth)
     (* color (node-value node)))
    
    (t
     (loop
	named nmax
	for move in '(:left :up :right :down)
	as child = (make-child-tron node move color)
	when child do
	  (let ((cval (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
;	    (when (< depth 2)
;	      (logmsg "-----------~%~%")
;	      (logmsg "At depth " depth ", alpha " alpha ", beta " beta ", color " color ", cval " cval "~%")
;	      (logmsg child)
;	      )
	    
	    (when (> cval alpha)
	      (setq alpha cval))
	    (when (>= cval beta)
	      (return-from nmax alpha)))
	finally (return-from nmax alpha)))))

(defun find-negamax-moves (node)
  (let ((nm nil))
    (setq nm
	  (loop
	     for move in '(:left :up :right :down)
	     as child = (make-child-tron node move 1)
	     when child collect
	       (list move
		     (- (negamax child (1- +minimax-depth+) +defeat+  +victory+ -1))
		     child)))
    
    (sort nm #'> :key (lambda (x) (cadr x)))))
    

(defun decide-move (tron)
  (let ((moves (find-negamax-moves tron)))

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

