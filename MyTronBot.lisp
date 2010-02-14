
(proclaim '(optimize (speed 3) (safety 3) (debug 3)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)
(defparameter +defeat+ -5000)
(defparameter +draw+   -2500)

(declaim (type fixnum +minimax-depth+))
(defparameter +minimax-depth+ 5)

(setq *verbose* t)

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

(defun node-value (node color)
  (let ((reach1 (count-reachables node color))
	(reach2 (count-reachables node (- color)))
	(area (tron-area node)))
    (let ((val (- reach1 reach2)))
      (setq val (the fixnum (truncate val)))
;      (logmsg node)
;    (let ((val (the fixnum (truncate (* +victory+
;				       (/ reachable area))))))
      (logmsg "Node value " (list val reach1 reach2) "~%")
      val)))

(defun negamax (node depth alpha beta color)
  (declare (type fixnum depth alpha beta color))
  (cond
    ((victory-p node color)
;     (format t "victory for ~a~%" color)
     (the fixnum (* color +victory+)))
    
    ((defeat-p node color)
;     (format t "defeat for ~a~%" color)
     (the fixnum (* color +defeat+)))
    
    ((draw-p node color)
;     (format t "draw!~%")
     (the fixnum (* color +draw+)))
    
    ((zerop depth)
     (the fixnum (* color (the fixnum (node-value node color)))))
    
    (t
     (loop
	named nmax
	for move in '(:left :up :right :down)
	as child = (make-child-tron node move color)
	when child do
	  (let ((cval (negamax child (1- depth) (- beta) (- alpha) (- color))))
	    (when (= depth 1)
	      (logmsg child)
	      (logmsg "At depth 1 negamax " cval ", alpha " alpha
		      ", beta " beta ", color " color "~%"))
	    (when (> cval alpha)
	      (setq alpha cval))
	    (when (>= alpha beta)
;	      (format t "prunning, alpha ~a, beta ~a, color ~a~%" alpha beta color)
;	      (print child)
	      (return-from nmax alpha))
	    )
	finally (return-from nmax alpha)))))

(defun find-negamax-move (node)
  (let* ((alpha +defeat+)
	 (beta +victory+)
	 (color 1)
	 (depth +minimax-depth+))
    
    (declare (type fixnum alpha beta color depth))
    (loop
       for move in '(:left :up :right :down)
       as child = (make-child-tron node move 1)
       when child do
	 (let ((cval (the fixnum (negamax
				  child
				  (the fixnum (1- depth))
				  (the fixnum (- beta))
				  (the fixnum (- alpha))
				  (the fixnum (- color))))))
	    (when (> cval alpha)
	      (setq alpha cval)))
       and collect (list move alpha))))


(defun decide-move (tron)
  (let ((nm (find-negamax-move tron)))
    (setq nm (sort nm #'> :key (lambda (x) (cadr x))))
    (logmsg "nm " nm "~%")
    (if nm (caar nm) ':left)))

;(defparameter tt (with-open-file (*input* "test.txt")
;		   (read-tron (make-tron))))


;;; driver loop

(defun main ()
  (loop
     with tron = (make-tron)
     while (peek-char nil *input* nil nil)
     for move from 0
     do
       (read-tron tron)
       (make-move (decide-move tron))))

