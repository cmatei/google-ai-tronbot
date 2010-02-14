
(proclaim '(optimize (speed 3) (safety 1) (debug 3)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 100)
(defparameter +defeat+ -100)
(defparameter +draw+    -50)

(declaim (type fixnum +minimax-depth+))
(defparameter +minimax-depth+ 8)

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
			(the fixnum (ff-area (1- x) y))
			(the fixnum (ff-area x (1- y)))
			(the fixnum (ff-area (1+ x) y))
			(the fixnum (ff-area x (1+ y))))))))

	(the fixnum (+ (the fixnum (ff-area (1- x) y))
		       (the fixnum (ff-area x (1- y)))
		       (the fixnum (ff-area (1+ x) y))
		       (the fixnum (ff-area x (1+ y))))))))
  

;; (defparameter tt
;; (with-input-from-string (*input*
;; "5 5
;; #####
;; # 1##
;; #2###
;; #   #
;; #####
;; ")
;;   (read-tron (make-tron))))
;; 
;; (defparameter tt
;;   (with-open-file (*input* "maps/u.txt")
;;     (read-tron (make-tron))))
;; 
;; (time (count-reachables tt -1))

(defun node-value (node color)
  (let ((reachable (count-reachables node color))
	(area (tron-area node)))
    (the fixnum (truncate (* +victory+
			     (/ reachable area))))))

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
	    (when (> cval alpha)
	      (setq alpha cval))
	    (when (> alpha beta)
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

