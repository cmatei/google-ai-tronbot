
(proclaim '(optimize (speed 3) (safety 3) (debug 3)))

(load "Map.lisp")

(in-package :my-tron-bot)

(declaim (type fixnum +victory+ +defeat+ +draw+))
(defparameter +victory+ 5000)

;; ok this is weird, but it bumped me up 50 places
;(defparameter +defeat+ 0)

(defparameter +defeat+ -5000)

(defparameter +draw+   -10)

(declaim (type fixnum +minimax-depth+))
;; this is safe
;;(defparameter +minimax-depth+ 9)
(defparameter +minimax-depth+ 9)

(defparameter +max-time+ 300)

(setq *verbose* nil)

(defun final-value-p (value)
  (or (= value +victory+)
      (= value +defeat+)
      (= value +draw+)))

(defun dir-and-score (moves)
  (mapcar #'(lambda (x) (list (car x) (cadr x))) moves))

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

;;    (if (and (zerop reach1) (zerop reach2))
;;	+draw+
;;	(the fixnum (- reach1 reach2)))

    (the fixnum (- reach1 reach2))))


(defun negamax (node depth alpha beta color)
  (labels ((nmax-loop (node depth color alpha beta)
	     (if (zerop depth)
		 (node-value node)
		 (loop
		    named nmax
		    for move in '(:left :up :right :down)
		    as child = (make-child-tron node move color)
	            ; if (not child) do
		    ; (logmsg "DEPTH: " depth ", " color " can't move " move "~%")
		    ; else do
		    when child do
		      (let ((cval (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
					;	    (when (= depth 8)
					;	      (logmsg "-----------~%~%")
					;	      (logmsg "At depth " depth ", alpha " alpha ", beta " beta ", color " color ", cval " cval "~%")
					;	      (logmsg child)
					;	      )
			
			(when (> cval alpha)
			  (setq alpha cval))
			(when (>= cval beta)
			  (return-from nmax alpha)))
		    finally (return-from nmax alpha)))))

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

      (when (final-value-p val)
	(logmsg "At depth " depth ", color " color ", alpha " alpha ", beta " beta ", val " val "~%")
	(logmsg node "~%------~%"))
      val)))

;;   (defun negamax (node depth alpha beta color)
;;     (declare (type fixnum depth alpha beta color))
;;   
;;     (cond
;;       ((and (= color 1)
;;   	  (or (equal (tron-p1 node) (tron-p2 node))
;;   	      (and (player-stuck-p node 1)
;;   		   (player-stuck-p node -1))))
;;   	  
;;        +draw+)
;;   
;;   ;;    ((and (= color 1)
;;   ;;	  (player-stuck-p node -1))
;;   ;;     +victory+)
;;   
;;   ;;    ((and (= color 1)
;;   ;;	  (player-stuck-p node 1))
;;   ;;     +defeat+)
;;   
;;       ((zerop depth)
;;        (* color (node-value node)))
;;       
;;       (t
;;        (loop
;;   	named nmax
;;   	for move in '(:left :up :right :down)
;;   	as child = (make-child-tron node move color)
;;   ;	if (not child) do
;;   ;	  (logmsg "DEPTH: " depth ", " color " can't move " move "~%")
;;   ;	else do
;;   	when child do
;;   	  (let ((cval (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
;;   ;	    (when (= depth 8)
;;   ;	      (logmsg "-----------~%~%")
;;   ;	      (logmsg "At depth " depth ", alpha " alpha ", beta " beta ", color " color ", cval " cval "~%")
;;   ;	      (logmsg child)
;;   ;	      )
;;   
;;   	    (when (> cval alpha)
;;   	      (setq alpha cval))
;;   	    (when (>= cval beta)
;;   	      (return-from nmax alpha)))
;;   	finally (return-from nmax alpha)))))


(defun find-negamax-moves (node)
  (let ((start-depth 4)
	(start-time (get-internal-real-time)))
    (labels ((time-remaining ()
	       (< (- (get-internal-real-time) start-time)
		  +max-time+))
	     
	     (reached-final (moves)
	       (or (null moves)
		   (= (cadar moves) +victory+)
		   (= (cadar moves) +defeat+)
		   (= (cadar moves) +draw+)))

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

      (let ((val nil))
	(loop
	   for depth fixnum = start-depth then (incf depth 2)
	   do (setq val (find-moves depth))
	   until (or
		  (reached-final val)		  
		  (not (time-remaining))
		  (> depth 500))
	   finally (logmsg "DEPTH: " depth ", val " (dir-and-score val) "~%"))
	val))))
    

;;    (defun decide-move (tron)
;;      (let ((moves nil))
;;    
;;    ;    (time (setq moves (find-negamax-moves tron)))
;;        (setq moves (find-negamax-moves tron))
;;        
;;        (mapcar (lambda (x) (logmsg (car x) " " (cadr x) ",  ")) moves)
;;    ;    (logmsg "~%")
;;    ;;     
;;    ;;     ;; from equivalent moves, choose the one that brings us closer
;;        (setq moves (remove-if-not (lambda (x) (equal (cadr x) (cadar moves)))
;;     			       moves))
;;    ;; 
;;    ;     (mapcar (lambda (x) (logmsg (car x) " " (cadr x) ",  ")) moves)
;;    ;    (logmsg "~%~%")
;;    ;;     
;;        (setq moves (sort moves #'< :key (lambda (x) (player-distance (caddr x)))))
;;    
;;        (if moves (caar moves) ':left)))

(defun rec-fill-move (node depth)
  (cond ((player-stuck-p node 1)
	 +defeat+)
	((zerop depth)
	 (count-reachables node 1))
	(t
	 (let ((best +defeat+))
	   (loop
	      for move in '(:left :up :right :down)
	      as child = (make-child-tron node move 1)
	      when child
	      do (let ((cval (rec-fill-move child (1- depth))))
		   (when (> cval best)
		     (setq best cval))))
	   best))))

(defun find-fill-move (node depth)
  (loop
     for m in '(:left :up :right :down)
     as child = (make-child-tron node m 1)
     when child collect (list m (rec-fill-move node (1- depth)))))
  

(defun decide-move (tron)
  (let* ((start-time (get-internal-real-time))
	 (moves (find-negamax-moves tron))
	 (end-time (get-internal-real-time)))

    (logmsg "Took " (- end-time start-time) "~%")
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

