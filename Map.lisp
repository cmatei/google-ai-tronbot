
(defpackage :my-tron-bot
  (:use :cl))

(in-package :my-tron-bot)

(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)

(defparameter *verbose* nil)
(defparameter *log* nil)

(defstruct tron
  (map nil)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (x1 0 :type fixnum)
  (y1 0 :type fixnum)  
  (x2 0 :type fixnum)
  (y2 0 :type fixnum))

(declaim (inline empty-square-p))
(defun empty-square-p (map x y)
  (declare (type (simple-array character (* *)) map)
	   (type fixnum x y))
  (equal (aref map x y) #\space))

(declaim (inline player-stuck-p))
(defun player-stuck-p (tron player)
  (declare (type fixnum player))
  (let ((map (tron-map tron))
	(x (if (= player 1)
	       (tron-x1 tron)
	       (tron-x2 tron)))
	(y (if (= player 1)
	       (tron-y1 tron)
	       (tron-y2 tron))))

    (declare (type fixnum x y))
    (not (or (empty-square-p map (1- x) y)
	     (empty-square-p map (1+ x) y)
	     (empty-square-p map x (1- y))
	     (empty-square-p map x (1+ y))))))


(defun set-tron-size (tron line)
  (let ((sp (position #\space line)))
    (setf (tron-width tron) (parse-integer (subseq line 0 sp))
	  (tron-height tron) (parse-integer (subseq line sp)))))
	  

(defun read-tron (tron)
  (if (or (zerop (tron-width tron))
	  (zerop (tron-height tron)))
    (set-tron-size tron (read-line *input* nil nil))
    (read-line *input* nil))
  
  (unless (tron-map tron)
    (setf (tron-map tron)
	  (make-array (list (tron-width tron) (tron-height tron))
		      :element-type 'character
		      :adjustable nil
		      :fill-pointer nil
		      :displaced-to nil)))
  (loop
     repeat (tron-height tron)
     for y fixnum from 0
     do (loop
	   for c character across (read-line *input* nil nil)
	   for x fixnum from 0
	   do (setf (aref (tron-map tron) x y) c)	    
	     (case c
	       (#\1 (setf (tron-x1 tron) x
			  (tron-y1 tron) y))
	       (#\2 (setf (tron-x2 tron) x
			  (tron-y2 tron) y)))))
  tron)


(defun emit-move (dir)
  (case dir
    (:up    (princ "1" *output*))
    (:right (princ "2" *output*))
    (:down  (princ "3" *output*))
    (:left  (princ "4" *output*)))
  (terpri *output*)
  (force-output *output*))


(declaim (inline can-move-to))
(defun can-move-to (node color x y)
  (declare (type fixnum color x y))
  ;; I can only move to empty squares, but the opponent can then come
  ;; over me, since I move "first"
  (if (= color 1)
      (empty-square-p (tron-map node) x y)
      (or (empty-square-p (tron-map node) x y)
	  (and (= (tron-x1 node) x)
	       (= (tron-y1 node) y)))))
  
(defun make-child-tron (node move color)
  (declare (type tron node)
	   (type fixnum color))

  (let* ((x (if (= color 1) (tron-x1 node) (tron-x2 node)))
	 (y (if (= color 1) (tron-y1 node) (tron-y2 node))))

    (declare (type fixnum x y))

    (case move
      (:left  (decf x))
      (:right (incf x))
      (:up    (decf y))
      (:down  (incf y)))

    (if (not (can-move-to node color x y))
	nil
	(let* ((newtron (make-tron))
	       (nmap (copy-tron-map node)))
	  (declare (type (simple-array character (* *)) nmap))
	  
	  (setf (aref nmap x y) (if (= color 1) #\1 #\2))
	  
	  (setf (tron-map newtron) nmap)

	  (setf (tron-width newtron) (tron-width node))
	  (setf (tron-height newtron) (tron-height node))

	  (if (= color 1)
	      (setf (tron-x1 newtron) x
		    (tron-y1 newtron) y
		    (tron-x2 newtron) (tron-x2 node)
		    (tron-y2 newtron) (tron-y2 node))
	      (setf (tron-x2 newtron) x
		    (tron-y2 newtron) y
		    (tron-x1 newtron) (tron-x1 node)
		    (tron-y1 newtron) (tron-y1 node)))

	  newtron))))


(defun make-tron-movement (tron move color)
  (declare (type fixnum color))

  (let ((x (if (= color 1) (tron-x1 tron) (tron-x2 tron)))
	(y (if (= color 1) (tron-y1 tron) (tron-y2 tron))))

    (case move
      (:left  (decf x))
      (:right (incf x))
      (:up    (decf y))
      (:down  (incf y)))

;    (format t "would move ~a to ~a, ~a~%" color x y)
;    (format t "from: ~a,~a ~a,~a~%"
;	    (tron-x1 tron) (tron-y1 tron)
;	    (tron-x2 tron) (tron-y2 tron))
    (if (not (can-move-to tron color x y))
	nil
	(progn
	  (if (= color 1)
	      (setf (tron-x1 tron) x (tron-y1 tron) y)
	      (setf (tron-x2 tron) x (tron-y2 tron) y))

;	  (format t "moved ~a,~a ~a,~a~%"
;		  (tron-x1 tron) (tron-y1 tron)
;		  (tron-x2 tron) (tron-y2 tron))

	  (setf (aref (tron-map tron) x y)
		(if (= color 1) #\1 #\2))
	  t))))
	  

(defun undo-tron-movement (tron move color)
  (declare (type fixnum color))
  (setf (aref (tron-map tron)
	      (if (= color 1) (tron-x1 tron) (tron-x2 tron))
	      (if (= color 1) (tron-y1 tron) (tron-y2 tron))) #\space)
	      
  (case move
    (:left  (if (= color 1) (incf (tron-x1 tron)) (incf (tron-x2 tron))))
    (:right (if (= color 1) (decf (tron-x1 tron)) (decf (tron-x2 tron))))
    (:up    (if (= color 1) (incf (tron-y1 tron)) (incf (tron-y2 tron))))
    (:down  (if (= color 1) (decf (tron-y1 tron)) (decf (tron-y2 tron)))))

  (setf (aref (tron-map tron) (tron-x1 tron) (tron-y1 tron)) #\1)
  (setf (aref (tron-map tron) (tron-x2 tron) (tron-y2 tron)) #\2))
  
	       

(defmethod print-object ((tron tron) s)
  (format s "1: ~a,~a 2: ~a,~a~%"
	  (tron-x1 tron) (tron-y1 tron)
	  (tron-x2 tron) (tron-y2 tron))
  (loop
     for y fixnum from 0 below (tron-height tron)
     do (progn
	  (loop
	     for x from 0 below (tron-width tron)
	     do (princ (aref (tron-map tron) x y) s))
	  (terpri s))))
    
	 
(defun logmsg (&rest args)
  (when (and *verbose*
	     (not *log*))
    (setf *log* (open "sbcl.log"
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create))
    (setf *trace-output* *log*))
  
  (when *verbose*
    (format *log* (with-output-to-string (s) (dolist (a args) (princ a s))))
    (force-output *log*)))
