
(defpackage :my-tron-bot
  (:use :cl))

(in-package :my-tron-bot)

(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)

(defstruct tron
  (map nil)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (p1 nil)
  (p2 nil))

(defun tron-area (tron)
  (the fixnum (* (tron-width tron)
		 (tron-height tron))))

(defun x-of (tron color)
  (if (= color 1)
      (car (tron-p1 tron))
      (car (tron-p2 tron))))

(defun y-of (tron color)
  (if (= color 1)
      (cadr (tron-p1 tron))
      (cadr (tron-p2 tron))))

(declaim (inline empty-square-p))
(defun empty-square-p (map x y)
  (declare (type (simple-array character (* *)) map)
	   (type fixnum x y))
  (equal (aref map x y) #\space))

(declaim (inline player-stuck-p))
(defun player-stuck-p (tron player)
  (let ((map (tron-map tron))
	(x (x-of tron player))
	(y (y-of tron player)))
    (declare (type fixnum x y))
    (not (or (empty-square-p map (the fixnum (1- x)) y)
	     (empty-square-p map (the fixnum (1+ x)) y)
	     (empty-square-p map x (the fixnum (1- y)))
	     (empty-square-p map x (the fixnum (1+ y)))))))

(declaim (inline victory-p))
(defun victory-p (tron player)
  (declare (type fixnum player))
  (and (not (player-stuck-p tron player))
       (player-stuck-p tron (the fixnum (- player)))))

(declaim (inline defeat-p))
(defun defeat-p (tron player)
  (declare (type fixnum player))
  (and (player-stuck-p tron player)
       (not (player-stuck-p tron (the fixnum (- player))))))

(declaim (inline drawp-p))
(defun draw-p (tron player)
  (declare (type fixnum player))
  (and (player-stuck-p tron player)
       (player-stuck-p tron (the fixnum (- player)))))

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
     with map = (tron-map tron)
     repeat (the fixnum (tron-height tron))
     for y fixnum from 0
     do
       (loop
	   for c character across (read-line *input* nil nil)
	   for x fixnum from 0
	   do (setf (aref (the (simple-array character (* *)) map) x y) c)
	     (case c
	       (#\1 (setf (tron-p1 tron) (list x y)))
	       (#\2 (setf (tron-p2 tron) (list x y))))))
  tron)


(defun make-move (dir)
  (case dir
    (:north (princ "1" *output*))
    (:east  (princ "2" *output*))
    (:south (princ "3" *output*))
    (:west  (princ "4" *output*))
    (:up    (princ "1" *output*))
    (:right (princ "2" *output*))
    (:down  (princ "3" *output*))
    (:left  (princ "4" *output*)))
  (terpri *output*)
  (force-output *output*))

;; utilities

(defun copy-tron-map (arr)
  (let* ((w (the fixnum (car  (array-dimensions arr))))
	 (h (the fixnum (cadr (array-dimensions arr))))
	 (narr (make-array (list w h)
			   :element-type 'character
			   :adjustable nil
			   :fill-pointer nil
			   :displaced-to nil))
	 (darr (make-array (the fixnum (* w h))
			   :element-type 'character
			   :displaced-to arr))
	 (ndarr (make-array (the fixnum (* w h))
			    :element-type 'character
			    :displaced-to narr)))

    (dotimes (i (the fixnum (* w h)))
      (setf (aref ndarr i) (aref darr i)))

    narr))

(defun make-child-tron (node move color)
  (declare (type tron node)
	   (type fixnum color))

  (let* ((pos (if (= color 1) (tron-p1 node) (tron-p2 node)))
	 (x (car pos))
	 (y (cadr pos)))
    (declare (type fixnum x y))

    (case move
      (:left  (decf x))
      (:right (incf x))
      (:up    (decf y))
      (:down  (incf y)))
    
    (if (not (empty-square-p (tron-map node) x y))
	nil
	(let* ((newtron (make-tron))
	       (map (copy-tron-map (tron-map node))))
	  (declare (type (simple-array character (* *)) map))
	  
	  (setf (aref map x y) (if (= color 1) #\1 #\2))
	  
	  (setf (tron-map newtron) map)

	  (setf (tron-width newtron) (tron-width node))
	  (setf (tron-height newtron) (tron-width node))

	  (setf (tron-p1 newtron)
		(if (= color 1)
		    (list x y)
		    (tron-p1 node)))

	  (setf (tron-p2 newtron)
		(if (= color 1)
		    (tron-p2 node)
		    (list x y)))

	  newtron))))

(defun tron-map-string (arr)
  (declare (type (simple-array character (* *)) arr))

  (with-output-to-string (*output*)
    (destructuring-bind (w h) (array-dimensions arr)
      (declare (type fixnum w h))
      (loop
	 for i from 0 below h
	 do (progn
	      (loop
		 for j from 0 below w
		 do
		   (princ (aref arr j i) *output*))
	      (terpri *output*))))
     *output*))

(defmethod print-object ((tron tron) s)
  (declare (type stream s))
  (format s "~%~a" (tron-map-string (tron-map tron))))
    
	 