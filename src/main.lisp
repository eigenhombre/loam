(defpackage loam
  (:use :cl :charms :cl-oju))

(in-package :loam)

;; To jack in, run "slimerepl" from bash, then
;; `slime-connect` to `localhost:5555`.

(defparameter *world-size* 100)

(defun make-world ()
  (make-array (list *world-size* *world-size*)
              :element-type 'character
              :initial-element #\space
              :adjustable t))

(defvar *world* (make-world))
(defparameter *their-x* 10)
(defparameter *their-y* 10)

(defun initialize-world ()
  (loop repeat 50
     do (let* ((x (random *world-size*))
               (y (random *world-size*))
               (ch (rand-nth '(#\^ #\" #\' #\. #\# #\*))))
          (setf (aref *world* x y) ch))))

(defmacro with-screen-dims (w h &rest body)
  `(multiple-value-bind
         (,w ,h)
       (charms:window-dimensions charms:*standard-window*)
     ,@body))

(defun draw-world (w h)
  (loop for x from 0 below w
     do (loop for y from 0 below h
           do
             (if (and (< x w)
                      (< x *world-size*)
                      (< y h)
                      (< y *world-size*))
                 (charms:write-char-at-point
                  charms:*standard-window*
                  (aref *world* x y) x y))))
  (charms:write-char-at-point charms:*standard-window*
                              #\@
                              *their-x*
                              *their-y*)
  (charms/ll:curs-set 0))

(defun movdel (w h dx dy)
  (setf *their-x* (min (1- w) (max 0 (+ dx *their-x*))))
  (setf *their-y* (min (1- h) (max 0 (+ dy *their-y*)))))

(defun poll-till-done ()
  (loop named poll-kbd
     do (progn
          (with-screen-dims w h
            (draw-world w h)
            (case (charms:get-char charms:*standard-window*)
              (#\q (return-from poll-kbd))
              (#\h (movdel w h -1 0))
              (#\l (movdel w h 1 0))
              (#\k (movdel w h 0 -1))
              (#\j (movdel w h 0 1))
              (#\y (movdel w h -1 -1))
              (#\n (movdel w h -1 1))
              (#\i (movdel w h 1 -1))
              (#\m (movdel w h 1 1))))
          (sleep 0.01))))

(defmacro with-charms (&rest body)
  `(charms:with-curses () ,@body))

(defun show-game (&key poll)
  (with-charms
    (charms:clear-window charms:*standard-window*)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys charms:*standard-window*)
    (with-screen-dims w h (draw-world w h))
    (when poll (poll-till-done))))

;;(floor (/ width 2))

(defun main (&rest args)
  (setf *random-state* (make-random-state t))
  (initialize-world)
  (show-game :poll t))

(defmacro comment (&rest body))
(comment
 (main)

 )
