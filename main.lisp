(in-package :loam)

;; A game in progress

;; Many ideas and a few code snippets borrowed from Steve Losh's `silt2`:
;;    https://github.com/sjl/silt2/blob/master/silt.lisp

;; To jack in to a curses-supporting REPL, run "slimerepl" from bash,
;; then `slime-connect` to `localhost:5555`.

(defparameter *world-size* 100)

(defparameter *curses-enabled* nil)
(defparameter *screen* nil)
(defparameter *their-x* 40)
(defparameter *their-y* 40)
(defparameter *screen-width* 1)
(defparameter *screen-height* 1)
(defparameter *screen-center-x* 1)
(defparameter *screen-center-y* 1)

(clear-entities)

(define-aspect attraction charge)
(define-aspect location x y)
(define-aspect renderable)
(define-aspect ident id)
(define-entity being (ident location attraction renderable))
(defun make-being (x y)
  (create-entity 'being
                 :ident/id (random 100000)
                 :location/x x
                 :location/y y
                 :attraction/charge -10))

(defmacro comment (&rest ign)
  (declare (ignore ign)))

(defvar *beings*)

(defun init-beings (num-beings w h)
  (setf *beings*
        (loop repeat num-beings
           collect (make-being (random w)
                               (random h)))))

(defun init-random-number-generator ()
  (setf *random-state* (make-random-state t)))

(defun clamp (mn x mx)
  (cond ((< x mn) mn)
        ((> x mx) mx)
        (t x)))

(defun movdel (w h dx dy)
  (setf *their-x* (clamp 0 (+ dx *their-x*) (1- w)))
  (setf *their-y* (clamp 0 (+ dy *their-y*) (1- h))))

(defun dbug (e)
  (format t "~10,'0d ~5d ~5d ~5d ~%"
          (ident/id e)
          (location/x e)
          (location/y e)
          (attraction/charge e)))

(defmacro with-screen-dims (w h &body body)
  `(multiple-value-bind (,w ,h) (get-screen-dims *screen*)
     ,@body))

(define-system render-entities ((e renderable))
  (with-screen-dims w h
    (let ((x (location/x e))
          (y (location/y e)))
      (if (and (< x w)
               (< y h))
          (write-chr-at *screen* #\# x y)))))

(defun dist (e)
  (max (abs (- (location/x e) *their-x*))
       (abs (- (location/y e) *their-y*))))

(defun dxdy (charge x y w h)
  (let* ((sign (if (> charge 0) -1 1))
         (dx (cond ((= x *their-x*) 0)
                   ((= x 0) 0)
                   ((= x (1- w)) 0)
                   ((< x *their-x*) sign)
                   (t (- sign))))
         (dy (cond ((= y *their-y*) 0)
                   ((= y 0) 0)
                   ((= y (1- h)) 0)
                   ((< y *their-y*) sign)
                   (t (- sign)))))
    (values (rand-nth `(,dx ,dx ,dx 0 1 -1))
            (rand-nth `(,dy ,dy ,dy 0 1 -1)))))

(define-system move ((e location attraction))
  (multiple-value-bind (w h) (get-screen-dims *screen*)
    (multiple-value-bind (dx dy) (dxdy (attraction/charge e)
                                       (location/x e)
                                       (location/y e)
                                       w h)
      (setf (location/x e) (clamp 0 (+ dx (location/x e)) w))
      (setf (location/y e) (clamp 0 (+ dy (location/y e)) h))
      (when (= (dist e) 1)
        (setf (attraction/charge e) (- (attraction/charge e)))))))

(defun manage-screen ()
  (multiple-value-bind (w h) (get-screen-dims *screen*)
    (setf *screen-width* (1- w) *screen-height* (1- h)
          *screen-center-x* (floor w 2)
          *screen-center-y* (floor h 2))))

(defun write-centered (text x y)
  (etypecase text
    (string (write-centered (list text) x y))
    (list (loop
             for string in text
             for tx = (- x (floor (length string) 2))
             for ty from y
             do (write-str-at *screen* string tx ty)))))

(defmacro render (&body body)
  `(progn
     (manage-screen)
     (clear *screen*)
     ,@body
     (refresh *screen*)))

(defun render-title ()
  (render
    (write-centered '("L O A M"
                      "(apologies to Steve Losh)"
                      "Press any key to start....")
                    *screen-center-x*
                    (1- *screen-center-y*))))

(defun splash-screen ()
  (set-cursor-visibility *screen* 0)
  (render-title))

(defun wait-key-pressed ()
  (get-key-blocking *screen*))

(defun main-state ()
  (render
    (run-render-entities)
    (write-chr-at *screen* #\@ *their-x* *their-y*)
    (set-cursor-position *screen* *their-x* *their-y*))
  (loop named joe
     do (let ((kp (get-key-nonblock *screen*)))
          (with-screen-dims w h
            (case kp
              (#\q (return-from joe))
              (#\h (movdel w h -1 0))
              (#\l (movdel w h 1 0))
              (#\k (movdel w h 0 -1))
              (#\j (movdel w h 0 1))
              (#\y (movdel w h -1 -1))
              (#\b (movdel w h -1 1))
              (#\u (movdel w h 1 -1))
              (#\n (movdel w h 1 1))))
          (run-move)
          (render
            (run-render-entities)
            (write-chr-at *screen* #\@ *their-x* *their-y*)
            (set-cursor-position *screen* *their-x* *their-y*))
          (sleep 0.01))
     finally
       (return-from joe)))

(defun start-game ()
  (splash-screen)
  (set-cursor-visibility *screen* 2)
  (wait-key-pressed)
  (main-state))

(defun init-screen (really-do-curses?)
  (setf *screen*
        (if really-do-curses?
            (make-curses-screen)
            (make-repl-screen))))

(defmacro with-screen (do-curses &body body)
  `(if ,do-curses
       (progn
         (init-screen t)
         (charms:with-curses ()
           ,@body))
       (progn
         (init-screen nil)
         ,@body)))

(defun main_ (really-do-curses? num-beings)
  (init-random-number-generator)
  (with-screen really-do-curses?
    (with-screen-dims w h
      (init-beings num-beings w h))
    (start-game))
  (format t "Thanks for playing.~%"))

(defun main (&rest _)
  (declare (ignore _))
  (main_ t 100))

(comment (main_ nil 10))

