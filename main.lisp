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

(defparameter *beings*
  (loop repeat 15
     collect (make-being (random *world-size*)
                         (random *world-size*))))

(defun init-random-number-generator ()
  (setf *random-state* (make-random-state t)))

(defun movdel (w h dx dy)
  (setf *their-x* (min (1- w) (max 0 (+ dx *their-x*))))
  (setf *their-y* (min (1- h) (max 0 (+ dy *their-y*)))))

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

(define-system move ((e location attraction))
  (let* ((sign (if (> (attraction/charge e) 0) -1 1))
         (dx (cond ((= (location/x e) *their-x*) 0)
                   ((= (location/x e) 0) 0)
                   ((= (location/x e) (1- *world-size*)) 0)
                   ((< (location/x e) *their-x*) sign)
                   (t (- sign))))
         (dy (cond ((= (location/y e) *their-y*) 0)
                   ((= (location/y e) 0) 0)
                   ((= (location/y e) (1- *world-size*)) 0)
                   ((< (location/y e) *their-y*) sign)
                   (t (- sign)))))
    (setf (location/x e) (+ dx (location/x e)))
    (setf (location/y e) (+ dy (location/y e)))
    (when (= (dist e) 1)
      (setf (attraction/charge e) (- (attraction/charge e))))))


;; TBD: Bring this back into the new framework

;; (defun draw-world ()
;;   (run-render-entities)
;;   (charms:write-char-at-point charms:*standard-window*
;;                               #\@
;;                               *their-x*
;;                               *their-y*)
;;   (charms/ll:curs-set 0))

;; (defun poll-till-done ()
;;   (loop named poll-kbd
;;      do (progn
;;           (run-move)
;;           (draw-world)
;;           (with-screen-dims w h
;;             (case (charms:get-char charms:*standard-window*)
;;               (#\q (return-from poll-kbd))
;;               (#\h (movdel w h -1 0))
;;               (#\l (movdel w h 1 0))
;;               (#\k (movdel w h 0 -1))
;;               (#\j (movdel w h 0 1))
;;               (#\y (movdel w h -1 -1))
;;               (#\b (movdel w h -1 1))
;;               (#\u (movdel w h 1 -1))
;;               (#\n (movdel w h 1 1))))
;;           (sleep 0.1))))

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
                      "Press any key to start...")
                    *screen-center-x*
                    (1- *screen-center-y*))))

(defun splash-screen ()
  (render-title))

(defun wait-key-pressed ()
  (get-key-blocking *screen*))

(defun main-state ()
  (render
    (run-render-entities))
  (sleep 1.5))

(defun start-game ()
  (splash-screen)
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

(defun main_ (really-use-curses-p)
  (init-random-number-generator)
  (with-screen really-use-curses-p
    (start-game)))

(defun main (&rest _)
  (declare (ignore _))
  (main_ t))

(comment
 (main_ nil))
