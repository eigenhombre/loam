(in-package :loam)

(defgeneric write-str-at (self txt x y))
(defgeneric write-chr-at (self c x y))
(defgeneric get-screen-dims (self))
(defgeneric get-key-blocking (self))
(defgeneric get-key-nonblock (self))
(defgeneric clear (self))
(defgeneric refresh (self))
(defgeneric set-cursor-visibility (self visibility))
(defgeneric set-cursor-position (self x y))

(defclass screenable () () (:documentation "A curses-ish screen"))
(defclass repl-screenable (screenable) () (:documentation "For the REPL!!!"))
(defclass curses-screenable (screenable) () (:documentation "Curses, foiled again!"))

(defun make-repl-screen () (make-instance 'repl-screenable))
(defun make-curses-screen () (make-instance 'curses-screenable))

;; REPL
(defmethod get-screen-dims ((s repl-screenable))
  (values 100 100))

(defmethod write-str-at ((s repl-screenable) txt x y)
  (format t "~d, ~d <- '~a'~%" x y txt))

(defmethod write-chr-at ((s repl-screenable) c x y)
  (format t "~d, ~d <- '~a'~%" x y c))

(defmethod get-key-blocking ((s repl-screenable))
  (let ((ret (rand-nth '(#\. #\. #\. #\. #\. #\. #\q))))
    (format t "CHR(block) ~a~%" ret)
    ret))

(defmethod get-key-nonblock ((s repl-screenable))
  (let ((ret (rand-nth '(#\. #\. #\. #\. #\. #\. #\q))))
    (format t "CHR(non-block) ~a~%" ret)
    ret))

(defmethod clear ((s repl-screenable))
  (format t "CLS~%"))

(defmethod refresh ((s repl-screenable))
  (format t "REFRESH~%"))

(defmethod set-cursor-visibility ((s repl-screenable) visibility)
  (format t "CURS-SET ~a~%" visibility))

(defmethod set-cursor-position ((s repl-screenable) x y)
  (format t "CURS-MOV ~a, ~a~%" x y))

;; Curses
(defmethod get-screen-dims ((s curses-screenable))
  (charms:window-dimensions charms:*standard-window*))

(defmethod write-str-at ((s curses-screenable) txt x y)
  (charms:write-string-at-point charms:*standard-window* txt x y))

(defmethod write-chr-at ((s curses-screenable) c x y)
  (charms:write-char-at-point charms:*standard-window* c x y))

(defmethod get-key-blocking ((s curses-screenable))
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*)
  (charms:enable-non-blocking-mode charms:*standard-window*))

(defmethod get-key-nonblock ((s curses-screenable))
  (charms:get-char charms:*standard-window* :ignore-error t))

(defmethod clear ((s curses-screenable))
  (charms:clear-window charms:*standard-window*))

(defmethod refresh ((s curses-screenable))
  (charms:refresh-window charms:*standard-window*))

(defmethod set-cursor-visibility ((s curses-screenable) visibility)
  (charms/ll:curs-set visibility))

(defmethod set-cursor-position ((s curses-screenable) x y)
  (charms:move-cursor charms:*standard-window* x y))
