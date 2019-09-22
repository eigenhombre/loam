(in-package :loam)

(defgeneric write-str-at (self txt x y))
(defgeneric write-chr-at (self c x y))
(defgeneric get-screen-coords (self))
(defgeneric get-key-blocking (self))
(defgeneric get-key-nonblock (self))
(defgeneric clear (self))
(defgeneric refresh (self))

(defclass screenable () () (:documentation "A curses-ish screen"))
(defclass repl-screenable (screenable) () (:documentation "For the REPL!!!"))
(defclass curses-screenable (screenable) () (:documentation "Curses, foiled again!"))

;; REPL
(defmethod get-screen-coords ((s repl-screenable))
  (values 100 100))

(defmethod write-str-at ((s repl-screenable) txt x y)
  (format t "~d, ~d <- '~a'~%" x y txt))

(defmethod write-chr-at ((s repl-screenable) c x y)
  (format t "~d, ~d <- '~a'~%" x y c))

(defmethod get-key-blocking ((s repl-screenable))
  (format t "CHR(block) #\\.")
  #\.)

(defmethod get-key-nonblock ((s repl-screenable))
  (format t "CHR(non-block) #\\.")
  #\.)

(defmethod clear ((s repl-screenable))
  (format t "CLS~%"))

(defmethod refresh ((s repl-screenable))
  (format t "REFRESH~%"))

;; Curses
(defmethod get-screen-coords ((s curses-screenable))
  (charms:window-dimensions charms:*standard-window*))

(defmethod write-str-at ((s curses-screenable) txt x y)
  (charms:write-string-at-point charms:*standard-window* txt x y))

(defmethod write-chr-at ((s curses-screenable) c x y)
  (error "implement me"))

(defmethod get-key-blocking ((s curses-screenable))
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defmethod get-key-nonblock ((s curses-screenable))
  (error "implement me"))

(defmethod clear ((s curses-screenable))
  (charms:clear-window charms:*standard-window*))

(defmethod refresh ((s curses-screenable))
  (charms:refresh-window charms:*standard-window*))

(defun make-repl-screen () (make-instance 'repl-screenable))
(defun make-curses-screen () (make-instance 'curses-screenable))
