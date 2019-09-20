(defpackage loam
  (:use :cl :charms :cl-oju))

(in-package :loam)

;; To jack in, run "slimerepl" from bash, then
;; `slime-connect` to `localhost:5555`.

(defun ch-char (char_ x y)
  (charms:write-string-at-point charms:*standard-window* char_ x y))

(defmacro with-game-loop (w h &rest body)
  `(charms:with-curses ()
     (charms:disable-echoing)
     (multiple-value-bind
           (,w ,h)
         (charms:window-dimensions charms:*standard-window*)
       ,@body
       (loop named poll-kbd
          do (progn
               (when (eql (charms:get-char charms:*standard-window*) #\q)
                 (return-from poll-kbd))
               (sleep 0.01))))))

(defun write-chars-and-poll ()
  (with-game-loop
      width height
      (loop repeat 10
         do (let ((x (random width))
                  (y (random height)))
              (ch-char (rand-nth '(";" "\"" "'")) x y)
              (charms:refresh-window charms:*standard-window*)))
      (ch-char "@" (floor (/ width 2)) (floor (/ height 2)))))

(defun main (&rest args)
  (write-chars-and-poll))
