;;; A game engine for a simple text-based game

(defparameter *nodes* '((living-room (you are in the living-room.
                                      A wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                 There is a well in front of you.))
                        (attic (you are in the attic.
                                There is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-location (location nodes)
  (first (rest (assoc location nodes))))

;; `edge` is formatted as (location direction object)
(defun describe-path (edge)
  `(there is a ,(third edge) going ,(second edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (rest (assoc location edges)))))
