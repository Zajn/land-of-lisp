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

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
  (first (rest (assoc location nodes))))

;; `edge` is formatted as (location direction object)
(defun describe-path (edge)
  `(there is a ,(third edge) going ,(second edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (rest (assoc location edges)))))

(defun objects-at (loc objs obj-locations)
  (labels ((at-loc-p (obj)
             (eq (second (assoc obj obj-locations)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locations)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locations)))))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (rest (assoc *location* *edges*))
                    :key #'second)))
    (if next
        (progn (setf *location* (first next))
               (look))
        '(you cannot go that way))))

(defun pickup (obj)
  (cond ((member obj
                 (objects-at *location* *objects* *object-locations*))
         (push (list obj 'body) *object-locations*)
         `(you are now carrying the ,obj))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
