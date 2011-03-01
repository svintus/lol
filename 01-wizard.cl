;; Wizard's House Adventure
;;
;; Locations:
;; Attic
;; Living Room
;; Garden

;; DATA

(defparameter *nodes*
  '((living-room (you are in a living room.
                      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
                 there is a well in front of you.))
    (attic (you are in the attic.
                there is a giant welding torch in the corner.))))


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

;; FUNCTIONS

(defun describe-location (location nodes)ctacti
  (cadr (assoc location nodes)))


(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;; Doesn't really use the objs param
(defun objects-at (loc objs obj-locs)
  (mapcar #'car
          (remove-if-not (lambda (obj-loc) (eq (cadr obj-loc) loc))
                           obj-locs)))


(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
                         `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj
                            (objects-at loc objs obj-locs)))))

