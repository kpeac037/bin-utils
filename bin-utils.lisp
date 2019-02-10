;;;;Wrote some utilities while working on an incredibly boring Digital Systems assignment so that I don't neck myself part way through. Here's the results.
;;;;I intend to use them myself for emulation projects down the road. Given that there are probably a few efficieny issues, those will be ironed out as I require better solutions.
;;;;I also expect a few bugs to crop up. What can I say, I'm new to this.

;;To-Do:
;;    Export list
;;    Floating point operations

(defpackage :bin-utils
  (:use :cl))
(in-package :bin-utils)

;;Standard range utility
(defun range (f stop &optional (b 1))
  (loop for i from f to stop by b
     collect i))


;;Function that most of this library revolves around. Given any
;;integer input, it will return a BCD list represention of that
;;input.
;;Pass it to (MAPPER INT BASE) for a string result with
;;proper formating
(defun base (int &optional (base 2))
  "Return a BCD list for a given INT in particular BASE"
  (labels ((acker (current acc)
             (if (zerop current)
                 acc
                 (acker (floor current base) (push (rem current base) acc)))))
    (acker int '())))

;;Return the string representation of a BCD list given by
;;the function (BASE INT BASE).
;;Only works up to base-16, but can easily be extended
(defun mapper (base-list)
  "Return a properly formatted string given a BCD representation"
  (let ((vals '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)))
    (labels ((stringify (current acc)
               (if current
                   (stringify (cdr current) (concatenate 'string (string (nth (car current) vals)) acc))
                   (reverse acc))))
      (stringify base-list ""))))

;;Utility function combining MAPPER and BASE.
;;Use this for most higher base conversions
(defun pretty-base (int &optional (base 2))
  "Return a higher base STRING given a decimal INT"
  (mapper (base int base)))

;;Essentially the reverse operation provided by BASE
;;Spit out an integer given a string
(defun reverse-base (base-rep &optional (base 2))
  "Return the INT given a representation returned by BASE"
  (let ((pow (length base-rep)))
    (labels ((base-reccer (current acc)
               (decf pow)
               (if (null (cdr current))
                   (+ acc (* (expt base pow)) (car current))
                   (base-reccer (cdr current) (+ acc (* (expt base pow) (car current)))))))
      (base-reccer base-rep (1+ pow)))))

(defun reverse-mapper (input &optional (base 2))
  "Given some input string, return the INT value"
  (let ((vals (mapcar #'(lambda (x y) ; Create an ASSOC list for 
                          (cons x y)) ; quick lookup.
                      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)
                      (range 0 16))))
    (reverse-base (loop for char across input
                     collect (cdr (assoc char vals)))
                  base)))


(defun reverse-base (base-rep &optional (base 2))
  (let ((pow (length base-rep)))       ; Sum a list which
    (reduce #'+                        ; is the evaluated
            (mapcar #'(lambda (x)      ; BCD representation
                        (decf pow)
                        (* (expt base pow) x))
                    base-rep))))
