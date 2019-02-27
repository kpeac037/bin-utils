;;;;Wrote some utilities while working on an incredibly boring Digital Systems assignment so that I don't neck myself part way through. Here's the results.
;;;;I intend to use them myself for emulation projects down the road. Given that there are probably a few efficieny issues, those will be ironed out as I require better solutions.
;;;;I also expect a few bugs to crop up. What can I say, I'm new to this.

;;To-Do:
;;    Floating point operations
(defpackage :bin-utils
  (:use :cl)
  (:export #:base
           #:mapper
           #:pretty-base
           #:mapbin
           #:reverse-mapper
           #:reverse-base
           #:binary-stream
           #:read-all
           #:read-x-bytes
           #:hex-dump))

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
    ;;I realize I totally neglected if INT was equal to 0. NIL
    ;;is returned when this is the case. Here's the fix for now.
    (if (zerop int)
        (acker int '(0))
        (acker int '()))))

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

(defun mapbin (list &optional (base 2))
  "Maps pretty-base over a list of ints, frequently returned by other functions"
  (mapcar #'(lambda (x) (pretty-base x base)) list))

;; ;;Essentially the reverse operation provided by BASE
;; ;;Spit out an integer given a string
;; (defun reverse-base (base-rep &optional (base 2))
;;   "Return the INT given a representation returned by BASE"
;;   (let ((pow (length base-rep)))
;;     (labels ((base-reccer (current acc)
;;                (decf pow)
;;                (if (null (cdr current))
;;                    (+ acc (* (expt base pow)) (car current))
;;                    (base-reccer (cdr current) (+ acc (* (expt base pow) (car current)))))))
;;       (base-reccer base-rep (1+ pow)))))


;;What the fuck did I write this one for then?
;;It seems to be more accurate than the recursive alternative.
;;Behaviour is more predictible, and it's easier to read. Using
;;this instead from now on
(defun reverse-base (base-rep &optional (base 2))
  (let ((pow (length base-rep)))       ; Sum a list which
    (reduce #'+                        ; is the evaluated
            (mapcar #'(lambda (x)      ; BCD representation
                        (decf pow)
                        (* (expt base pow) x))
                    base-rep))))


(defun reverse-mapper (input &optional (base 2))
  "Given some input string, return the INT value"
  (let ((vals (mapcar #'(lambda (x y) ; Create an ASSOC list for 
                          (cons x y)) ; quick lookup.
                      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)
                      (range 0 16))))
    (reverse-base (loop for char across input
                     collect (cdr (assoc char vals)))
                  base)))

(defun binary-stream (file)
  "Return a binary-stream for a file located at FILE"
  (let ((fh (open file :element-type '(unsigned-byte 8))))
    fh))

;;Utility to read the entirety of a binary file into list form.
;;Pretty attrocious on memory for larger files though, so a
;;binary stream wouldn't be out of place
(defun read-all (binary-file)
  (let ((fh (binary-stream binary-file)))
    (loop for byte = (read-byte fh nil nil)
       until (eq byte nil)
       collect byte)))

;;Read X number of bytes from a binary stream.
;;Handy for limiting the amount of data we're taking into memory,
;;but still gathering enough to do something with. I predict it
;;will become more useful as I start reading off instructions.
(defun read-x-bytes (x stream)
  (labels ((gather-bytes (current acc)
             (if (= current x)
                 (reverse acc)
                 (let ((byte (read-byte stream nil nil)))
                   (if (null byte)
                       (reverse acc) ;Exit, we're done
                       (gather-bytes (1+ current) (cons byte acc)))))))
    ;I get some weird behaviour when I don't check that 1
    (gather-bytes 0 '())))



;;Print WIDTH bytes at a time.
;;Probably only for debugging purposes.
;;Would be pretty straightforward (I hope) to create a function
;;that inserts an instruction at some range within this dump file
(defun hex-dump (file &optional (width 10))
  (let ((fh (binary-stream file))
        (loc 0))
    (loop for bytes = (read-x-bytes width fh)
       until (null bytes)
       do (progn
            (format t "~7,'0d ~{~3a~}~%" loc (mapbin bytes 16))
            (setf loc (+ width loc))))))
