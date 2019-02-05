;;;;Wrote some utilities while working on an increibly boring Digital Systems assignment so that I don't neck myself part way through. Here's the result.

;;Standard range utility
(defun range (f stop &optional (b 1))
  (loop for i from f to stop by b
     collect i))
(expt 

(defun base (int base)
  (let* ((numerals (write-to-string int))
         (pow (length numerals)))
    (loop for i across numerals
         ;collect i)))
       do (decf pow)
         collect (* (parse-integer (string i)) (expt base pow)))))

;;Easily extended to higher bases
(defun bin (int)
  (labels ((bin-acc (current acc)
             (if (zerop current)
                 acc
                 (bin-acc (floor current 2) (push (rem current 2) acc)))))
    (bin-acc int '())))

(defun hex->bin (hex-int)
  ;;Get DEC value of hex numeral, convert to binary)


(defun hex (n)
  ;;Sorry for this hackjob. It prevents divisions by 0.
  (if (zerop (mod n 16)) ;and not equal to zero
      (cons 16 (loop for i from 0 to (log (// n 16) 16) collect 0))
		   ;;(reduce #'(lambda (x y) (format nil "~a ~a" x y))
		;;		     (loop for i from 0 to (log (// n 16) 16)
		;;			collect (write-to-string 0))))


  ;;The important stuff starts here
  (let* ((power (multiple-value-bind (val check) ;All this bullshit
		    (ceiling (log n 16))         ;is just to handle
		  (if (zerop check)              ;the overflow that
		      (setf val (1+ val)))       ;will occur at any
		  (expt 16 (1- val))))           ;powers of sixteen
	 (find (// n power)))                    ;that we will find
    (if (eq 1 power)
	(cons find nil)
	
	;;If the next one is two powers less,
	;;then we better account for that with more 0s
	(let ((next-power (// power 16))
	      (next-mod (mod n power)))
	  (if (< next-mod next-power)
	      (append (list find 0) (hex (mod n power)))
	      (cons find (hex (mod n power)))))))))

(defun int->hex (n)
  (reduce #'(lambda (x y) (format nil "~a~a" x y))
	  (mapcar #'hexmap (hex n))))
  


(defun hexify (file)
  (let ((fh (open file :element-type '(unsigned-byte 8))))
    (loop for byte = (read-byte fh nil nil)
       until (eq byte nil)
       collect byte)))

(defun hexmap (n)
  (case n
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")
    (6 "6")
    (7 "7")
    (8 "8")
    (9 "9")
    (10 "b")
    (11 "b")
    (12 "c")
    (13 "d")
    (14 "e")
    (15 "f")))

(defun // (x &rest y)
  (multiple-value-bind (first rest)
      (floor (/ x (reduce #'* y)))
    first))

(defun finder (num base)
  ;;Find the binary numeral.
  (labels ((find-rec (current)
	                (let ((remainder (rem current base))
		              (divisor (// current base)))
			  (if (zerop current)
			      0
			      (cons remainder (find-rec divisor))))))
    (mapcar #'hexmap (reverse (nconc (find-rec num) nil)))))


(defun rec (lst)
  (if (null (cdr lst))
      (car lst)
      (concatenate 'string (car lst) (rec (cdr lst)))))

(defun hex (input)
  (if (zerop input)
      "0"
      (rec (finder input 16))))
