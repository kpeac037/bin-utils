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
  "Maps pretty-base over a list of ints, frequently reutrned by other functions"
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

(defun reverse-mapper (input &optional (base 2))
  "Given some input string, return the INT value"
  (let ((vals (mapcar #'(lambda (x y) ; Create an ASSOC list for 
                          (cons x y)) ; quick lookup.
                      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)
                      (range 0 16))))
    (reverse-base (loop for char across input
                     collect (cdr (assoc char vals)))
                  base)))


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

(defun binary-stream (file)
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

(defvar 8080-opcodes
  '(("00" . ("NOP" 1))
    ("01" . ("LXI B,D16" 3))
    ("02" . ("STAX B" 1))
    ("03" . ("INX B" 1))
    ("04" . ("INR B" 1))
    ("05" . ("DCR B" 1))
    ("07" . ("RLC" 1))
    ("09" . ("DAD B" 1))
    ("0a" . ("LDAX B" 1))
    ("0b" . ("DCX B" 1))
    ("0c" . ("INR C" 1))
    ("0d" . ("DCR C" 1))
    ("0e" . ("MVI C,D8" 2))
    ("0f" . ("RRC" 1))
    ("11" . ("LXI D,D16" 3))
    ("12" . ("STAX D" 1))
    ("13" . ("INX D" 1))
    ("14" . ("INR D" 1))
    ("15" . ("DCR D" 1))
    ("17" . ("RAL" 1))
    ("19" . ("DAD D" 1))
    ("1a" . ("LDAX D" 1))
    ("1b" . ("DCX D" 1))
    ("1c" . ("INR E" 1))
    ("1d" . ("DCR E" 1))
    ("1e" . ("MVI E,D8" 2))
    ("1f" . ("RAR" 1))
    ("21" . ("LXI H,D16" 3))
    ("23" . ("INX H" 1))
    ("24" . ("INR H" 1))
    ("25" . ("DCR H" 1))
    ("26" . ("MVI H,D8" 2))
    ("27" . ("DAA" 1))
    ("29" . ("DAD H" 1))
    ("2b" . ("DCX H" 1))
    ("2c" . ("INR L" 1))
    ("2d" . ("DCR L" 1))
    ("2f" . ("CMA" 1))
    ("34" . ("INR M" 1))
    ("35" . ("DCR M" 1))
    ("36" . ("MVI M,D8" 2))
    ("37" . ("STC" 1))
    ("3c" . ("INR A" 1))
    ("3d" . ("DCR A" 1))
    ("3e" . ("MVI A,D8" 2))
    ("3f" . ("CMC" 1))
    ("40" . ("MOV B,B" 1))
    ("41" . ("MOV B,C" 1))
    ("42" . ("MOV B,D" 1))
    ("43" . ("MOV B,E" 1))
    ("44" . ("MOV B,H" 1))
    ("45" . ("MOV B,L" 1))
    ("46" . ("MOV B,M" 1))
    ("47" . ("MOV B,A" 1))
    ("48" . ("MOV C,B" 1))
    ("49" . ("MOV C,C" 1))
    ("4a" . ("MOV C,D" 1))
    ("4b" . ("MOV C,E" 1))
    ("4c" . ("MOV C,H" 1))
    ("4d" . ("MOV C,L" 1))
    ("4e" . ("MOV C,M" 1))
    ("4f" . ("MOV C,A" 1))
    ("50" . ("MOV D,B" 1))
    ("51" . ("MOV D,C" 1))
    ("52" . ("MOV D,D" 1))
    ("53" . ("MOV D,E" 1))
    ("54" . ("MOV D,H" 1))
    ("55" . ("MOV D,L" 1))
    ("56" . ("MOV D,M" 1))
    ("57" . ("MOV D,A" 1))
    ("58" . ("MOV E,B" 1))
    ("59" . ("MOV E,C" 1))
    ("5a" . ("MOV E,D" 1))
    ("5b" . ("MOV E,E" 1))
    ("5c" . ("MOV E,H" 1))
    ("5d" . ("MOV E,L" 1))
    ("5e" . ("MOV E,M" 1))
    ("5f" . ("MOV E,A" 1))
    ("60" . ("MOV H,B" 1))
    ("61" . ("MOV H,C" 1))
    ("62" . ("MOV H,D" 1))
    ("63" . ("MOV H,E" 1))
    ("64" . ("MOV H,H" 1))
    ("65" . ("MOV H,L" 1))
    ("66" . ("MOV H,M" 1))
    ("67" . ("MOV H,A" 1))
    ("68" . ("MOV L,B" 1))
    ("69" . ("MOV L,C" 1))
    ("6a" . ("MOV L,D" 1))
    ("6b" . ("MOV L,E" 1))
    ("6c" . ("MOV L,H" 1))
    ("6d" . ("MOV L,L" 1))
    ("6e" . ("MOV L,M" 1))
    ("6f" . ("MOV L,A" 1))
    ("70" . ("MOV M,B" 1))
    ("71" . ("MOV M,C" 1))
    ("72" . ("MOV M,D" 1))
    ("73" . ("MOV M,E" 1))
    ("74" . ("MOV M,H" 1))
    ("75" . ("MOV M,L" 1))
    ("76" . ("HLT" 1))
    ("77" . ("MOV M,A" 1))
    ("78" . ("MOV A,B" 1))
    ("79" . ("MOV A,C" 1))
    ("7a" . ("MOV A,D" 1))
    ("7b" . ("MOV A,E" 1))
    ("7c" . ("MOV A,H" 1))
    ("7d" . ("MOV A,L" 1))
    ("7e" . ("MOV A,M" 1))
    ("7f" . ("MOV A,A" 1))
    ("80" . ("ADD B" 1))
    ("81" . ("ADD C" 1))
    ("82" . ("ADD D" 1))
    ("83" . ("ADD E" 1))
    ("84" . ("ADD H" 1))
    ("85" . ("ADD L" 1))
    ("86" . ("ADD M" 1))
    ("87" . ("ADD A" 1))
    ("88" . ("ADC B" 1))
    ("89" . ("ADC C" 1))
    ("8a" . ("ADC D" 1))
    ("8b" . ("ADC E" 1))
    ("8c" . ("ADC H" 1))
    ("8d" . ("ADC L" 1))
    ("8e" . ("ADC M" 1))
    ("8f" . ("ADC A" 1))
    ("90" . ("SUB B" 1))
    ("91" . ("SUB C" 1))
    ("92" . ("SUB D" 1))
    ("93" . ("SUB E" 1))
    ("94" . ("SUB H" 1))
    ("95" . ("SUB L" 1))
    ("96" . ("SUB M" 1))
    ("97" . ("SUB A" 1))
    ("98" . ("SBB B" 1))
    ("99" . ("SBB C" 1))
    ("9a" . ("SBB D" 1))
    ("9b" . ("SBB E" 1))
    ("9c" . ("SBB H" 1))
    ("9d" . ("SBB L" 1))
    ("9e" . ("SBB M" 1))
    ("9f" . ("SBB A" 1))
    ("a0" . ("ANA B" 1))
    ("a1" . ("ANA C" 1))
    ("a2" . ("ANA D" 1))
    ("a3" . ("ANA E" 1))
    ("a4" . ("ANA H" 1))
    ("a5" . ("ANA L" 1))
    ("a6" . ("ANA M" 1))
    ("a7" . ("ANA A" 1))
    ("a8" . ("XRA B" 1))
    ("a9" . ("XRA C" 1))
    ("aa" . ("XRA D" 1))
    ("ab" . ("XRA E" 1))
    ("ac" . ("XRA H" 1))
    ("ad" . ("XRA L" 1))
    ("ae" . ("XRA M" 1))
    ("af" . ("XRA A" 1))
    ("b0" . ("ORA B" 1))
    ("b1" . ("ORA C" 1))
    ("b2" . ("ORA D" 1))
    ("b3" . ("ORA E" 1))
    ("b4" . ("ORA H" 1))
    ("b5" . ("ORA L" 1))
    ("b6" . ("ORA M" 1))
    ("b7" . ("ORA A" 1))
    ("b8" . ("CMP B" 1))
    ("b9" . ("CMP C" 1))
    ("ba" . ("CMP D" 1))
    ("bb" . ("CMP E" 1))
    ("bc" . ("CMP H" 1))
    ("bd" . ("CMP L" 1))
    ("be" . ("CMP M" 1))
    ("bf" . ("CMP A" 1))
    ("c0" . ("RNZ" 1))
    ("c1" . ("POP B" 1))
    ("c5" . ("PUSH B" 1))
    ("c7" . ("RST 0" 1))
    ("c8" . ("RZ" 1))
    ("c9" . ("RET" 1))
    ("cf" . ("RST 1" 1))
    ("d0" . ("RNC" 1))
    ("d1" . ("POP D" 1))
    ("d5" . ("PUSH D" 1))
    ("d7" . ("RST 2" 1))
    ("d8" . ("RC" 1))
    ("df" . ("RST 3" 1))
    ("e0" . ("RPO" 1))
    ("e1" . ("POP H" 1))
    ("e3" . ("XTHL" 1))
    ("e5" . ("PUSH H" 1))
    ("e7" . ("RST 4" 1))
    ("e8" . ("RPE" 1))
    ("e9" . ("PCHL" 1))
    ("eb" . ("XCHG" 1))
    ("ef" . ("RST 5" 1))
    ("f0" . ("RP" 1))
    ("f3" . ("DI" 1))
    ("f7" . ("RST 6" 1))
    ("f8" . ("RM" 1))
    ("f9" . ("SPHL" 1))
    ("fb" . ("EI" 1))
    ("ff" . ("RST 7" 1))))

(defun opcode-in (binary-stream opcode-list)
  ;;Not a huge fan of how this instruction base is handled
  ;;Whole function might end up getting reworked.
  ;;Works for now though.
  (let* ((hex (pretty-base (car (read-x-bytes 1 binary-stream)) 16))
         (info (assoc hex opcode-list :test 'equalp)))
    (list hex info)))
         ;(instruction (nth 0 info))
         ;(inc (nth 1 info)))
    ;(if (= 1 inc)
        ;(list instruction)
        ;(append (list instruction) (mapbin
                           ;(read-x-bytes (1- inc) binary-stream)
                           ;16)))))

        ;(list instruction))))
        ;(list instruction (mapbin
        ;                   (read-x-bytes (1- inc) binary-stream)
        ;                   16)))))
