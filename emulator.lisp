(ql:quickload :cl-utilities)
(defpackage :emulator
  (:use :cl
        :bin-utils))
(in-package :emulator)

;;;Most instructions in 8080 are 1 byte. Some are 2 or 3 though
;;;A B C D E H and L registers. Also a Program Counter PC and
;;;a Stack Pointer SP
;;;Some instructions work on pairs of registers. BC, DE, HL
;;;Others just work on A
;;;HL used anytime data written to or from memory

;; Todo: Toss this in a utilities file
(defun subseq-search (string substr)
  (declare (string string substr))
  (let ((len (length substr)))
    (loop for i from 0 to (- (length string) len)
       if (equal substr (subseq string i (+ i len)))
         return t)))


(defclass 8080-conditions ()
    ((z :type int
        :initform 1
        :accessor z)
     (s :type int
        :initform 1
        :accessor s)
     (p :type int
        :initform 1
        :accessor p)
     (cy :type int
         :initform 1
         :accessor cy)
     (ac :type int
         :initform 1
         :accessor ac)
     (pad :type int
          :initform 3
          :accessor pad)))


(defclass 8080-state ()
           ((a :type int
               :accessor a)
            (b :type int
               :accessor b)
            (c :type int
               :accessor c)
            (d :type int
               :accessor d)
            (e :type int
               :accessor e)
            (h :type int
               :accessor h)
            (l :type int
               :accessor l)
            (sp :type int
               :accessor sp)
            (pc :type int
               :accessor pc)
            (memory :type int
               :accessor memory)
            (cc :type 8080-conditions
                :accessor cc)
            (int-enable :type int
                        :accessor int-enable)))
  

(defvar *state* (make-instance 

;; First  we need to put this into a hash table (serializable?).
;; There will be too many lookups for such an easy optimization.
;; May also allow for defining (and saving) functions to be called
;; upon to invocation of each one of these opcodes.
(defvar 8080-grammar
  '(
    ("0" . ("NOP" 1))
    ("1" . ("LXI B D16" 3))
    ("2" . ("STAX B" 1))
    ("3" . ("INX B" 1))
    ("4" . ("INR B" 1))
    ("5" . ("DCR B" 1))
    ("6" . ("MVI B D8" 2))
    ("7" . ("RLC" 1))
    ("8" . ("-" 1))
    ("9" . ("DAD B" 1))
    ("a" . ("LDAX B" 1))
    ("b" . ("DCX B" 1))
    ("c" . ("INR C" 1))
    ("d" . ("DCR C" 1))
    ("e" . ("MVI C D8" 2))
    ("f" . ("RRC" 1))
    ("10" . ("-" 1))
    ("11" . ("LXI D D16" 3))
    ("12" . ("STAX D" 1))
    ("13" . ("INX D" 1))
    ("14" . ("INR D" 1))
    ("15" . ("DCR D" 1))
    ("16" . ("MVI D D8" 2))
    ("17" . ("RAL" 1))
    ("18" . ("-" 1))
    ("19" . ("DAD D" 1))
    ("1a" . ("LDAX D" 1))
    ("1b" . ("DCX D" 1))
    ("1c" . ("INR E" 1))
    ("1d" . ("DCR E" 1))
    ("1e" . ("MVI E D8" 2))
    ("1f" . ("RAR" 1))
    ("20" . ("-" 1))
    ("21" . ("LXI H D16" 3))
    ("22" . ("SHLD adr" 3))
    ("23" . ("INX H" 1))
    ("24" . ("INR H" 1))
    ("25" . ("DCR H" 1))
    ("26" . ("MVI H D8" 2))
    ("27" . ("DAA" 1))
    ("28" . ("-" 1))
    ("29" . ("DAD H" 1))
    ("2a" . ("LHLD adr" 3))
    ("2b" . ("DCX H" 1))
    ("2c" . ("INR L" 1))
    ("2d" . ("DCR L" 1))
    ("2e" . ("MVI L D8" 2))
    ("2f" . ("CMA" 1))
    ("30" . ("-" 1))
    ("31" . ("LXI SP D16" 3))
    ("32" . ("STA adr" 3))
    ("33" . ("INX SP" 1))
    ("34" . ("INR M" 1))
    ("35" . ("DCR M" 1))
    ("36" . ("MVI M D8" 2))
    ("37" . ("STC" 1))
    ("38" . ("-" 1))
    ("39" . ("DAD SP" 1))
    ("3a" . ("LDA adr" 3))
    ("3b" . ("DCX SP" 1))
    ("3c" . ("INR A" 1))
    ("3d" . ("DCR A" 1))
    ("3e" . ("MVI A D8" 2))
    ("3f" . ("CMC" 1))
    ("40" . ("MOV B B" 1))
    ("41" . ("MOV B C" 1))
    ("42" . ("MOV B D" 1))
    ("43" . ("MOV B E" 1))
    ("44" . ("MOV B H" 1))
    ("45" . ("MOV B L" 1))
    ("46" . ("MOV B M" 1))
    ("47" . ("MOV B A" 1))
    ("48" . ("MOV C B" 1))
    ("49" . ("MOV C C" 1))
    ("4a" . ("MOV C D" 1))
    ("4b" . ("MOV C E" 1))
    ("4c" . ("MOV C H" 1))
    ("4d" . ("MOV C L" 1))
    ("4e" . ("MOV C M" 1))
    ("4f" . ("MOV C A" 1))
    ("50" . ("MOV D B" 1))
    ("51" . ("MOV D C" 1))
    ("52" . ("MOV D D" 1))
    ("53" . ("MOV D E" 1))
    ("54" . ("MOV D H" 1))
    ("55" . ("MOV D L" 1))
    ("56" . ("MOV D M" 1))
    ("57" . ("MOV D A" 1))
    ("58" . ("MOV E B" 1))
    ("59" . ("MOV E C" 1))
    ("5a" . ("MOV E D" 1))
    ("5b" . ("MOV E E" 1))
    ("5c" . ("MOV E H" 1))
    ("5d" . ("MOV E L" 1))
    ("5e" . ("MOV E M" 1))
    ("5f" . ("MOV E A" 1))
    ("60" . ("MOV H B" 1))
    ("61" . ("MOV H C" 1))
    ("62" . ("MOV H D" 1))
    ("63" . ("MOV H E" 1))
    ("64" . ("MOV H H" 1))
    ("65" . ("MOV H L" 1))
    ("66" . ("MOV H M" 1))
    ("67" . ("MOV H A" 1))
    ("68" . ("MOV L B" 1))
    ("69" . ("MOV L C" 1))
    ("6a" . ("MOV L D" 1))
    ("6b" . ("MOV L E" 1))
    ("6c" . ("MOV L H" 1))
    ("6d" . ("MOV L L" 1))
    ("6e" . ("MOV L M" 1))
    ("6f" . ("MOV L A" 1))
    ("70" . ("MOV M B" 1))
    ("71" . ("MOV M C" 1))
    ("72" . ("MOV M D" 1))
    ("73" . ("MOV M E" 1))
    ("74" . ("MOV M H" 1))
    ("75" . ("MOV M L" 1))
    ("76" . ("HLT" 1))
    ("77" . ("MOV M A" 1))
    ("78" . ("MOV A B" 1))
    ("79" . ("MOV A C" 1))
    ("7a" . ("MOV A D" 1))
    ("7b" . ("MOV A E" 1))
    ("7c" . ("MOV A H" 1))
    ("7d" . ("MOV A L" 1))
    ("7e" . ("MOV A M" 1))
    ("7f" . ("MOV A A" 1))
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
    ("c2" . ("JNZ adr" 3))
    ("c3" . ("JMP adr" 3))
    ("c4" . ("CNZ adr" 3))
    ("c5" . ("PUSH B" 1))
    ("c6" . ("ADI D8" 2))
    ("c7" . ("RST 0" 1))
    ("c8" . ("RZ" 1))
    ("c9" . ("RET" 1))
    ("ca" . ("JZ adr" 3))
    ("cb" . ("-" 1))
    ("cc" . ("CZ adr" 3))
    ("cd" . ("CALL adr" 3))
    ("ce" . ("ACI D8" 2))
    ("cf" . ("RST 1" 1))
    ("d0" . ("RNC" 1))
    ("d1" . ("POP D" 1))
    ("d2" . ("JNC adr" 3))
    ("d3" . ("OUT D8" 2))
    ("d4" . ("CNC adr" 3))
    ("d5" . ("PUSH D" 1))
    ("d6" . ("SUI D8" 2))
    ("d7" . ("RST 2" 1))
    ("d8" . ("RC" 1))
    ("d9" . ("-" 1))
    ("da" . ("JC adr" 3))
    ("db" . ("IN D8" 2))
    ("dc" . ("CC adr" 3))
    ("dd" . ("-" 1))
    ("de" . ("SBI D8" 2))
    ("df" . ("RST 3" 1))
    ("e0" . ("RPO" 1))
    ("e1" . ("POP H" 1))
    ("e2" . ("JPO adr" 3))
    ("e3" . ("XTHL" 1))
    ("e4" . ("CPO adr" 3))
    ("e5" . ("PUSH H" 1))
    ("e6" . ("ANI D8" 2))
    ("e7" . ("RST 4" 1))
    ("e8" . ("RPE" 1))
    ("e9" . ("PCHL" 1))
    ("ea" . ("JPE adr" 3))
    ("eb" . ("XCHG" 1))
    ("ec" . ("CPE adr" 3))
    ("ed" . ("-" 1))
    ("ee" . ("XRI D8" 2))
    ("ef" . ("RST 5" 1))
    ("f0" . ("RP" 1))
    ("f1" . ("POP PSW" 1))
    ("f2" . ("JP adr" 3))
    ("f3" . ("DI" 1))
    ("f4" . ("CP adr" 3))
    ("f5" . ("PUSH PSW" 1))
    ("f6" . ("ORI D8" 2))
    ("f7" . ("RST 6" 1))
    ("f8" . ("RM" 1))
    ("f9" . ("SPHL" 1))
    ("fa" . ("JM adr" 3))
    ("fb" . ("EI" 1))
    ("fc" . ("CM adr" 3))
    ("fd" . ("-" 1))
    ("fe" . ("CPI D8" 2))
    ("ff" . ("RST 7" 1))))

(defun opcode-lookup (byte)
  (declare (integer byte))
  (let* ((data (pretty-base byte 16))
         (opcode (assoc data 8080-grammar :test 'equalp)))
    opcode))
    ;(cl-utilities:split-sequence #\Space (nth 1 opcode))))

(defun opcode-operate (opcode-list)
  "Change the STATE of our emulator given a list of information provided by OPCODE-LOOKUP"
  (declare (list opcode-list))
  (let ((inc (nth 2 opcode-list))
        (opcode-info (cl-utilities:split-sequence #\Space (nth 1 opcode-list))))
    (list inc opcode-info)))

;; ALL THE POSSIBLE WAYS TO IMPLEMENT A SHELL:
;; 1) hash/assoc table lookup
;;        - Return lambdas which call another function
;;        - Return forms which may be evaluated by a macro (added
;;          benefit of being able to parse these forms for information
;;          regarding PARAMETERS)
;;        - COND lookup (absolute pain in the ass)
;;        - Switch macro
;;        - Personalized macro to deal with this
;;
;; ALL THE POSSIBLE FORMS OF MEMONICS
;; 1) MEM
;; 2) MEM reg
;; 3) MEM reg int
;; 4) MEM addr
;; 5) -            This might signify data

(defvar 8080-shell
  (make-hash-table :test 'equal))
(defun append-shell (memonic function)
  (setf (gethash memonic 8080-shell) function))

;;Screwing around with different ways to make an emulator shell.
;;This one uses a hash table to lookup each instruction. At the
;;hash location, a function is stored describing what to do
;;(just call another function). Maybe this would be better as
;;a macro? And rather than store a function, we would store a form
;;to be evaluated.
;;This way we could actually examine the parameters (which will
;;be one of the difficulties with this way of doing things)
(defun update-shell-with-functions ()
  (mapcar #'(lambda (x) (append-shell (car x) (cdr x)))
          '(("NOP" . #'(lambda () nil))
            ("LXI" . #'(lambda ()
            ("STAX" . #'(lambda (register) (STAX register)))
            ("INX" . #'(lambda (register) (INX register)))
            ("INR" . #'(lambda (register) (INR register)))
            ("DCR" . #'(lambda (register) (DCR register)))
            ("MVI" . #'(lambda (register d8) (MVI register d8))))))))

;;So uhhh...
;;This is a thing I guess.
;;At first I thought it was ridiculous (actually it probably still ;;is), but I eventually got to the point where I had to see how well
;;it would work.
;;Or if it would work at all for that matter >_>
;;Honestly I should probably just do this with classes like a sane
;;person might (credit at github.com/samanthadoran/potential-disco
;;for a great example as to how that might work)
(defun update-shell-with-forms ()
    (mapcar #'(lambda (x) (append-shell (car x) (cdr x)))
     '(("NOP" . `(NOP))
       ("LXI" . `(LXI ,reg1 ,int))
       ("STAX" . `(STAX ,reg1))
       ("INX" . `(INX ,reg1))
       ("INR" . `(INR ,reg1))
       ("DCR" . `(DCR ,reg1))
       ("MVI" . `(MVI ,reg1 ,int))
       ("RLC" . `(RLC))
       ("DAD" . `(DAD ,reg1))
       ("LDAX" . `(LDAX ,reg1))
       ("DCX" . `(DCX ,reg1))
       ("RRC" . `(RRC))
       ("RAL" . `(RAL))
       ("RAR" . `(RAR))
       ("SHLD" . `(SHLD ,adr))
       ("DAA" . `(DAA))
       ("LHLD" . `(LDLD ,adr))
       ("CMA" . `(CMA))
       ("STC" . `(STC))
       ("CMC" . `(CMC))
       ("LDA" . `(LDA ,adr))
       ("MOV" . `(MOV ,reg1 ,reg2))
       ("HLT" . `(HLT))
       ("ADD" . `(ADD ,reg1))
       ("ADC" . `(ADC ,reg1))
       ("SUB" . `(SUB ,reg1))
       ("SBB" . `(SBB ,reg1))
       ("ANA" . `(ANA ,reg1))
       ("XRA" . `(XRA ,reg1))
       ("ORA" . `(ORA ,reg1))
       ("CMP" . `(CMP ,reg1))
       ("RNZ" . `(RNZ))
       ("POP" . `(POP ,reg1))
       ("JNZ" . `(JNZ ,adr))
       ("JMP" . `(JMP ,adr))
       ("CNZ" . `(CNZ ,adr))
       ("PUSH" . `(PUSH ,reg))
       ("ADI" . `(ADI ,int))
       ("RST" . `(RST ,int))
       ("RZ" . `(RZ))
       ("RET" . `(RET))
       ("JZ" . `(JZ ,adr))
       ("CZ" . `(CZ ,adr))
       ("CALL" . `(CALL ,adr))
       ("RNC" . `(RNC))
       ("JNC" . `(JNC ,adr))
       ("OUT" . `(OUT ,int))
       ("CNC" . `(CNC ,adr))
       ("SUI" . `(SUI ,adr))
       ("RC" . `(RC))
       ("JC" . `(JC ,adr))
       ("IN" . `(IN ,int))
       ("CC" . `(CC ,adr))
       ("SBI" . `(SBI ,int))
       ("RPO" . `(RPO))
       ("JPO" . `(JPO ,adr))
       ("XTHL" . `(XTHL))
       ("CPO" . `(CPO ,adr))
       ("ANI" . `(ANI ,int))
       ("RPE" . `(RPE))
       ("PCHL" . `(PCHL))
       ("JPE" . `(JPE ,adr))
       ("XCHG" . `(XCHG))
       ("CPE" . `(CPE ,adr))
       ("XRI" . `(XRI ,adr))
       ("RP" . `(RP))
       ("JP" . `(JP ,adr))
       ("DI" . `(DI))
       ("CP" . `(CP ,adr))
       ("ORI" . `(ORI ,int))
       ("RM" . `(RM))
       ("SPHL" . `(SPHL))
       ("JM" . `(JM ,adr))
       ("EI" . `(EI))
       ("CM" . `(CM ,adr))
       ("CPI" . `(CPI ,int)))))
;;75 distinct functions in total.
       
       

       
(defun inx (reg)
  (format t "INX called with register value: ~a~%" reg))

(defun inr (reg)
  
(defmacro op-eval (opcode)
  `(let ((reg "REEE"))
    ,(gethash opcode 8080-shell)))



;;UNCLEAN
;;UNCLEAN UNCLEAN UNCLEAN
;;BURN IT
;;PREFERABLY WITH FIRE
;;For real though, the better solution would probably be to
;;lookup each opcode one at a time
(defun opcode-in (binary-stream &optional (opcode-list 8080-grammar))
  ;;Not a huge fan of how this instruction base is handled
  ;;Whole function might end up getting reworked.
  ;;Works for now though.
  (handler-case
  (let* ((hex (pretty-base (car (read-x-bytes 1 binary-stream)) 16))
         (info (assoc hex opcode-list :test 'equalp))
         (instruction (nth 1 info))
         (inc (nth 2 info)))
    ;; LXI B,d16     We sub d16 with #20c0 (next 3 bytes)
    ;; STA adr       adr always subbed with a 3 byte address
    (if (= 1 inc)
        instruction
        (let ((codes (mapbin (read-x-bytes (1- inc)
                                           binary-stream)              
                             16)))
          (cond
            ((subseq-search instruction "D8")
             ;; replace last 2 with next 1
             (format nil "~a~a" (subseq instruction 0 (- (length instruction) 2)) (nth 0 codes)))
            ((subseq-search instruction "adr")
             (format nil "~a ~a~a" (subseq instruction 0 (- (length instruction) 3)) (nth 1 codes) (nth 0 codes)))
            ((subseq-search instruction "D16")
             (format nil "~a~a~a" (subseq instruction 0 (- (length instruction) 3)) (nth 1 codes) (nth 0 codes)))
            (t
             (format nil "~a" instruction))))))
    (error (c)
      (format t "(OPCODE-IN) caught error: ~a" c)
      nil)))


;; Obviously not the cleanest way to do this.
;; It works for now though
(defun disassemble-binary (file)
  (let ((fh (binary-stream file)))
    (loop for line = (opcode-in fh)
       while line
         do (format t "~a~%" line))))

