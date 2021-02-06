;**********************************
;* CSE341 - Programming Languages *
;*                                *
;*    151044084 Muhammed ÖZKAN    *
;*                                *
;**********************************


(defvar KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" "," "'"))
(defvar OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA" "CUT"))
(defvar Comment ";")
(defvar Possible (list "(" ")" "\""))
(defvar opoc 0)
(defvar tokens (list))
(defvar tokensLine (list))
(defvar sym (list))
(defvar symValue (list))


(defun evaluateLine (line)
 (let ((chunks) (result 0) (tmpWord) (flag 0))
  (setf tokensLine (list))
  (setf tokens (list))
  (setf line (string-trim '(#\Space #\Tab #\Newline) line))
  (setf chunks (split-str line))
  (loop for chunk in chunks
   do
   (progn
    (setf tmpWord (string-trim '(#\Space #\Tab #\Newline) chunk))
    (setf result (evaluateWord tmpWord))
    (if (or (equal result 2) (equal result -1)) (return result))))
  (if (equal result -1)
   (write "SYNTAX_ERROR Expression not recognized")
   (progn
    (if (equal result 2) ()
     (progn
      (setf flag (evaluate))
      (if (equal flag nil) (setf flag (expListI)))
      (if (equal flag nil) (write "SYNTAX_ERROR Expression not recognized"))))))result))

(defun evaluateWord (chunk)
 (let ((len (length chunk)) (subword) (j 0) (result) (tmp) (flag 0) (id 0))
  (loop for i from 1 to len
   do
   (progn
    (if (= flag 1) (setf flag 0))
    (setf subword (string-downcase (subseq chunk j i)))
    (if (= flag 0)
     (progn
      (setf result (findInList subword Operator))
      (if (not (equal result nil))
       (progn
        (if (equal result 4)
         (if (and (< i len) (string= (subseq chunk i (+ i 1)) "*")) (progn (setf i (+ i 1)) (setf result 3))))
        (if (equal result 7) (progn (setf result (+ result (mod opoc 2))) (setf opoc (+ opoc 1))))
        (if (or (equal result 5) (equal result 6) (equal result 7) (equal result 9) (equal result 10))
         (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list (nth result OP)))) (setf j i) (setf flag 1))
         (if (>= i len)
          (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list (nth result OP)))) (setf flag 1))
          (progn
            (setf tmp (subseq chunk i (+ i 1)))
            (if (equal (findInList tmp Possible) nil)
             (progn (setf flag -1))
             (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list (nth result OP)))) (setf j i) (setf flag 1))))))))))
      (if (= flag 0)
     (progn
      (setf result (findInList subword KeyWord))
      (if (not (equal result nil))
       (if (>= i len)
        (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list (nth result KW)))) (setf flag 1))
        (progn
          (setf tmp (subseq chunk i (+ i 1)))
                        (if (and (equal (findInList tmp Possible) nil))
           (if (equal (isID (concatenate 'string subword tmp)) nil)             (progn (setf flag -1)))
           (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list (nth result KW)))) (setf j i) (setf flag 1))))))))
      (if (= flag 0)
     (progn
         (setf result (isVal subword))
          (if (not (equal result nil))
       (progn
        (loop
         (setf tmp (string-downcase (subseq chunk j i)))
         (setf i (+ i 1))
         (when (or (equal (isVal tmp) nil) (> i len)) (return)))
              (setf i (- i 1))
        (if (equal (isVal tmp) nil) (setf i (- i 1)))                    (if (>= i len)
         (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list "VALUE"))) (setf flag 1))
         (progn
           (setf tmp (subseq chunk i (+ i 1)))
                    (if (equal (findInList tmp Possible) nil)
            (progn (setf flag -1))
            (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list "VALUE"))) (setf j i) (setf flag 1)
            ))))))))
      (if (and (= flag 0) (string= subword Comment))
      (if (and (< i len) (string= (subseq chunk i (+ i 1)) Comment))
       (progn (setf tokens (append tokens (list "COMMENT"))) (setf tokensLine (append tokensLine (list "COMMENT"))) (setf j i) (setf flag 2))))
      (if (= flag 0)
     (progn
         (setf result (isID subword))
      (if (equal result t)
       (if (= i len)
        (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list "IDENTIFIER"))) (setf flag 1))
        (progn
         (setf tmp (string-downcase (subseq chunk j (+ i 1))))
         (setf id (isID tmp))
         (if (equal result id)
          ()
          (progn
            (setf tmp (subseq chunk i (+ i 1)))
            (if (equal (findInList tmp Possible) nil)
             (progn (setf flag -1))
             (progn (setf tokens (append tokens (list subword))) (setf tokensLine (append tokensLine (list "IDENTIFIER"))) (setf j i) (setf flag 1)))))))
       (progn (setf flag -1)))))
    (if (= flag 2) (return flag))))
  flag))

(defun split-str (string &optional (separator " "))
 (split-1 string separator))
 
(defun split-1 (string &optional (separator " ") (r nil))
 (let ((n (position separator string
   :from-end t
   :test #'(lambda (x y)
     (find y x :test #'string=)))))
  (if n
 (split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
 (cons string r))))


(defun findExpresion (exp)
 (let ((counter 0) (str) (j 0) (result nil))
  (if (string= (nth 0 exp) "(")
   (progn
    (loop for i in exp
     do (progn
      (setf str (string-downcase i))
      (if (string= str "(") (setf counter (+ counter 1)))
      (if (string= str ")") (setf counter (- counter 1)))
      (setf j (+ j 1))
      (if (= counter 0) (return j))))(setf result j)))result))

(defun evaluate (&optional(tmpTokens tokens) (tmpTokensLine tokensLine)(flag2 0))
 (let ((len (list-length tmpTokens)) (result 0) (flag1 0) (val1 0) (val2 0) (val3 0) (tmp 2) (tmp2) (val4) (kw) (tmp3) (tmp4))
  (if (and (string= (nth 0 tmpTokensLine) "OP_OP") (string= (nth (- len 1) tmpTokensLine) "OP_CP"))
   (progn
    (setf kw (nth 1 tmpTokensLine))
    (if (or (string= kw "OP_PLUS") (string= kw "OP_MINUS") (string= kw "OP_MULT") (string= kw "OP_DIV") (string= kw "OP_DBLMULT"))
     (progn
      (setf flag1 1)
      (if (or (string= (nth 2 tmpTokensLine) "VALUE") (string= (nth 2 tmpTokensLine) "IDENTIFIER"))
       (progn
        (if (string= (nth 2 tmpTokensLine) "VALUE") (progn (setf val1 (parse-integer (nth 2 tmpTokens))) (setf tmp 3)))
        (if (string= (nth 2 tmpTokensLine) "IDENTIFIER") (progn (setf val1 (findId (nth 2 tmpTokens))) (setf tmp 3))))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 2)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (and (not (equal tmp nil)) (< tmp len)) (setf val1 (evaluate (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf result nil))
        (if (equal val1 nil) (setf result nil))))
      (if (equal tmp nil) (setf result nil) (setf tmp2 (+ tmp 2)))
      (if (not (equal result nil))
       (if (or (string= (nth tmp tmpTokensLine) "VALUE") (string= (nth tmp tmpTokensLine) "IDENTIFIER"))
        (progn
         (if (string= (nth tmp tmpTokensLine) "VALUE") (progn (setf val2 (parse-integer (nth tmp tmpTokens)))))
         (if (string= (nth tmp tmpTokensLine) "IDENTIFIER") (progn (setf val2 (findId (nth tmp tmpTokens))))))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (evaluate (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
         (setf tmp2 (+ tmp2 1))) (setf result nil))
         (if (equal val2 nil) (setf result nil)))))
      (if (and (not (equal result nil)) (equal tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (progn
        (if (string= kw "OP_PLUS") (setf result (+ val1 val2)))
        (if (string= kw "OP_MINUS") (setf result (- val1 val2)))
        (if (string= kw "OP_MULT") (setf result (* val1 val2)))
        (if (string= kw "OP_DIV") (setf result (/ val1 val2)))
        (if (string= kw "OP_DBLMULT") (setf result (expt val1 val2))))
       (setf result nil))))
    (if (string= kw "IDENTIFIER")
     (progn
      (setf flag1 1)
      (setf val1 (expListI (subseq tmpTokens 2 (- len 1)) (subseq tmpTokensLine 2 (- len 1)) 1))
      (if (equal val1 nil) (setf val1 (evaluate (subseq tmpTokens 2 (- len 1)) (subseq tmpTokensLine 2 (- len 1)) 1)))
      (if (equal val1 nil) (setf result nil) (setf result (nth 1 tmpTokens)))
      (if (equal flag2 1) (setf result 0))))
    (if (and (string= kw "KW_SET") (string= (nth 2 tmpTokensLine) "IDENTIFIER"))
     (progn
      (setf flag1 1)
      (setf val1 (expListI (subseq tmpTokens 3 (- len 1)) (subseq tmpTokensLine 3 (- len 1)) 1))
      (if (equal val1 nil) (setf val1 (evaluate (subseq tmpTokens 3 (- len 1)) (subseq tmpTokensLine 3 (- len 1)) 1)))
      (if (equal val1 nil)
       (setf result nil)
       (progn
        (setf val2 (position (nth 2 tmpTokens) sym :test #'string=))
        (if (equal val2 nil)
         (progn
          (setf sym (append sym (list (nth 2 tmpTokens))))
          (setf symValue (append symValue (list val1))))
         (setf (elt symValue val2) val1))
        (setf result val1)))))
    (if (string= kw "KW_IF")
     (progn
      (setf flag1 1)
      (setf tmp3 (nth 2 tmpTokensLine))
      (if (or (string= tmp3 "VALUE") (string= tmp3 "KW_TRUE") (string= tmp3 "KW_FALSE") (string= tmp3 "IDENTIFIER"))
       (progn (setf val1 (expB (list (nth 2 tmpTokens))(list (nth 2 tmpTokensLine)) 1)) (setf tmp 3))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 2)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (not (equal tmp nil)) (setf val1 (expB (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf result nil))))
      (if (equal tmp nil) (setf result nil) (setf tmp2 (+ tmp 2)))
      (if (not (equal result nil))
       (if (or (string= (nth tmp tmpTokensLine) "VALUE") (string= (nth tmp tmpTokensLine) "IDENTIFIER"))
        (progn
         (if (string= (nth tmp tmpTokensLine) "VALUE") (progn (setf val2 (parse-integer (nth tmp tmpTokens)))))
         (if (string= (nth tmp tmpTokensLine) "IDENTIFIER") (progn (setf val2 (findId (nth tmp tmpTokens))))))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (not (equal tmp2 nil)) (progn (setf val2 (evaluate (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) (setf tmp2 (+ tmp2 1))) (setf result nil))
         (if (equal val2 nil) (setf result nil)))))
      (if (and (not (equal tmp2 nil)) (< tmp2 len))
       (progn (setf val3 (evaluate (subseq tmpTokens (- tmp2 1) (- len 1)) (subseq tmpTokensLine (- tmp2 1) (- len 1)) 1))
       (if (equal val3 nil) (setf result nil) (setf tmp2 len)))
       (setf result nil))
      (if (and (not (equal result nil)) (= tmp2 len) (not (equal val2 nil)) (not (equal val3 nil)))
       (progn
        (if (equal val1 -2) (setf val1 nil))
        (if (equal val3 nil)
         (if val1 (setf result val2))
         (if val1 (setf result val2) (setf result val3))))
       (setf result nil))))
    (if (and (string= kw "KW_IF") (equal result nil))
     (progn
      (setf flag1 1)
      (setf result 0)
      (setf tmp 2)
      (setf tmp3 (nth 2 tmpTokensLine))
      (if (or (string= tmp3 "VALUE") (string= tmp3 "KW_TRUE") (string= tmp3 "KW_FALSE") (string= tmp3 "IDENTIFIER"))
       (progn (setf val1 (expB (list (nth 2 tmpTokens))(list (nth 2 tmpTokensLine)) 1)) (setf tmp 3))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 2)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (not (equal tmp nil)) (setf val1 (expB (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf result nil))))
      (if (equal tmp nil) (setf result nil) (setf tmp2 (+ tmp 2)))
      (if (not (equal result nil))
       (if (string= (nth (- tmp 1) tmpTokensLine) "OP_OP")
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens (- tmp 1))))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (expListI (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
         (setf tmp2 (+ tmp2 1))) (setf result nil)))
        (setf result nil)))
      (if (not (equal tmp2 len))
       (progn (setf val3 (expListI (subseq tmpTokens (- tmp2 1) (- len 1)) (subseq tmpTokensLine (- tmp2 1) (- len 1)) 1))
        (if (equal val3 nil) (setf result nil) (setf tmp2 len))))
      (if (and (not (equal result nil)) (= tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (progn
        (if (equal val1 -2) (setf val1 nil))
        (if (equal val3 nil)
         (if val1 (setf result val2))
         (if val1 (setf result val2) (setf result val3))))
       (setf result nil))))
    (if (and (string= kw "KW_FOR") (string= (nth 2 tmpTokensLine) "OP_OP") (string= (nth 3 tmpTokensLine) "IDENTIFIER"))
     (progn
      (setf flag1 1)
      (setf val1 (evaluate (list (nth 3 tmpTokens))(list (nth 3 tmpTokensLine)) 1))
      (if (equal val1 nil) (setf result nil))
      (setf tmp 4)
      (if (not (equal val1 nil))
       (if (or (string= (nth tmp tmpTokensLine) "VALUE") (string= (nth tmp tmpTokensLine) "IDENTIFIER"))
        (progn
         (if (string= (nth tmp tmpTokensLine) "VALUE") (progn (setf val2 (parse-integer (nth tmp tmpTokens))) (Setq tmp2 5)))
         (if (string= (nth tmp tmpTokensLine) "IDENTIFIER") (progn (setf val2 (findId (nth tmp tmpTokens))) (Setq tmp2 5))))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (setf val2 (evaluate (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) (setf result nil))
         (if (equal val2 nil) (setf result nil)))))
      (if (not (equal result nil))
       (if (or (string= (nth tmp2 tmpTokensLine) "VALUE") (string= (nth tmp2 tmpTokensLine) "IDENTIFIER"))
        (progn
         (if (string= (nth tmp2 tmpTokensLine) "VALUE") (progn (setf val3 (parse-integer (nth tmp2 tmpTokens))) (Setq tmp3 6)))
         (if (string= (nth tmp2 tmpTokensLine) "IDENTIFIER") (progn (setf val3 (findId (nth tmp2 tmpTokens))) (Setq tmp3 6))))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp2)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp3 nil)) (setf tmp3 (+ tmp2 tmp4)))
         (if (and (not (equal tmp3 nil)) (< tmp2 len)) (setf val3 (evaluate (subseq tmpTokens tmp2 tmp3) (subseq tmpTokensLine tmp2 tmp3) 1)) (setf result nil))
         (if (equal val3 nil) (setf result nil)))))
      (if (equal (nth tmp3 tmpTokensLine) "OP_CP") (setf tmp3 (+ tmp3 1)) (setf result nil))
      (if (not (equal result nil))
       (if (string= (nth tmp3 tmpTokensLine) "OP_OP")
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp3)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp4 nil)) (setf tmp4 (+ tmp3 tmp4)))
         (if (and (not (equal tmp4 nil)) (< tmp4 len)) (progn (setf val4 (expListI (subseq tmpTokens tmp3 tmp4) (subseq tmpTokensLine tmp3 tmp4) 1)) 
         (setf tmp4 (+ tmp4 1))) (setf result nil)))
        (setf result nil)))
      (if (and (not (equal result nil)) (= tmp4 len) (not (equal val2 nil)) (not (equal val3 nil)) (not (equal val4 nil)))
       (setf result val4)
       (setf result nil))))
    (if (and (string= kw "KW_LOAD") (string= (nth 2 tmpTokensLine) "OP_OC") (string= (nth 3 tmpTokensLine) "IDENTIFIER"))
     (progn
      (setf flag1 1)
      (setf tmp (nth 3 tmpTokens))
      (setf tmp2 (open tmp :if-does-not-exist nil))
      (if (equal tmp2 nil) (write NIL)(write T)) (terpri)
      (setf result tmp)))
    (if (and (string= kw "KW_DEFFUN") (string= (nth 2 tmpTokensLine) "IDENTIFIER"))
     (progn
      (setf flag1 1)
      (setf tmp 3)
      (setf sym (append sym (list (nth 2 tmpTokens))))
      (setf symValue (append symValue (list 0)))
      (if (string= (nth 3 tmpTokensLine) "IDENTIFIER")
       (progn (setf val1 0) (setf tmp 4))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 3)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (and (not (equal tmp nil)) (< tmp len)) (setf val1 (isIdList (subseq tmpTokens 3 tmp) (subseq tmpTokensLine 3 tmp))) (setf result nil))
        (if (equal val1 nil) (setf result nil))))
      (if (not (equal result nil))
       (if (or (string= (nth tmp tmpTokensLine) "VALUE") (string= (nth tmp tmpTokensLine) "IDENTIFIER"))
        (progn
         (if (string= (nth tmp tmpTokensLine) "VALUE") (progn (setf val2 (parse-integer (nth tmp tmpTokens)))))
         (if (string= (nth tmp tmpTokensLine) "IDENTIFIER") (progn (setf val2 (findId (nth tmp tmpTokens))))))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (evaluate (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
         (setf tmp2 (+ tmp2 1))) (setf result nil))
         (if (equal val2 nil) (setf result nil)))))
      (if (and (not (equal result nil)) (= tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (setf result (nth 2 tmpTokens))
       (setf result nil))))
    (if (and (equal result nil) (string= kw "KW_DEFFUN") (string= (nth 2 tmpTokensLine) "IDENTIFIER"))
     (progn
      (setf flag1 1)
      (setf tmp 3)
      (if (string= (nth 3 tmpTokensLine) "IDENTIFIER")
       (progn (setf val1 0) (setf tmp 4))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 3)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (and (not (equal tmp nil)) (< tmp len)) (setf val1 (isIdList (subseq tmpTokens 3 tmp) (subseq tmpTokensLine 3 tmp))) (setf result nil))
        (if (equal val1 nil) (setf result nil))))
      (if (string= (nth tmp tmpTokensLine) "OP_OP")
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (expListI (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
         (setf tmp2 (+ tmp2 1))) (setf result nil)))
        (setf result nil))
      (if (and (not (equal result nil)) (= tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (setf result (nth 2 tmpTokens))
       (setf result nil))))
    (if (and (string= kw "KW_EXIT") (equal len 3))
     (progn (setf flag1 1) (princ "Exiting.")(terpri) (exit)))
    (if (string= kw "KW_DISP")
     (progn
      (setf flag1 1)
      (setf val1 (evaluate (subseq tmpTokens 2 (- len 1)) (subseq tmpTokensLine 2 (- len 1)) 1))
      (if (equal val1 nil)
       (setf val1 (expListI (subseq tmpTokens 2 (- len 1)) (subseq tmpTokensLine 2 (- len 1)) 1)))
      (if (not (equal val1 nil))
       (progn (write val1)(terpri) (setf result val1))
       (setf result nil))))
    (if (equal flag1 0)
     (progn
      (setf result (expB tmpTokens tmpTokensLine 0)))))
   (progn
    (if (string= (nth 0 tokens) "COMMENT")
     (setf flag2 1)
     (progn
      (setf tmp3 (nth 0 tmpTokensLine))
      (if (equal len 1)
       (progn
        (if (string= tmp3 "VALUE") (setf val1 (parse-integer (nth 0 tmpTokens))) (setf tmp 3))
        (if (string= tmp3 "IDENTIFIER") (setf val1 (findId (nth 0 tmpTokens))) (setf tmp 3))
        (setf result val1))
       (setf result nil))))))
  (if (and (not (equal result nil)) (not (equal result -555)) (= flag2 0)) (write result))result))

(defun expB (&optional(tmpTokens tokens) (tmpTokensLine tokensLine)(flag2 0))
 (let ((len (list-length tmpTokens)) (result 0) (flag1 0) (val1 0) (val2 0) (tmp 2) (tmp2) (kw) (tmp3)(tmp4) (flag3 0))
  (if (and (string= (nth 0 tmpTokensLine) "OP_OP") (string= (nth (- len 1) tmpTokensLine) "OP_CP"))
   (progn
    (setf kw (nth 1 tmpTokensLine))
    (if (or (string= kw "KW_AND") (string= kw "KW_OR") (string= kw "KW_EQUAL") (string= kw "KW_LESS"))
     (progn
      (setf flag1 1)
      (setf tmp3 (nth 2 tmpTokensLine))
      (if (or (string= tmp3 "VALUE") (string= tmp3 "KW_TRUE") (string= tmp3 "KW_FALSE") (string= tmp3 "IDENTIFIER"))
       (progn
        (if (string= tmp3 "VALUE") (setf val1 (parse-integer (nth 2 tmpTokens))) (setf tmp 3))
        (if (string= tmp3 "KW_TRUE") (setf val1 t) (setf tmp 3))
        (if (string= tmp3 "KW_FALSE") (setf val1 -2) (setf tmp 3))
        (if (string= tmp3 "IDENTIFIER") (setf val1 (findId (nth 2 tmpTokens))) (setf tmp 3)))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 2)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (and (not (equal tmp nil)) (< tmp len)) (progn (setf val1 (expB (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf tmp2 (+ tmp 2))) (setf result nil))))
      (if (equal tmp nil) (setf result nil) (setf tmp3 (nth tmp tmpTokensLine)))
      (if (not (equal result nil))
       (if (or (string= tmp3 "VALUE") (string= tmp3 "KW_TRUE") (string= tmp3 "KW_FALSE") (string= tmp3 "IDENTIFIER"))
        (progn
         (if (string= tmp3 "VALUE") (setf val2 (parse-integer (nth tmp tmpTokens))))
         (if (string= tmp3 "KW_TRUE") (setf val2 t))
         (if (string= tmp3 "KW_FALSE") (setf val2 -2))
         (if (string= tmp3 "IDENTIFIER") (setf val2 (findId (nth tmp tmpTokens))))
         (setf tmp2 5))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4)))
         (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (expB (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
         (setf tmp2 (+ tmp2 1))) (setf result nil)))))
      (if (and (not (equal result nil)) (equal tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (progn
        (if (equal val1 -2) (setf val1 nil))
        (if (equal val2 -2) (setf val2 nil))
        (if (string= kw "KW_AND") (setf result (and val1 val2)))
        (if (string= kw "KW_OR") (setf result (or val1 val2)))
        (if (string= kw "KW_EQUAL") (setf result (equal val1 val2)))
        (if (string= kw "KW_LESS") (setf result (< val1 val2)))
        (if (= flag2 0) (setf flag3 2)
         (progn (if (equal result nil) (setf result -2)))))
       (setf result nil))))
    (if (string= kw "KW_NOT")
     (progn
      (setf flag1 1)
      (setf tmp3 (nth 2 tmpTokensLine))
      (if (or (string= tmp3 "VALUE") (string= tmp3 "KW_TRUE") (string= tmp3 "KW_FALSE") (string= tmp3 "IDENTIFIER"))
       (progn
        (if (string= tmp3 "VALUE") (setf val1 (parse-integer (nth 2 tmpTokens))) (setf tmp 3))
        (if (string= tmp3 "KW_TRUE") (setf val1 t) (setf tmp 3))
        (if (string= tmp3 "KW_FALSE") (setf val1 nil) (setf tmp 3))
        (if (string= tmp3 "IDENTIFIER") (setf val1 (findId (nth 2 tmpTokens))) (setf tmp 3))
        (setf result (not val1))
        (if (= flag2 0) (setf flag3 2)
         (progn (if (equal result nil) (setf result -2)))))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens 2)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
        (if (and (not (equal tmp nil)) (< tmp len)) (progn (setf val1 (expB (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf tmp (+ tmp 1))) (setf result nil))
        (if (and (not (equal result nil)) (equal tmp len) (not (equal val1 nil)))
         (progn
          (setf result (not val1)))
         (setf result nil))))))
    (if (equal flag1 0) (setf result nil)))
   (progn
    (setf tmp3 (nth 0 tmpTokensLine))
    (if (equal len 1)
     (progn
      (if (string= tmp3 "VALUE") (setf val1 (parse-integer (nth 0 tmpTokens))) (setf tmp 3))
      (if (string= tmp3 "KW_TRUE") (setf val1 t) (setf tmp 3))
      (if (string= tmp3 "KW_FALSE") (setf val1 -2) (setf tmp 3))
      (if (string= tmp3 "IDENTIFIER") (setf val1 (findId (nth 0 tmpTokens))) (setf tmp 3))
      (setf result val1))(setf result nil))))
  (if (or (and (not (equal result nil)) (= flag2 0)) (= flag3 2)) (write result))
  (if (and (= flag2 0) (= flag3 2)) (setf result -555))
  result))

(defun expListI (&optional(tmpTokens tokens) (tmpTokensLine tokensLine)(flag2 0))
 (let ((len (list-length tmpTokens)) (result 0) (flag1 0) (val1 0) (val2 0) (tmp 2) (tmp2) (tmp4) (kw))
  (if (and (string= (nth 0 tmpTokensLine) "OP_OP") (string= (nth (- len 1) tmpTokensLine) "OP_CP"))
   (progn
    (setf kw (nth 1 tmpTokensLine))
    (if (and (or (string= kw "KW_APPEND") (string= kw "KW_CONCAT")) (or (string= (nth 2 tmpTokensLine) "CUT") (string= (nth 3 tmpTokensLine) "KW_LIST")))
     (progn
      (setf flag1 1)
      (setf tmp4 (findExpresion (subseq tmpTokens 2)))
      (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
      (if (equal tmp nil) (setf result nil) (setf tmp2 (+ tmp 2)))
      (if (and (not (equal tmp nil)) (< tmp len)) (setf val1 (expListI (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf result nil))
      (if (and (not (equal tmp nil)) (< tmp len))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4))))
       (setf tmp2 nil))
      (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (expListI (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1)) 
      (setf tmp2 (+ tmp2 1))) (setf result nil))
      (if (and (not (equal result nil)) (= tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (progn
        (setf result (list))
        (setf result (append result val1))
        (setf result (append result val2)))(setf result nil))))
    (if (and (equal result nil) (string= kw "KW_APPEND"))
     (progn
      (setf flag1 1)
      (if (or (string= (nth 2 tmpTokensLine) "VALUE") (string= (nth 2 tmpTokensLine) "IDENTIFIER"))
       (progn
        (if (string= (nth 2 tmpTokensLine) "VALUE") (progn (setf val1 (parse-integer (nth 2 tmpTokens))) (Setq tmp 3)))
        (if (string= (nth 2 tmpTokensLine) "IDENTIFIER") (progn (setf val1 (findId (nth 2 tmpTokens))) (Setq tmp 3))))
       (if (string= (nth 2 tmpTokensLine) "IDENTIFIER")
        (progn (setf val1 (list (findId (nth 2 tmpTokens)))) (setf tmp 3))
        (progn
         (setf tmp4 (findExpresion (subseq tmpTokens 2)))
         (if (equal tmp4 nil) (progn (setf result nil) (setf tmp nil)) (setf tmp (+ tmp tmp4)))
         (if (not (equal tmp nil)) (setf val1 (evaluate (subseq tmpTokens 2 tmp) (subseq tmpTokensLine 2 tmp) 1)) (setf result nil))
         (if (equal val1 nil) (setf result nil)))))
      (if (equal tmp nil) (setf result nil) (setf tmp2 (+ tmp 2)))
      (if (and (not (equal tmp nil)) (< tmp len))
       (progn
        (setf tmp4 (findExpresion (subseq tmpTokens tmp)))
        (if (equal tmp4 nil) (progn (setf result nil) (setf tmp2 nil)) (setf tmp2 (+ tmp tmp4))))
       (setf tmp2 nil))
      (if (and (not (equal tmp2 nil)) (< tmp2 len)) (progn (setf val2 (expListI (subseq tmpTokens tmp tmp2) (subseq tmpTokensLine tmp tmp2) 1))
      (setf tmp2 (+ tmp2 1))) (setf result nil))
           (if (and (not (equal result nil)) (= tmp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
       (progn
        (setf result (list))
        (setf result (append result val1))
        (setf result (append result val2)))(setf result nil))))
    (if (string= (nth 1 tmpTokensLine) "KW_LIST")
     (progn
      (setf flag1 1)
      (setf (elt tmpTokens 1) "(")
      (setf (elt tmpTokensLine 1) "OP_OP")
      (setf val1 (convertList (subseq tmpTokens 1) (subseq tmpTokensLine 1)))
      (if (equal val1 nil) (setf result nil) (setf result val1))))
    (if (equal flag1 0) (setf result nil)))
   (progn
    (if (string= (nth 0 tmpTokensLine) "CUT") (setf result (convertList (subseq tmpTokens 1) (subseq tmpTokensLine 1))) (setf result nil))
    (setf result nil)))(if (and (not (equal result nil)) (= flag2 0)) (write result))result))

(defun convertList (tmpTokens tmpTokensLine)
 (let ((len (list-length tmpTokens)) (kw) (result 1) (val1) (llist (list)))
  (if (and (string= (nth 0 tmpTokensLine) "OP_OP") (string= (nth (- len 1) tmpTokensLine) "OP_CP") (> len 2))
   (progn
    (loop for i from 1 to (- len 2)
     do(progn
       (setf kw (nth i tmpTokensLine))
       (if (string= kw "VALUE")
        (setf val1 (list (parse-integer (nth i tmpTokens))))
        (if (string= kw "IDENTIFIER") (setf val1 (findId (nth i tmpTokens))) (setf result nil)))
       (if (not (equal result nil)) (setf llist (append llist val1)))))
    (if (not (equal result nil)) (setf result llist)))(setf result nil))result))

(defun findId (exp)
 (let((result 0))
  (setf result (position exp sym :test #'string=))
  (if (equal result nil)
   (progn (format t "variable ~S has no value." exp) (terpri) (exit))
   (setf result (nth result symValue)))result))

(defun isIdList (tmpTokens tmpTokensLine)
 (let ((len (list-length tmpTokensLine)) (kw) (result 1))
  (if (and (string= (nth 0 tmpTokensLine) "OP_OP") (string= (nth (- len 1) tmpTokensLine) "OP_CP") (> len 2))
   (progn
    (loop for i from 1 to (- len 2)
     do(progn
       (setf kw (nth i tmpTokensLine))
       (if (string= kw "IDENTIFIER") (progn (setf sym (append sym (list (nth i tmpTokens)))) (setf symValue (append symValue (list 0)))) 
       (setf result nil)))))(setf result nil))result))
       
(defun findInList (chunk complist &optional (i 0))
 (if (null complist)
  nil
  (if (string= chunk (car complist))
   i
   (findInList chunk (cdr complist) (+ i 1)))))

(defun isID (chunk)
 (let ((len (- (length chunk) 1)) (char) (result t))
  (loop for i from 0 to len
   do
   (progn
    (setf char (char chunk i))
    (if (= i 0)
     (if (or (alpha-char-p char) (char= char #\_) (char= char #\.) (char= char #\+)) (setf result t) (setf result nil))
     (if (or (alpha-char-p char) (digit-char-p char) (char= char #\_) (char= char #\.) (char= char #\+)) () (setf result nil)))
    (if (equal result nil) (return result))))
  result))

(defun isVal (chunk)
 (let ((char) (result t))
  (if (equal (every #'digit-char-p chunk) nil)
   (setf result nil)
   (progn
    (if (= (length chunk) 1)
     (setf result t)
     (progn
      (setf char (char chunk 0))
      (if (equal (digit-char-p char) 0) (setf result nil) (setf result t))))))
  result))


(defun gppinterpreter (&optional filename)
 (if filename
  (let ((in (open filename :if-does-not-exist nil)))
     (when in
    (loop for line = (read-line in nil)
     while line do (progn (evaluateLine line) (terpri)))
    (close in)))
  (let ((line) (flag))
   (loop
     (format t ">> ")
     (setf line (read-line))
     (when (string= line "") (return))
     (setf flag (evaluateLine line))
     (terpri)
  (when (= flag -1) (return))))))
      
(if *args* (gppinterpreter (car *args*)) (gppinterpreter))
