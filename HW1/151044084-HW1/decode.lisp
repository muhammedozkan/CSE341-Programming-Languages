; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author:   151044084  Muhammed OZKAN      *
; *	                                          *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list-doc (filename)
(let ((doc '()) (parag '()) (word '()))
	(with-open-file (input filename)
  		(loop for chr = (read-char input nil)  			
  			do(when (string= chr #\linefeed)
  				(nreverse word)		(push word parag)
				(nreverse parag)	(push parag doc)
				(setf parag '())	(setf word '()))
	    	(when (or (string= chr " ") (string= chr nil))
	  		  	(nreverse word)	  	(push word parag)
		      	(setf word '())
				(when (string= chr nil)
				(nreverse parag)	(push parag doc)))
	  		while chr
	  			do(if (and (string/= chr " ") (string/= chr #\linefeed))
	  			(push (converter-sym2ch chr) word))))(nreverse doc)))

(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	(let ((dic '()) (word '()))
	(with-open-file (input filename)
  		(loop for chr = (read-char input nil)  			
  			do(when (string= chr #\linefeed)
  				(nreverse word)		(push word dic)
				(setf word '()))
	    	(when (or (string= chr " ") (string= chr nil))
	    		(nreverse word)	  	(push word dic)
		      	(setf word '()))
	  		while chr
	  			do (if (and (string/= chr " ") (string/= chr #\linefeed))
	  			(push (converter-sym2ch chr) word))))(nreverse dic)))

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defun spell-checker-0	(word filename)
	;;reading dictionary and
	;;linear searching O(n)
(dolist (dicword (read-as-list filename))
	(when (equal dicword word)
				(return-from spell-checker-0 T)))
	(return-from spell-checker-0 NIL))


;;hashmap searching O(logn)
(defun spell-checker-1 (word filename)
(setq hashlist	(get-hash-map filename))
(when (gethash (sxhash word) hashlist)
				(return-from spell-checker-1 T))
	(return-from spell-checker-1 NIL))

;;reading dictionary
(defun get-hash-map (filename)
(let ((hashmap (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7)))
 	(dolist (dicword (read-as-list filename))	
(setf (gethash (sxhash dicword) hashmap) dicword)) hashmap))

(defun split-word (list after)
  (labels
    ((inner (first-part remaining second-part)
       (if (= 0 remaining)
         (values first-part second-part)
         (if (null second-part)
           (values first-part second-part)
           (inner (cons (first second-part) first-part)
                  (1- remaining)	(rest second-part))))))
    (multiple-value-bind (first-part second-part) (inner '() after list)
      (list (nreverse first-part) second-part))))

;; takes a char, converts cipher char
(defun convert-chiper (ch chiper)
	(case ch
('a (nth 0 chiper))('b (nth 1 chiper))('c (nth 2 chiper))('d (nth 3 chiper))('e (nth 4 chiper))('f (nth 5 chiper))('g (nth 6 chiper))('h (nth 7 chiper))('i (nth 8 chiper))
('j (nth 9 chiper))('k (nth 10 chiper))('l (nth 11 chiper))('m (nth 12 chiper))('n (nth 13 chiper))('o (nth 14 chiper))('p (nth 15 chiper))('q (nth 16 chiper))('r (nth 17 chiper))
('s (nth 18 chiper))('t (nth 19 chiper))('u (nth 20 chiper))('v (nth 21 chiper))('w (nth 22 chiper))('x (nth 23 chiper))('y (nth 24 chiper))('z (nth 25 chiper))))

;;The accuracy of the code is determined by looking at the entire article. use spell-checker-0
(defun is-correct-chiper(chiper doc filename)
(dolist (w (make-list-doc doc))
(if (spell-checker-0 (decode-word w chiper) filename)
()(return-from is-correct-chiper nil)))
(return-from is-correct-chiper T))

;;document convert the word list
(defun make-list-doc(lst)
(let (newlst '())
(dolist (p lst)
	(dolist (w p)
		(push w newlst)))
(nreverse newlst)))

;; decode chiper alphabet convert the alphabet
(defun decode-chiper (ch chiper)
(loop for i from 0 to 25 do
(when (equal (nth i chiper) ch)
(return (nth i '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))))

;;takes a symbol, converts it to char
(defun converter-sym2ch (ch)
	(case ch
		(#\a 'a)(#\b 'b)(#\c 'c)(#\d 'd)(#\e 'e)(#\f 'f)(#\g 'g)(#\h 'h)(#\i 'i)(#\j 'j)(#\k 'k)(#\l 'l)(#\m 'm)
		(#\n 'n)(#\o 'o)(#\p 'p)(#\q 'q)(#\r 'r)(#\s 's)(#\t 't)(#\u 'u)(#\v 'v)(#\w 'w)(#\x 'x)(#\y 'y)(#\z 'z)))
		
;; takes a char, converts it to symbol
(defun converter-ch2sym (ch)
	(case ch
		('a #\a)('b #\b)('c #\c)('d #\d)('e #\e)('f #\f)('g #\g)('h #\h)('i #\i)('j #\j)('k #\k)('l #\l)('m #\m)
		('n #\n)('o #\o)('p #\p)('q #\q)('r #\r)('s #\s)('t #\t)('u #\u)('v #\v)('w #\w)('x #\x)('y #\y)('z #\z)))

;;convert word to number. 
(defun convert-word2num (word)
	(let ((o '()) (i 0) (k 0))
	(dolist (ch word)
		(block nested
		(do ((j 0 (1+ j)))
 			((< i j))
			(if (equal (nth j word) ch)
				(progn (push j o) (setf k 1) (return-from nested)))))
		(if (= k 1)
			(setf k 0)
			(push i o)
			)
		(incf i 1))
	(return-from convert-word2num (nreverse o))))

;assign each possible letter
(defun assign-letter (last-letter letter)
	(dolist (cl last-letter)
	(if (equal cl '(nil 0))
		(progn  (setf (nth 0 cl) letter)
		(setf (nth 1 cl) 1)  (return-from assign-letter last-letter)))
	(if (equal (nth 0 cl) letter)
		(progn   (setf (nth 1 cl) ( + (nth 1 cl) 1))  (return-from assign-letter last-letter))))
	(setf lst (copy-list '(x 1)))
	(push lst (cdr (last last-letter))) 
	(setf (nth 0 (nth 0 (last last-letter))) letter)
	(return-from assign-letter last-letter))

;;decide and assign alphabet
(defun decide-and-assign(encoded decoded)
	(let ((i 0))
	(dolist (e encoded)
		(dolist (letter abcee)
			(if (equal (car letter) e)
				(progn  (assign-letter (cdr letter) (nth i decoded)) (incf i))))))

	)
		
;;replace the old value in the list with the new one.
(defun replace-letter (new old lst)
  (cond ((equal old lst) new)
        ((atom lst)lst)
        (t (cons (replace-letter new old (car lst)) (replace-letter new old (cdr lst))))))

;;replace the old value in the document with the new one(E,T,A,O,I,N).
(defun replace-doc(doc letters)
;;E,T,A,O,I,N replace @ and € temp letter

(replace-letter  (second (sixth letters)) '@ (replace-letter  (first (sixth letters)) '€ 
 (replace-letter '€ (second (sixth letters)) (replace-letter '@ (first (sixth letters)) (replace-letter  (second (fifth letters)) '@ (replace-letter  (first (fifth letters)) '€ 
 (replace-letter '€ (second (fifth letters)) (replace-letter '@ (first (fifth letters)) (replace-letter  (second (fourth letters)) '@ (replace-letter  (first (fourth letters)) '€ 
 (replace-letter '€ (second (fourth letters)) (replace-letter '@ (first (fourth letters)) (replace-letter  (second (third letters)) '@ (replace-letter  (first (third letters)) '€ 
 (replace-letter '€ (second (third letters)) (replace-letter '@ (first (third letters)) (replace-letter  (second (second letters)) '@ (replace-letter  (first (second letters)) '€
 (replace-letter '€ (second (second letters)) (replace-letter '@ (first (second letters)) (replace-letter  (second (first letters)) '@ (replace-letter  (first (first letters)) '€ 
 (replace-letter '€ (second (first letters)) (replace-letter '@ (first (first letters)) doc)))))))))))))))))))))))))

(defun most-use-6-letter (document)
  (let ((arr (find-letter-ratio document))(mList nil)(max -1)(index -1)(rList nil))
    (loop for i from 0 to 5 do
      (loop for j from 0 to 25 do
        (if (member j mList)
          () (progn (if (> (aref arr j) max)
             (progn (setf max (aref arr j))
                    (setf index j))))))
        (setf mList (append mList (list index)))
        (setf max -1))
    ; add six element to list and return it
    (setf rList (append rList (list (list (converter-sym2ch (i2c (first mList))) 'e))))
    (setf rList (append rList (list (list (converter-sym2ch (i2c (second mList))) 't))))
    (setf rList (append rList (list (list (converter-sym2ch (i2c (third mList))) 'a))))
    (setf rList (append rList (list (list (converter-sym2ch (i2c (fourth mList))) 'o))))
    (setf rList (append rList (list (list (converter-sym2ch (i2c (fifth mList))) 'i))))
    (setf rList (append rList (list (list (converter-sym2ch (i2c (sixth mList))) 'n))))rList))

; finds the frequency ratio of characters for the document and returns an array
(defun find-letter-ratio (document)
  (let ((arr (make-array 26 :initial-element 0)))
    (dolist (p document)(dolist (i p)(dolist (ch i)
         (setf (aref arr   (c2i (converter-ch2sym ch))) (1+ (aref arr (c2i (converter-ch2sym ch))))))))arr))
		  
;;find and test possible alphabets according to permutation
(defun permutation (candidate remaining doc filename)
(when (and (is-correct-chiper candidate doc filename) (= (list-length candidate) 26))
(terpri)(terpri)
(princ "-----------Found Possible Chiper Alphabet----------------")
(print candidate)
(terpri)(terpri)
(princ "                      Decoded Text                        ")
(print (decode-doc doc candidate))
(terpri)(terpri)
(princ "---------------------------------------------------------")
(terpri)(terpri)
 (return-from permutation nil))
		(dotimes (i (list-length remaining))
			(let ((newCandidate) (newRemaining))
		(setf newCandidate (append candidate (list (nth i remaining))))	
		(setf newRemaining (append (second (split-word remaining (+ i 1))) (first (split-word remaining i))))
(permutation newCandidate newRemaining doc filename))))

;; -----------------------------------------------------
;; ENCODE FUNCTIONS
(defun encode-word (word chiper)
	(if (null word) ()
		(append (list (convert-chiper (car word) chiper)) (encode-word (cdr word) chiper))))

(defun encode-parag (paragraph chiper)
	(if (null paragraph) ()
		(append (list (encode-word (car paragraph) chiper)) (encode-parag (cdr paragraph) chiper))))

(defun encode-doc (doc chiper)
	(if (null doc) ()
		(append (list (encode-parag (car doc) chiper)) (encode-doc (cdr doc) chiper))))

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph filename)
	(permutation '(a b c d e f g h i j k l m n o p q r) '(t u v s w x y z) paragraph filename)
	)

(defun Gen-Decoder-B-0 (paragraph filename doc-letters)
  	(Gen-Decoder-A (replace-doc (replace-doc paragraph doc-letters) (reverse doc-letters)) filename )
  	)

(defun Gen-Decoder-B-1 (paragraph filename)
  	(dolist (d (read-as-list filename))
  		(dolist (p paragraph)
			(dolist (w p)
  	(if (equal (convert-word2num d) (convert-word2num w)) 
  		(decide-and-assign w d)))))
  	(print (decoder-doc paragraph))
)

;decode document
(defun decoder-doc (doc)
	(dolist (p doc)(dolist (w p)(let ((k 0))(dolist (ch w)
		(dolist (letter abcee)
			(if (equal (car letter) ch)	(let ((i 0))
				(dolist (l (cdr letter))
					(if (> (nth 1 l) i)	(progn (setf i (nth 1 l)) 
						(setf (nth k w) (nth 0 l))))))))
	(incf k)))))(return-from decoder-doc doc))

(defun Code-Breaker (document decoder filename)
(when (equal decoder 'Gen-Decoder-A)
(Gen-Decoder-A document filename))
(when (equal decoder 'Gen-Decoder-B-0)
 (Gen-Decoder-B-0 document filename (most-use-6-letter document)))
(when (equal decoder 'Gen-Decoder-B-1)
 (Gen-Decoder-B-1 document filename)))


(defun decode-word (word chiper)
	(if (null word) ()
		(append (list (decode-chiper (car word) chiper)) (decode-word (cdr word) chiper))))

(defun decode-parag (paragraph chiper)
	(if (null paragraph) ()
		(append (list (decode-word (car paragraph) chiper)) (decode-parag (cdr paragraph) chiper))))

(defun decode-doc (doc chiper)
	(if (null doc) ()
		(append (list (decode-parag (car doc) chiper)) (decode-doc (cdr doc) chiper))))

	
;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
(setq abcee '((a (nil 0)) (b (nil 0)) (c (nil 0)) (d (nil 0)) (e (nil 0)) (f (nil 0)) (g (nil 0)) (h (nil 0)) (i (nil 0)) (j (nil 0)) (k (nil 0)) (l (nil 0)) (m (nil 0)) 
(n (nil 0)) (o (nil 0)) (p (nil 0)) (q (nil 0)) (r (nil 0)) (s (nil 0)) (t (nil 0)) (u (nil 0)) (v (nil 0)) (w (nil 0)) (x (nil 0)) (y (nil 0) ) (z (nil 0) )))

(princ ".............................................................................")
(terpri)
(princ "				Testing")
(terpri)
(princ ".............................................................................")
(terpri)(terpri)
(princ "read-as-list (filename) Testing(Dictionary)")
(terpri)
(princ (read-as-list "dictionary1.txt"))
(terpri)(terpri)(terpri)
(princ "read-as-list-doc (filename) Testing(Document)")
(terpri)
(princ (read-as-list-doc "document2.txt"))
(terpri)(terpri)(terpri)
(princ "encode-doc (doc chiper) Testing(Encode Document)")
(terpri)
(princ "(a b c d e f g h i j k l m n o p q r)constant    8!(v t u y z s w x)")
(terpri)(terpri)
(princ (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)))
(terpri)(terpri)(terpri)
(princ "spell-checker-0	(word filename) Testing(Lineer Search)")
(terpri)
(princ "(H E L L O) is cheking dictionary1 Result is ")(princ (spell-checker-0 '(H E L L O) "dictionary1.txt"))
(terpri)
(princ "(X M A S) is cheking dictionary1 Result is ")(princ (spell-checker-0 '(X M A S) "dictionary1.txt"))
(terpri)(terpri)(terpri)
(princ "spell-checker-1	(word filename) Testing(Using Hash Map)")
(terpri)
(princ "(H E L L O) is cheking dictionary2 Result is ")(princ (spell-checker-1 '(H E L L O) "dictionary2.txt"))
(terpri)
(princ "(X M A S) is cheking dictionary2 Result is ")(princ (spell-checker-1 '(X M A S) "dictionary2.txt"))
(terpri)(terpri)(terpri)
(princ "Code-Breaker (document decoder filename) Testing(Gen-Decoder-A)")
(terpri)
(Code-Breaker (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)) 'Gen-Decoder-A "dictionary1.txt")
(terpri)(terpri)(terpri)
(princ "replace-doc (doc letters) Testing(Replace Document)")
(terpri)(terpri)
(princ (most-use-6-letter (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x))))
(terpri)(terpri)
(princ (replace-doc (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)) (most-use-6-letter (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)))))
(terpri)(terpri)(terpri)
(princ "Code-Breaker (document decoder filename) Testing(Gen-Decoder-B-0)")
(terpri)
(Code-Breaker (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)) 'Gen-Decoder-B-0 "dictionary1.txt")
(terpri)(terpri)
(princ "Code-Breaker (document decoder filename) Testing(Gen-Decoder-B-1)")
(terpri)
(Code-Breaker (encode-doc (read-as-list-doc "document2.txt") '(a b c d e f g h i j k l m n o p q r v t u y z s w x)) 'Gen-Decoder-B-1 "dictionary1.txt"))

;; test code...
(test_on_test_data)