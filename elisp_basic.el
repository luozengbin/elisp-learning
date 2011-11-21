;; 
;; printing
;; 
;; --------------------------------------------------------------

(message "hi")                          ; printing

                                        ; printing variable values
(message "Her age is: %d" 16)           ; %d is for number
(message "Her name is: %s" "Vicky")     ; %s is for string
(message "Her mid init is: %c" 86)      ; %c is for character in ascii code
(message "My list is: %S" '(8 2 3))     ; %S is for a list

;; 
;; Arithmetic Functions
;; 
;; --------------------------------------------------------------
(+ 4 5 1)     ;    ⇒ 10
(- 9 2)       ;    ⇒  7
(- 9 2 3)     ;    ⇒  4
(* 2 3)       ;    ⇒  6
(* 2 3 2)     ;    ⇒ 12
(/ 7 2)       ;    ⇒  3 (Integer part of quotient)
(/ 7 2.0)     ;    ⇒  3.5
(% 7 4)       ;    ⇒  3 (Remainder)
(expt 2 3)    ;    ⇒ 8


;; float must have a 0 followed by a dot
(integerp 3.) ; returns t
(floatp 3.) ; returns nil
(floatp 3.0) ; returns t

;; 
;; True and False
;; 
;; --------------------------------------------------------------
;; all the following are false. They all evaluate to “nil”
nil
()
'()
(list)

(if nil "yes" "no") ; ⇒ "no"
(if () "yes" "no") ; ⇒ "no"
(if '() "yes" "no") ; ⇒ "no"
(if (list) "yes" "no") ; ⇒ "no"

(if t "yes" "no") ; ⇒ "yes"
(if 0 "yes" "no") ; ⇒ "yes"
(if "" "yes" "no") ; ⇒ "yes"
(if [] "yes" "no") ; ⇒ "yes". The [] is vector of 0 elements

(and t nil) ; ⇒ nil
(or t nil) ; ⇒ t

;; 
;; Comparison Functions
;; 
;; --------------------------------------------------------------

(< 3 4) ; less than
(> 3 4) ; greater than
(<= 3 4) ; less or equal to
(>= 3 4) ; greater or equal to

(= 3 3)   ; ⇒ t
(= 3 3.0) ; ⇒ t

(/= 3 4) ; ⇒ t

(string= "this" "this") ; ⇒ t. Case matters.
(string< "a" "b") ; ⇒ t. by lexicographic order.
(string< "B" "b") ; ⇒ t.

;; testing if two values have the same datatype and value.
(equal "abc" "abc") ; ⇒ t
(equal 3 3) ; ⇒ t
(equal 3.0 3.0) ; ⇒ t
(equal 3 3.0) ; ⇒ nil. Because datatype doesn't match.

;; testing equality of lists
(equal '(3 4 5) '(3 4 5))  ; ⇒ t
(equal '(3 4 5) '(3 4 "5")) ; ⇒ nil

;; testing equality of symbols
(equal 'abc 'abc) ; ⇒ t



(not (= 3 4)) ; ⇒ t
(/= 3 4) ; ⇒ t. “/=” is for comparing numbers only

(not (equal 3 4)) ; ⇒ t. General way to test inequality.

;; 
;; Global and Local Variables
;; 
;; --------------------------------------------------------------
(setq x 1) ; assign 1 to x
(setq a 3 b 2 c 7) ; assign 3 to a, 2 to b, 7 to c

(setq a 10)
(setq b 12)

(let ((a 7) (b 3))
  (+ a b)
  ) ; returns 7

;; 
;; A Block of Expressions
;; 
;; --------------------------------------------------------------
(progn (message "hi") (message "lo"))
(prog1 (message "AA") (message "BB") (message "CC"))
(prog2 (message "AA") (message "BB") (message "CC"))

(if t
    (progn ; true
      (message "AAAA")
      (message "BBBB")
      )
  (progn ; else
    (message "CCCC")
    (message "DDDD")
    )
  )

;; 
;; If Then Else
;; 
;; --------------------------------------------------------------
(if (< 3 2) (message "yes") )
(if (< 3 2) (message "yes") (message "no") )

(if nil (message "yes") (message "no") )  ; prints no

(when (< 2 3) 
  (message "aaa")
  (message "bbb")
  (message "ccc")
  )

;; 
;; Iteration
;; 
;; --------------------------------------------------------------
(setq x 0)

(while (< x 4)
  (princ (format "yay %d." x))
  (setq x (1+ x)))
