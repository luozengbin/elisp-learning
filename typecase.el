;;; typecase 分岐練習

(defun grade-judgment (x)
  (typecase x
    ((integer 0 50)  (message "bad"))
    ((integer 51 60) (message "Grade E"))
    ((integer 61 70) (message "Grade D"))
    ((integer 71 80) (message "Grade C"))
    ((integer 71 80) (message "Grade B"))
    ((integer 91 100) (message "Grade A!!!"))
    (t (message "error"))))

(grade-judgment 10)                    ; => "bad"
(grade-judgment 60)                    ; => "Grade E"
(grade-judgment 75)                    ; => "Grade C"
(grade-judgment 100)                   ; => "Grade A!!!"
