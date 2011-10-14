;; 次ぎの関数で１つの引数を受け取って、中身の数字を小文字の数字に置換して、戻りとして返す
(defun replace-digits-by-subscript (string)
  "Replace digits by Unicode subscript characters in STRING.
For example, 「103 and 42」 ⇒ 「₁₀₃ and ₄₂」."
  (let ((myStr string))
    (setq myStr (replace-regexp-in-string "0" "₀" myStr))
    (setq myStr (replace-regexp-in-string "1" "₁" myStr))
    (setq myStr (replace-regexp-in-string "2" "₂" myStr))
    (setq myStr (replace-regexp-in-string "3" "₃" myStr))
    (setq myStr (replace-regexp-in-string "4" "₄" myStr))
    (setq myStr (replace-regexp-in-string "5" "₅" myStr))
    (setq myStr (replace-regexp-in-string "6" "₆" myStr))
    (setq myStr (replace-regexp-in-string "7" "₇" myStr))
    (setq myStr (replace-regexp-in-string "8" "₈" myStr))
    (setq myStr (replace-regexp-in-string "9" "₉" myStr))
    ))

;; 改善案
(defun replace-digits-by-subscript3 (string)
  "Replace digits by Unicode subscript characters in STRING.
For example, 「103 and 42」 ⇒ 「₁₀₃ and ₄₂」."
  (let ((myStr string))
    (replace-regexp-in-string "[0-9]"
                              (lambda (arg) (string (aref "₀₁₂₃₄₅₆₇₈₉" (string-to-number arg)))) myStr)
    ))
