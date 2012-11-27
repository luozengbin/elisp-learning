;;; -------------------------------------------------------
;;; 「apps-数字」にコマンドを割り当てする
;;; -------------------------------------------------------

;; prefix-keyとして定義する
(define-key global-map (kbd "<apps>") nil) ; => nil
(define-key global-map (kbd "<apps>") (make-sparse-keymap)) ; => (keymap)


(kbd "<apps> 1")                        ; => [apps 49]
;; kdbでは文字リテラルしか受付しませんので、下記のコードはNGです。
;; (kbd (format "<apps> %s" "1"))
(read-kbd-macro "<apps> 1")             ; => [apps 49]
(read-kbd-macro (format "<apps> %s" 1))          ; => [apps 49]

;; 「apps-1」 ～ 「apps-9」までのキーストロークにコマンドを割り当てる
(loop for i from 0 to 9 do
      (global-set-key (read-kbd-macro (format "<apps> %s" i)) 'my-apps-command))

;; Vectorの参照
(elt [a b]  0)                            ; => a
(aref [a b] 1)                            ; => b

(defun my-apps-command ()
  (interactive)
  (let* ((base-num (elt (kbd "<apps> 0") 1))
         (last-key-num (- (elt (this-command-keys-vector) 1) base-num)))
    (message "you press %d" last-key-num)))
