;;; eval for quote, backquote

;;; quote --> (my-info-find-node "Elisp-ja" "Quoting")
(list 'foo 'bar)                        ; => (foo bar)

(quote ( foo bar))                      ; => (foo bar)

'(foo bar)                              ; => (foo bar)

''foo                                   ; => (quote foo)
'(quote foo)                            ; => (quote foo)

;;; backquote --> (my-info-find-node "Elisp-ja" "Backquote")
(let ((var1 'foo)
      (var2 'bar)
      )
  ;; quoteと同じ効果
  `(var1 var2))                         ; => (var1 var2)

(let ((var1 'foo)
      (var2 'bar))
  ;; "`"記号で変数を展開する
  `(,var1 ,var2))                       ; => (foo bar)

;;; 特別な印`,@'を使って、評価結果を結果となるリストに"繋ぎ合わせる
(setq some-list '(2 3))                 ; => (2 3)
(cons 1 (append some-list '(4) some-list)) ; => (1 2 3 4 2 3)

;; 改善例
`(1 ,@some-list 4 ,@some-list)          ; => (1 2 3 4 2 3)


(setq list '(hack foo bar))             ; => (hack foo bar)
(cons 'use
      (cons 'the
            (cons 'words (append (cdr list) '(as elements))))) ; => (use the words foo bar as elements)

;; 改善例
`(use the words ,@(cdr list) as elements) ; => (use the words foo bar as elements)

;;; 練習１
(let ((list1 '(i love))
      (list2 '(gnu emacs)))
  `(,@list1 ,@list2))                   ; => (i love gnu emacs)

;;; 練習２
(let ((list1 '(1 2 3))
      (list2 '(4 5 6)))
  `(,@(cdr list1) ,@(car list2)))          ; => (2 3 . 4)

