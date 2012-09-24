;;; mystrings.el --- 文字列操作する関数

;; Copyright (C) 2011  Zengbin Luo

;; Author: Zengbin Luo <jalen.cn@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; To match the beginning of a string, use \`
;; 参考リンク：http://xahlee.blogspot.com/2011/09/emacs-regex-quirk-matching-beginningend.html
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

;; 文字列から[a,e,i,o,u]を削除する関数
;; 対話式で呼ぶとき → バッファーに選択されたリージョンか段落が
;;                     処理対象となる、処理結果をバッファーに差し入れる。
;; 関数で呼ぶときに → １個目の引数が渡された場合、それが処理対象となる。
;;                     １個目がnilの場合、 オプション引数（ポイント）を
;;                     参照し処理文字列を取得する処理結果は関数戻り値で返す。
(defun remove-vowel (myString &optional p1 p2 )
  "Remove vowel {a e i o u} letters.
When called interactively, work on current paragraph or text selection.
When called in lisp code, if MYSTR is non-nil, returns a changed string.
If MYSTR nil, change the text in the region between positions p1 p2."
  (interactive                          ;対話式の場合、引数の初期処理
   (if (use-region-p)
       (list nil (region-beginning) (region-end)) ;選択されたリージョンの位置情報を引数に割り当てる
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) ) ;カーソル位置の文書段落の位置情報を引数に割り当てる

  ;; ここから関数の本文です。
  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if myString t nil)) ;引数パターン判定
    (setq inputStr (if workOnStringP myString (buffer-substring-no-properties p1 p2)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" inputStr) )  )

    ;; 引数パターンより処理結果を返すか、バッファーに差し替えるかを行う
    (if workOnStringP
        outputStr
      (progn 
        (delete-region p1 p2)
        (insert outputStr) )) ) )

(defun title-case-string-region-or-line (ξstring &optional ξregion-boundary)
  "Capitalize the current line or text selection, following title conventions.

Capitalize first letter of each word, except words like {to, of,
the, a, in, or, and, …}. If a word already contains cap letters
such as HTTP, URL, they are left as is.

When called in a elisp program, if ξREGION-BOUNDARY is nil,
returns the changed ξSTRING, else, work on the region.
ξREGION-BOUNDARY is a pair [from to], it can be a vector or
list."
  (interactive
   (let ((bds (get-selection-or-unit 'line)))
     (list nil (vector (elt bds 1) (elt bds 2)) ) ) )

  (let ( replacePairs
         (workOnStringP (if ξregion-boundary nil t ) )
         (p1 (elt ξregion-boundary 0))
         (p2 (elt ξregion-boundary 1))
         )
    
    (setq replacePairs '(
                         [" A " " a "]
                         [" And " " and "]
                         [" At " " at "]
                         [" As " " as "]
                         [" By " " by "]
                         [" Be " " be "]
                         [" Into " " into "]
                         [" In " " in "]
                         [" Is " " is "]
                         [" It " " it "]
                         [" For " " for "]
                         [" Of " " of "]
                         [" Or " " or "]
                         [" On " " on "]
                         [" The " " the "]
                         [" That " " that "]
                         [" To " " to "]
                         [" Vs " " vs "]
                         [" With " " with "]
                         [" From " " from "]
                         ))

    (let ((case-fold-search nil))
      (if workOnStringP
          (progn
            (replace-pairs-in-string-recursive (upcase-initials ξstring) replacePairs)
            )
        (progn
          (save-restriction
            (narrow-to-region p1 p2)
            (upcase-initials-region (point-min) (point-max) )
            (replace-regexp-pairs-region (point-min) (point-max) replacePairs t t)
            ) ) ) ) ) )

(provide 'mystrings)
;;; trim-string.el ends here
