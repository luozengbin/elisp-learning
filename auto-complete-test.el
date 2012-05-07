;;; auto-complete-mode 拡張テスト

;;; 雛形
(defvar ac-source-mysource1
  '((属性 . 値)
    ...))

;;; ------ simaple 001
;;; Foo, Bar, Bazを表示する情報源を作成する
(defun mysource1-candidates ()
'("Foo" "Bar" "Baz"))

(defvar ac-source-mysource1
  '((candidates . 'mysource1-candidates)))

;;; ローカルバッファー変数ac-sourcesに反映する
;; (setq ac-sources '(ac-source-mysource1))
(add-to-list 'ac-sources 'ac-source-mysource1)

;;; ------ simaple 002
;;; init属性で初回補完実行する度に情報源を計算する
(defvar mysource2-cache nil)

(defun mysource2-init ()
  (message "running mysource2-init ...")
  (setq mysource2-cache '("2_Huge" "2_Processing" "2_Is" "2_Done" "2_Here")))

(defvar ac-source-mysource2
  '((init . mysource2-init)
    (candidates . mysource2-cache)))

;; (setq ac-sources '(ac-source-mysource2))
(add-to-list 'ac-sources 'ac-source-mysource2)

;;; ------ simaple 003
;;; キャッシュ方法１
(defun mysource3-candidates ()
  (message "cache data....")
  '("3_Huge" "3_Processing" "3_Is" "3_Done" "3_Here"))

(defvar ac-source-mysource3
  '((candidates . mysource3-candidates)
    (cache)))

(add-to-list 'ac-sources 'ac-source-mysource3)

;;; ------ simaple 004
;;; キャッシュ方法２
(defvar mysource4-cache nil)

;;; ファイル保存する度に情報源をクリアする
(ac-clear-variable-after-save 'mysource4-cache)

;;; 定期的(一分ごと)にキャッシュをクリアする
(ac-clear-variable-every-minute 'mysource4-cache)

(setq mysource4-suffix 1)

(defun mysource4-candidates ()
  (or mysource4-cache
      (setq mysource4-cache (list (concat "4_" (number-to-string (incf mysource4-suffix)))
                                  (concat "4_" (number-to-string (incf mysource4-suffix)))
                                  (concat "4_" (number-to-string (incf mysource4-suffix)))
                                  (concat "4_" (number-to-string (incf mysource4-suffix)))
                                  ))))

(defvar ac-source-mysource4
  '((candidates . mysource4-candidates)))

(add-to-list 'ac-sources 'ac-source-mysource4)

;;; ------ simaple 005
;;; action属性で補完後の動作を定義する

(defun mysource5-candidates ()
  (mapcar (lambda (x) (buffer-name x)) (buffer-list)))

(setq ac-source-mysource5
  '((candidates . mysource5-candidates)
    (cache)
    (action . ac-source-mysource5-action)))

(defun ac-source-mysource5-action ()
  (interactive)
  (print ac-point)
  (print ac-prefix)
  (print (pp-to-string ac-selected-candidate)))

(setq ac-sources '(ac-source-mysource5))
