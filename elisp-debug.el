;;; --- 01.trace-function
;;; trace-functionで関数をトレースする

;;; sample 001
(defun f (x) (+ x 3))
(defun g (x) (+ (f x) 7))
(trace-function 'f (get-buffer-create "*f-trace-output*"))
(g 10)
(untrace-function 'f)

;;; sample 002
(defun fact (n)
  (if (= 0 n) 1
    (* (fact (1- n)) n)))

(trace-function 'fact (get-buffer-create "*fact-trace-output*"))

(fact 3)

(untrace-all)

;;; sample 003
(defun hoge-1 (x) (1+ x))
(defun hoge-2 (x) (* 2 (hoge-1 x)))

(setq hoge-trace-buffer (get-buffer-create "*hoge-trace-output*"))
(trace-function 'hoge-1 hoge-trace-buffer)
(trace-function 'hoge-2 hoge-trace-buffer)

(hoge-2 33)
(hoge-1 4)

;;; --- 02.edebug
;;; M-x edebug-defunで関数をedebug対象にする
;;; C-M-x 関数定義を行う
;;; C-u C-M-x 関数定義を行う伴に、edebug対象にする

(defun fact (x)
  "階乗"
  (if (zerop x)
      1
    (* x (fact (1- x)))))

(fact 3)

;;; (edebug)関数でブレークポイントを埋め込む
;;; か b でブレークポイントを設定する
;;; u で設定したブレークポイントを解除することができます
;;; B でブレークポイント間でjumpする
(defun edebug-test-1 ()
  (message "start")
  (message "a")
  ;; ブレークポイント、debugモードのみ使用できる
  ;; g で次のブレークポイントまで実行する
  (edebug)
  (message "b")
  (message "c")
  (edebug)
  (message "d")
  (message "e"))

(edebug-test-1)

;;; i でサブ関数もdebug対象とする
(defun test-a ()
  (message "a")
  ;; ここで i を押す
  (test-b)
  (message "c"))

(defun test-b ()
  (message "b"))

(test-a)

;;; 式監視
;;; e で随時に式の値を確認することができます
;;; E 監視リストバッファーを開く
;;; 監視リストバッファーに監視したい式を記述し、C-c C-uで登録する
(defun func-a ()
  (let ((a 1)
        (b 2)
        c)
    (setq c (+ a b))
    (incf a)
    (incf b)))

(func-a)

;;; p で実行時参照しているバッファーを表示する
;;; x で条件breakpointを設定することができます

(defun func-b (x)
  (with-current-buffer "init_font.el"
    (let ((a 1)
          (b 2)
          c)
      (setq c (+ a b))
      (incf a)
      (incf b))))

(func-b t)

