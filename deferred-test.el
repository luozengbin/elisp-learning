;;; https://github.com/kiwanami/emacs-deferred/blob/master/README.ja.markdown

;;; インストール
;;; (auto-install-from-url https://github.com/kiwanami/emacs-deferred/raw/master/deferred.el")

;;; ---------------------------------------------------
;;; basic: 非同期実行チェーン
;;; ---------------------------------------------------
(deferred:$
  ;; 次のコードが非同期に順次でに実行される
  (deferred:next
    (lambda () (message "deferred start")))

  ;; リターン値が次のの式に伝播される
  (deferred:nextc it
    (lambda ()
      (message "chain 1")
      1))

  (deferred:nextc it
    (lambda (x)
      (message "chain 2 : %s" x)))

  (deferred:nextc it
    (lambda ()
      (read-minibuffer "Input a number: ")
      ))

  (deferred:nextc it
    (lambda (x)
      (message "Got the number : %i" x)))

  (deferred:nextc it
    (lambda ()
      (message "successfull!!!")))

  (deferred:error it                    ;チェーン実行中に例外が起きると、ここに飛ぶ
    (lambda (err)
      (message "Wrong input : %s" err))))

;;; -------------------------------------------------------------
;;; deferred:wait --> タイマーで一定時間後非同期実行する
;;; -------------------------------------------------------------
(deferred:$
  (deferred:next
    (lambda () (message "start!!!!")))
  (deferred:wait (* 2 1000))                  ;２秒
  (deferred:nextc it
    (lambda (x)
      (message "simple timer!: %s msec" x)))  ;実際経過した時間が引数として渡る
  )

(deferred:$
  (deferred:next
    (lambda () (message "start!!!!")))
  (deferred:wait-idle (* 2 1000))                  ;２秒
  (deferred:nextc it
    (lambda (x)
      (message "simple timer!: %s msec" x)))  ;実際経過した時間が引数として渡る
  )

;;; -------------------------------------------------------------
;;; 外部プロセス・コマンド実行
;;; -------------------------------------------------------------
(deferred:$
  (deferred:process "ls" "-al")         ;ls -alを非同期で実行する
  (deferred:nextc it
    (lambda (x)                         ;実行結果を別windowsに表示する
      (display-buffer (with-current-buffer (get-buffer-create "*temp*")
                        (erase-buffer)
                        (insert x)
                        (current-buffer))))))

;;; -------------------------------------------------------------
;;; HTTP通信
;;; -------------------------------------------------------------
(require 'url)

;;; HTML取得
(deferred:$
  (deferred:url-retrieve "http://www.gnu.org") ;HTML取得
  (deferred:nextc it
    (lambda (buf)                       ;HTMLバッファーの表示
      (with-current-buffer buf
        (goto-char (point-min)))
      (display-buffer buf))))

;;; 画像取得
(deferred:$
  (deferred:url-retrieve "http://www.google.co.jp/intl/en_com/images/srpr/logo1w.png")
  (deferred:nextc it
    (lambda (buf)
      (display-buffer
       (with-current-buffer (get-buffer-create "*temp*")
         (insert-image                  ;画像オブジェクトを生成する
          (with-current-buffer buf
            (create-image               ;画像データ抽出
             (substring (buffer-string) (+ (string-match "\n\n" (buffer-string)) 2))
             'png t)))
         (current-buffer))))))

;;; -------------------------------------------------------------
;;; 並列処理
;;; -------------------------------------------------------------
(deferred:$
  (deferred:parallel                    ;並列実行
    (lambda ()
      (deferred:url-retrieve "http://www.google.co.jp/intl/en_com/images/srpr/logo1w.png"))
    (lambda ()
      (deferred:url-retrieve "http://www.google.co.jp/images/srpr/nav_logo14.png")))
  (deferred:nextc it                    ;実行結果をリストで渡る
    (lambda (buffers)
      (display-buffer
       (with-current-buffer (get-buffer-create "*temp*")
         (erase-buffer)
         (loop for i in buffers
               do
               (insert-image
                (with-current-buffer i
                  (create-image
                   (substring (buffer-string) (+ (string-match "\n\n" (buffer-string)) 2))
                   'png t)))
               (insert "\n\n"))
         (current-buffer))))))

;;; -------------------------------------------------------------
;;; try-ctach-finally
;;; -------------------------------------------------------------
(deferred:$
  ;; try ブロック
  (deferred:next
    (lambda () (message "try block ......")))
  (deferred:nextc it
    (lambda () (message "%d" (read-minibuffer "input a bumber :"))))
  ;; catch ブロック
  (deferred:error it
    (lambda (err) (message "catch error : %s" err)))
  ;; finallyブロック --> 複数書ける
  (deferred:nextc it
    (lambda () (message "finally!!!"))))

;;; マクロで書く
(deferred:$
  (deferred:try                         ;deferredオブジェクト
    (deferred:$
      (deferred:next
        (lambda () (message "try block ......")))
      (deferred:nextc it
        (lambda () (message "%d" (read-minibuffer "input a bumber :")))))
    :catch
    (lambda (err) (message "catch error : %s" err))
    :finally
    (lambda () (message "finally!!!"))))

;;; -------------------------------------------------------------
;;; deferred:timeout と deferred:earlier
;;; -------------------------------------------------------------
;; ----> deferred:earlier
(deferred:$
  (deferred:earlier                     ;; 一番早く完了した処理の結果を次の処理に渡します、たの処理を停止する
    ;; ここに複数deferredを書く
    (deferred:process "sh" "-c" "sleep 3 | echo 'hello!'")
    (deferred:$
      (deferred:wait 1000) ; timeout msec
      (deferred:nextc it (lambda () "canceled!"))))
  (deferred:nextc it
    (lambda (x) (print x))))

;; ----> deferred:timeout
(deferred:$
  (deferred:timeout 1000
    ;; timeout時の処理
    (let ((msg "canceled!")) (message msg) msg) ;必ず実行される
    ;; main処理
    (deferred:process "sh" "-c" "sleep 3 | echo 'hello!'"))
  (deferred:nextc it
    (lambda (x) (message "last result: %s" x))))

;;; -------------------------------------------------------------
;;; deferred:cancel タスクのキャンセル
;;; -------------------------------------------------------------
(deferred:$
  (deferred:next (lambda () (message "start!!!")))
  (deferred:nextc it (lambda () (message "task 001")))
  (deferred:wait 3000)
  (progn
    (setq temp-task (deferred:nextc it (lambda () (message "task 002"))))
    temp-task)
  (deferred:nextc it (lambda () (message "task 003")))
  (deferred:nextc it (lambda () (message "task 004")))
  )
(deferred:cancel temp-task)

;;; -------------------------------------------------------------
;;; deferred:watch 監視タスク
;;; -------------------------------------------------------------
(deferred:$
  (deferred:next (lambda () (message "start!!!")))
  (deferred:nextc it (lambda () (read-minibuffer "input a number: ")))
  (deferred:watch it (lambda () (message "start watch")))
  (deferred:nextc it (lambda (x) (message "number = %d" x))))

;;; -------------------------------------------------------------
;;; ユーティリティ
;;; -------------------------------------------------------------
(deferred:loop '(1 2 3 4 5)             ;mapcのようなループ
  (lambda (x) (message "x = %d" x)))

;;; funcall 関数の非同期呼び出す
(deferred:call 'message "nihao %s" "akira")

;;; apply 関数の非同期呼び出す
(deferred:apply 'message '("nihao %s" "akira"))

;;; start-process, start-process-shell-command のラッパー
(deferred:$
  (deferred:process "ls" "-al")         ;実行結果が文字列で次のタスクに渡る
  (deferred:nextc it (lambda (x) (message x))))

(deferred:$
  (deferred:process-buffer "ls" "-al")         ;実行結果をバッファーオブジェクトで次のタスクに渡る
  (deferred:nextc it (lambda (buf) (display-buffer buf))))

;;; -------------------------------------------------------------
;;; インスタンスメソッド
;;; -------------------------------------------------------------
;;; 動的に実行chinaを組み立て
(let* ((start-task (deferred:new (lambda () (message "first !!!!"))))
       (next-task start-task) error-task)
  (lexical-let ((idx 0))
    (loop for i from 1 to 3
          do (setq next-task
                   (deferred:nextc next-task
                     (lambda () (message "task number : %d" (incf idx)))))))
  ;; error handler
  (setq error-task (deferred:error
                     next-task
                     (lambda (err) (message "got error! %s" err))))
  ;; finally handler
  (deferred:nextc
    error-task
    (lambda (err) (message "finally!!!")))

  ;;非同期でタスクを開始する
  (deferred:callback-post start-task)
  ;; 同期でタスクを起動する
  ;;(deferred:callback start-task)
  )

;;; -------------------------------------------------------------
;;; sample
;;; -------------------------------------------------------------
;;; deferred:lambda を使用する
(lexical-let ((count 0) (anm "-/|\\-")
              (end 50) (pos (point))
              (wait-time 50))
  (deferred:$
    (deferred:next
      (lambda (x) (message "Animation started.")))

    (deferred:nextc it
      (deferred:lambda (x)
        (save-excursion
          (when (> count 0)
            (goto-char pos) (delete-char 1))
          (insert (char-to-string
                   (aref anm (% count (length anm))))))
        (if (> end (incf count)) ; 止める場合はdeferredでないものを返す（この場合はnil）
            (deferred:nextc (deferred:wait wait-time) self)))) ; 続けるときはdeferredを返す

    (deferred:nextc it
      (lambda (x)
        (save-excursion
          (goto-char pos) (delete-char 1))
        (message "Animation finished.")))))

;;; 実行チェーンの組み立て
(lexical-let ((count 0) (anm "-/|\\-")
              (end 50) (pos (point))
              (wait-time 50))

  (let (start-task next-task end-task)
    (setq start-task
          (deferred:new (lambda (x) (message "Animation started."))))
    (setq next-task start-task)
    (loop for i from 1 to end
          do (setq next-task (deferred:nextc next-task
                               (lambda ()
                                 (save-excursion
                                   (when (> count 0)
                                     (goto-char pos) (delete-char 1))
                                   (insert (char-to-string
                                            (aref anm (% count (length anm)))))
                                   (incf count)
                                   ))))
          (setq next-task (deferred:nextc next-task
                            (lambda () (deferred:wait wait-time))))
          )
    (setq end-task
          (deferred:nextc next-task
            (lambda (x)
              (save-excursion
                (goto-char pos) (delete-char 1))
              (message "Animation finished."))))
    (deferred:callback-post start-task)
    ))


;;; 実行キューを空にして止めることが出来ます
(deferred:clear-queue)
