;; calfwカレンダーコンポネントでカスタマイズ練習を行う

;; cfw:source-data の簡単な例
(defun sample-data1 (b e)
  '(
    ((10 4  2011) . ("内容1"))          ;2011/10/04に表示される
    ((10 10 2011) . ("内容2" "内容2行目")) ;2011/10/10 に２行で表示される
    ))

(cfw:open-calendar-buffer
 :contents-sources
 (list 
  (make-cfw:source :name "test1" :data 'sample-data1)))

;; cfw:source-data 期間スケジュールの例
(defun sample-data2 (b e)
  '(
    ((10  8 2011) . ("内容1"))
    (periods
     ((10 8 2011) (10 9 2011) "期間1")
     ((10 11 2011) (10 12 2011) "次の期間"))))

(cfw:open-calendar-buffer
 :contents-sources
 (list 
  (make-cfw:source
   :name "test2" :data 'sample-data2)))

;; 以下の式を評価するとcalfwがリジョンでcurrentバッファーに挿入される
(cfw:create-calendar-component-region :height 10)

;; M-x cfw:get-calendar-text



