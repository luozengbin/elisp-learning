(defun write-region-test-001 ()
  "-> Added to ./temp/hoge.txt."
  (interactive)
  (write-region (point-min) (point-max) "./temp/hoge.txt" t) ;追記する場合
  )

(defun write-region-test-002 ()
  "-> Wrote /home/m/hoge.txt."
  (interactive)
  (write-region (point-min) (point-max) "./temp/hoge.txt" nil) ;追記しない場合
  )

