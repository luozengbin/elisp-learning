;;; mapcとmapcarの区別
;;; mapcは引数のリストをそのまま帰す
;;; mapcarは関数適用後の計算結果をリストで返す

(mapc '1+
      '(1 2 3))                         ; => (1 2 3)

(mapcar '1+
        '(1 2 3))                       ; => (2 3 4)

(message "nihao")

