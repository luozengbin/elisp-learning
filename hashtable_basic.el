;;; link http://www42.atwiki.jp/elisp/pages/13.html

;;; define a hashtable
(setq h (make-hash-table :test #'equal))       ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())

;;; 要素を追加する
(puthash "apple" 150 h)                        ; => 150
(puthash "banana" 200 h)                       ; => 200
(puthash "lemon" 300 h)                        ; => 300
h                                              ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("apple" 150 "banana" 200 "lemon" 300))

(puthash "fruits" '("apple" . "banana") h)     ; => ("apple" . "banana")
(puthash "animal" '("lion" "monkey" "fish") h) ; => ("lion" "monkey" "fish")
h                                              ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("apple" 150 "banana" 200 "lemon" 300 "fruits" ("apple" . "banana") "animal" ("lion" "monkey" "fish")))

;;; キーに関連付けられた値を取得する
(gethash "apple" h)                            ; => 150
(gethash "banana" h)                           ; => 200
(gethash "lemon" h)                            ; => 300
(gethash "papaia" h)                           ; => nil
(gethash "papaia" h 0)                         ; => 0
h                                              ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("apple" 150 "banana" 200 "lemon" 300 "fruits" ("apple" . "banana") "animal" ("lion" "monkey" "fish")))
(gethash "fruits" h)                           ; => ("apple" . "banana")
(gethash "animal" h)                           ; => ("lion" "monkey" "fish")
h                                              ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("apple" 150 "banana" 200 "lemon" 300 "fruits" ("apple" . "banana") "animal" ("lion" "monkey" "fish")))

;;; CLライブラリでhashに値を代入する
(eval-when-compile (require 'cl))
(setq x (make-hash-table :test #'equal))       ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
(setf (gethash "name" x) "akira")              ; => "akira"
x                                              ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("name" "akira"))

;;; ハッシュ内にキーが存在するかどうか調べる
(defun hash-exists-p (key table)
  (let ((novalue (make-symbol "<nil>")))
    (not (eq (gethash key table novalue) novalue))))

(hash-exists-p "apple" h)   ; => t
(hash-exists-p "pine" h)    ; => nil

;; ハッシュの要素数を取得する
(puthash "age" 28 x)        ; => 28
(puthash "weight" "51kg" x) ; => "51kg"
(hash-table-count x)        ; => 3

;;; ハッシュからエントリを削除する
(hash-table-count x)        ; => 3
(remhash "age" x)           ; => nil
(hash-table-count x)        ; => 2

;; ハッシュの全エントリに対してブロックを実行する
(maphash #'(lambda (key value)
             (princ (format "key=>%S,value=>%S\n" key value)))
         x)                 ; => nil

;;; ハッシュを配列に変換する
x                           ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("name" "akira" "weight" "51kg"))
(clrhash x)                 ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
