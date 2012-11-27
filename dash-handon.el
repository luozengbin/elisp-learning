;;; link https://github.com/magnars/dash.el

(require 'dash)

;;; -map
(-map (lambda (x) (message "%d" x))
      '(1 2 3 4 5))                     ; => ("1" "2" "3" "4" "5")
(--map (message "%d" it) '(1 2 3 4))    ; => ("1" "2" "3" "4")
(defun square (n) (* n n))
(-map 'square '(1 2 3 4))               ; => (1 4 9 16)

;;; -reduce-from
(-reduce-from '+  7 '(1 2 ))            ; => 10

(-reduce-from (lambda (memo item)
                (message "|%d   %d|" memo item) ; => "|7   1|", "|8   2|"
                (+ memo item)           ; => 8, 10
                )
              7
              '(1 2)) ; => 10

(--reduce-from (+ acc it) 7 '(1 2 3))   ; => 13

(--reduce-from
 (progn
   (message "|%d   %d|" acc it)         ; => "|7   1|", "|8   2|", "|10   3|"
   (+ acc it)                           ; => 8, 10, 13
   )
  7 '(1 2 3))                           ; => 13

;;; -reduce

(-reduce '+ '(1 2)) ;; => 3
(-reduce (lambda (memo item)
           (format "%s   %s" memo item) ; => "1   2", "1-2   3"
           (format "%s-%s" memo item)   ; => "1-2", "1-2-3"
           )
         '(1 2 3)) ;; => "1-2-3"
(--reduce (format "%s-%s" acc it) '(1 2 3)) ;; => "1-2-3"

;;; -filter
(-filter (lambda (num)
           num                          ; => 1, 2, 3, 4
           (= 0 (% num 2))              ; => nil, t, nil, t
           )
         '(1 2 3 4)) ;; => (2 4)
;; (-filter 'even? '(1 2 3 4))             ; =>
(--filter (= 0 (% it 2)) '(1 2 3 4))    ; => (2 4)

;;; -keep
(-keep 'cdr '((1 2 3) (4 5) (6)))       ; => ((2 3) (5))
(-keep (lambda (num)
         num                            ; => 1, 2, 3, 4, 5, 6
         (when (> num 3)
           (* 10 num)                   ; => 40, 50, 60
           ))
       '(1 2 3 4 5 6)) ; => (40 50 60)
(--keep (when (> it 3) (* 10 it)) '(1 2 3 4 5 6)) ; => (40 50 60)

;;; -map-when
;; (-map-when 'even? 'square '(1 2 3 4))
(--map-when (> it 2)
            (* it it)                   ; => 9, 16
            '(1 2 3 4))                 ; => (1 2 9 16)
(--map-when (= it 2) 17 '(1 2 3 4))     ; => (1 17 3 4)

;;; -flatten
(-flatten '((1)))                       ; => (1)
(-flatten '((1 (2 3) (((4 (5)))))))     ; => (1 2 3 4 5)

;;; -concat (&rest lists)
(-concat '(1)) ;; => (1)
(-concat '(1) '(2)) ;; => (1 2)
(-concat '(1) '(2 3) '(4)) ;; => (1 2 3 4)
(-concat '(1) '(2 (3)) '(4)) ;; => (1 2 (3) 4)

;;; -mapcat (fn list)
(-mapcat 'list '(1 2 3)) ;; => (1 2 3)
(-mapcat (lambda (item) (list 0 item)) '(1 2 3)) ;; => (0 1 0 2 0 3)
(--mapcat (list 0 it) '(1 2 3)) ;; => (0 1 0 2 0 3)

;;; -any?
;; (-any? 'even? '(1 2 3)) ;; =>
;; (-any? 'even? '(1 3 5)) ;; =>
(--any? (= 0 (% it 2)) '(1 2 3)) ;; => t

;;; -all?
;; (-all? 'even? '(1 2 3)) ;; => nil
;; (-all? 'even? '(2 4 6)) ;; => t
(--all? (= 0 (% it 2)) '(2 4 6))        ; => t
(--all? (= 0 (% it 2)) '(2 3 6))        ; => nil

;;; -none
;; (-none? 'even? '(1 2 3)) ;; => nil
;; (-none? 'even? '(1 3 5)) ;; => t
(--none? (= 0 (% it 2)) '(1 2 3)) ;; => nil
(--none? (= 0 (% it 2)) '(1 3 5)) ;; => t

;;;
(let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s))))) ;; => nil
(let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s)))) s) ;; => '(3 2 1)
(let (s) (--each '(1 2 3) (setq s (cons it s))) s) ;; => '(3 2 1)
