;; -*- coding: utf-8 -*-
;; 2010-03-27
;; print file names of files that have n occurrences of a string, of a given dir

;; input dir
(setq inputDir "D:/temp" )

;; add a ending slash if not there
;; in elisp, dir path should end with a slash
(when (not (string= "/" (substring inputDir -1) ))
  (setq inputDir (concat inputDir "/") )
  )

(defun my-process-file (fPath)
  "Process the file at FPATH …"
  (let (myBuffer p1 p2 (ii 0) searchStr)

    (when t
      ;; (and (not (string-match "/xx" fPath)) ) ; exclude some dir

      ;; create a temp buffer. Work in temp buffer. Faster.
      (setq myBuffer (get-buffer-create " myTemp"))
      (set-buffer myBuffer)
      (insert-file-contents fPath nil nil nil t)

      (setq searchStr "我" ) ; search string here

      (goto-char 1)
      (while (search-forward searchStr nil t)
        (setq ii (1+ ii))
        )

      ;; report if the occurrence is not n times
      (if (not (= ii 0))
          (princ (format "this many: %d %s\n" ii fPath))
        )

      (kill-buffer myBuffer)
      )
    ))

;; traverse the dir

(require 'find-lisp)

(let (outputBuffer)
  (setq outputBuffer "*xah occur output*" )
  (with-output-to-temp-buffer outputBuffer 
    (mapc 'my-process-file (find-lisp-find-files inputDir ".*$"))
    (princ "Done deal!")
    )
  )
