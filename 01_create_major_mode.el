(defun my-mode()
  (interactive)
  (setq major-mode 'my-mode mode-name "私のモード")
  (setq my-local-map (make-sparse-keymap))
  (define-key my-local-map "h" 'backward-char)
  (define-key my-local-map "j" 'next-line)
  (define-key my-local-map "k" 'previous-line)
  (define-key my-local-map "l" 'forward-char)
  (define-key my-local-map "\C-ch" 'hello-world)
  (use-local-map my-local-map)
  )

(defun hello-world()
  (interactive)
  (insert "Hello, world!\n")
  )


(defun my-backward-delete-fun()
  "backward delete a char or delete selected region automic"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'kill-region)
    (call-interactively 'backward-delete-char-untabify)
    )
  )

(global-set-key [backspace] 'my-backward-delete-fun)