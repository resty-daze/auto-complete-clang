(defun my-c-mode-common-hook()
  (setq ac-auto-start nil)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-delay 0.3)
  (define-key c-mode-base-map (kbd "M-/") 'auto-complete)

  (setq ac-sources '(ac-source-clang-complete))
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
