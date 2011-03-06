(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)

(defun my-c-mode-common-hook()
  (setq ac-auto-start nil)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-delay 0.3)
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
