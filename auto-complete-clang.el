(eval-when-compile (require 'cl))

(defvar clang-executable "clang")
(defvar clang-completion-doc-table (make-hash-table :test 'equal))

;; faces
(defface clang-completion-plain-face
  '((t (:inherit default :family "Verdana")))
  "clang completion hint base font" :group 'clang-completion-faces)

(defface clang-completion-type-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#729FCF" :weight bold :family "Verdana")))
  "clang completion hint font for types" :group 'clang-completion-faces)

(defface clang-completion-variable-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#73D216" :family "Verdana")))
  "clang completion hint font for variables" :group 'clang-completion-faces)

;; extra flags
(defvar clang-completion-pch nil)
(defvar clang-completion-flags nil)
(defvar clang-completion-suppress-error nil)

(defun clang-process-exec (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (or (eq (apply 'call-process (car command) nil '(t ".clang-completion-error") nil (cdr command)) 0)
                  clang-completion-suppress-error)
        (let ((last-command compile-command))
          (compile "cat .clang-completion-error")
          (setq compile-command last-command))))))

(defun clang-parse-completion-line (line)
  (cond ((string-match "^COMPLETION: Pattern" line) nil)  ;; exclude patterns
        ((string-match "^COMPLETION: \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
         (list (match-string 1 line) (match-string 2 line)))
        ((string-match "^OVERRIDE:  \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
         (list (match-string 1 line) (match-string 2 line)))
        (t nil))
  )

(defvar file-options-list '(nil))
(defvar dir-options-list '(nil))
(defvar clang-search-levels 3)

(defun get-cur-file-options-list ()
  "Get clang options for current file"
  (save-excursion
    (let ((cur-options-list '("")) (file-name (buffer-file-name)))
      ;; This regex sucks, fix me
      (while (re-search-forward "@clang:[[:space:]]*\\(-[^[:space:]]+\\)" nil t nil)
        (setq cur-options-list
              (cons (match-string-no-properties 1)
                    cur-options-list)))
      (list file-name cur-options-list))))

(defun clang-read-options-from-file (file-name)
  "Read clang options from given file"
  (let ((dir-options-list '("")) (option nil))
    (with-temp-buffer
      (insert-file-contents file-name)
      (goto-char (point-min))
      (while (re-search-forward "-[^[:space:]\n]+" nil t nil)
        ;; Todo, add in relative paths support
        (setq option (match-string-no-properties 0))
        (setq dir-options-list
              (cons option
                    dir-options-list)))
      dir-options-list)))

(defun get-dir-options-list ()
  "Go up clang-search-levels directory and look for clangOptions"
  (let ((n 3)
        (cur-dir (replace-regexp-in-string "^Directory " "" (pwd)))
        (dir-options-list '("")))
    (while (> n 0)
      (if (file-exists-p (concat cur-dir "clangOptions"))
          (progn
            (setq dir-options-list
                  (list
                   cur-dir
                   (clang-read-options-from-file (concat cur-dir "clangOptions"))))
            (setq n 0))
        (setq cur-dir (replace-regexp-in-string "[^/]+/$" "" cur-dir))
        (setq n
              (if (string= cur-dir "/")
                  0
                (- n 1)))))
    dir-options-list))

(defun get-file-embedded-options ()
  "Read embedded options in current file"
  (let ((cur-file-in-list nil) (temp-options-list file-options-list) (cur-options-list nil))
    (while (and
            (setq cur-file-in-list (pop temp-options-list))
            (not (string=
                  (car cur-file-in-list)
                  (buffer-file-name)))))
    (if cur-file-in-list
        (let ((temp (car (cdr cur-file-in-list))))
          (if temp
              temp
            '("")))
      (setq clang-options-checked nil)
      (setq cur-options-list (get-cur-file-options-list))
      (setq file-options-list
            (cons cur-options-list file-options-list))
      (car (cdr cur-options-list)))))

(defun compare-dir-options-element (e1 e2)
  "Compare 2 dir-options, according to the length of dir path"
  (if (not e2)
      nil
    (> (length (car e1))
       (length (car e2)))))

;; Sometimes, this relies on get-file-embedded-options to run first
;; in order to work correctly
(defun get-dir-embedded-options ()
  "Read embedded options in dirs"
  (let ((cur-dir-in-list nil) (cur-file-name (buffer-file-name))
        (dir-temp-options-list dir-options-list) (temp-list nil))
    (while (and
            (setq cur-dir-in-list (pop dir-temp-options-list))
            (not (string=
                  (car cur-dir-in-list)
                  (substring cur-file-name 0
                             (let ((dir-length (length (car cur-dir-in-list)))
                                   (file-length (length cur-file-name)))
                               (if (> dir-length file-length)
                                   file-length
                                 dir-length)))))))

    (if (and cur-dir-in-list clang-options-checked)
        (car (cdr cur-dir-in-list))
      (setq temp-list (get-dir-options-list))
      (setq dir-options-list
            ;; Append in an order determined by string length of directory path
            (sort (cons temp-list dir-options-list)
                  'compare-dir-options-element))
      (car (cdr temp-list)))))

(defun clang-process (buffer point)
  (defvar clang-options-checked)
  (make-local-variable 'clang-options-checked)
  (setq clang-options-checked t)
  (unless (buffer-file-name buffer)
    (return ""))
  (let* ((filename (buffer-file-name buffer))
         (col      (1+ (- point (point-at-bol))))
         (row      (count-lines point (point-min)))
         (cmd      
          (list clang-executable "-cc1"
                filename "-fsyntax-only" "-code-completion-at"
                (format "%s:%s:%s" filename row col))))
    ;; Don't interchange the order of (get-dir-embedded-options) and (get-file-embedded-options)
    (setq cmd (append cmd (get-file-embedded-options) (get-dir-embedded-options)))

    (when (listp clang-completion-flags)
      (setq cmd (append cmd clang-completion-flags)))
    (when (stringp clang-completion-pch)
      (setq cmd (append cmd (list "-include-pch" clang-completion-pch))))
    (message (format "complete at %s:%s:%s" filename row col))
    (clang-process-exec cmd)))

(defun clang-get-process-result (string)
  (let* ((completion-lines (split-string string "\n")))
    (delq nil (mapcar 'clang-parse-completion-line completion-lines))))

(defun clang-get-process-completion-result (string)
  (mapcar 'car (clang-get-process-result string)))

(defun clang-get-process-prototype-table (string)
  (let* ((lines (clang-get-process-result string))
         (result-table (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let* ((key (first line))
             (value (gethash key result-table)))
        (setq value (append value (list (second line))))
        (puthash key value result-table))
      )
    (setq clang-completion-doc-table result-table)))

(defun clang-get-completions (&optional buffer point)
  ;; save all modified buffers
  (or buffer (setq buffer (current-buffer)))
  (or point (setq point (point)))
  (save-some-buffers t)
  (let* ((output (clang-process buffer point)))
    (clang-get-process-prototype-table output)
    (clang-get-process-completion-result output)))

(defun filter-doc-buffer ()
  (while (re-search-backward "\\[#.*?::#\\]" nil t)
    (replace-match ""))
  (goto-char (point-max))

  (while (re-search-backward "\\[#\\|#\\]" nil t)
    (replace-match " "))
  (goto-char (point-max))
  (while (re-search-backward "{#\\|#}\\|<#\\|#>" nil t)
    (replace-match ""))
  )

(defun clang-get-doc (symbol)
  ;;(setq symbol (symbol-name (intern-soft symbol)))
  (let ((reslist (gethash symbol clang-completion-doc-table)))
    (with-temp-buffer
      (font-lock-add-keywords nil '(("\\[#\\(.*?\\)#\\]" 1
                                     'clang-completion-type-face t)))
      (font-lock-add-keywords nil '(("<#\\(.*?\\)#>" 1
                                     'clang-completion-variable-face t)))
      (font-lock-add-keywords nil '(("\\(.*\\)" 1
                                     'clang-completion-plain-face t)))
      (font-lock-mode t)

      (insert (reduce '(lambda (x y) (concat x "\n" y)) reslist))
      (font-lock-fontify-buffer)
      (filter-doc-buffer)

      (message (buffer-string))))
  ;;(with-temp-buffer
  ;;  (dolist (proto reslist)
  ;;    (insert proto)
  ;;    (insert "\n\n"))
  ;;  (filter-doc-buffer)
  ;;  (buffer-string))
  ;; display nothing
  (return nil))

(defvar ac-source-clang-complete
  '((candidates . (clang-get-completions nil ac-point))
    (prefix "[^a-zA-Z0-9_]\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . clang-get-doc)
    (requires . 0)
    (symbol . "C")
    (cache)))

;;(defvar ac-source-clang-static-complete
;;  '((candidates . (clang-get-completions nil ac-point))
;;    (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
;;    ;;(document . 'clang-get-doc)
;;    (requires . 0)
;;    (symbol . "M")
;;    (cache)))

(defun ac-complete-clang ()
  (interactive)
  (auto-complete '(ac-source-clang-complete)))

(provide 'auto-complete-clang)
