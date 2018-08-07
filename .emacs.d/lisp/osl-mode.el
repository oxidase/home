;;; Code:
(defgroup osl nil
  "Major mode for editing OSL files."
  :prefix "osl-"
  :group 'languages)

;;;###autoload
(define-derived-mode osl-mode idl-mode "OSL"
  "Major mode for editing OSL files."
  :group 'osl

  (setq-local comment-use-syntax t)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.osl\\'" . osl-mode))


;(defvar osl-mode-keywords
;  '(("\\(stored\\|property\\)" 1 font-lock-function-keyword-face)))

;(font-lock-add-keywords 'osl-mode osl-mode-keywords)


(provide 'osl-mode)
