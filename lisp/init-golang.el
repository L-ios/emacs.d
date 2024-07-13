;;; init-golang.el --- Support for the Golang language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; go-mode https://github.com/dominikh/go-mode.el
(when (maybe-require-package 'go-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;;; --------------- 代优化配置
  ;; Call Gofmt before saving
  (setq gofmt-command "goimports")
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  ;;autocomplete
  ;; (set (make-local-variable 'company-backends) '(company-go))
  ;; (company-mode)
  ;; Godef jump key binding
  ;; (local-set-key (kbd "M-,") 'godef-jump)
  ;; (local-set-key (kbd "M-.") 'pop-tag-mark)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)))


  (when (maybe-require-package 'lsp-mode)
    (add-hook 'go-mode-hook 'lsp-deferred)

    ;; Set up before-save hooks to format buffer and add/delete imports.
    ;; Make sure you don't have other gofmt/goimports hooks enabled.
    (defun lsp-go-install-save-hooks ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t))
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

    ;; lsp-register-custom-settings 无法找到
    ;; (lsp-register-custom-settings
    ;;  ;; other gopls setting see https://github.com/golang/tools/blob/master/gopls/doc/settings.md
    ;;  '(("gopls.completeUnimported" t t)
    ;;    ("gopls.staticcheck" t t)))
    )

;;; go-eldoc https://github.com/emacsorphanage/go-eldoc
  (when (maybe-require-package 'go-eldoc)
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold))

  (when (maybe-require-package 'yasnippet-snippets)))

;;; flymake-go
;;; go-autocomplete
;;; go-snappets
;;; goflymake
;;; eldoc

(provide 'init-golang)
;;; init-golang.el ends here
