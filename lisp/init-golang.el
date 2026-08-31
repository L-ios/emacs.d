;;; init-golang.el --- Support for the Golang language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; go-mode https://github.com/dominikh/go-mode.el
(when (maybe-require-package 'go-mode)
  ;; Call Gofmt before saving
  (setq gofmt-command "goimports")
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              ))
  (add-hook 'before-save-hook 'gofmt-before-save)

  (with-eval-after-load 'go-mode
    ;; Godef jump key binding
    (define-key go-mode-map (kbd "M-,") 'godef-jump)
    (define-key go-mode-map (kbd "M-.") 'pop-tag-mark)))

(when (maybe-require-package 'lsp-mode)
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))

  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none))
  (add-hook 'go-mode-hook 'lsp-deferred)

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))


;;; go-eldoc https://github.com/emacsorphanage/go-eldoc
(when (maybe-require-package 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

(when (maybe-require-package 'yasnippet-snippets))
;;; flymake-go
;;; go-autocomplete
;;; go-snappets
;;; goflymake
;;; eldoc

(provide 'init-golang)
;;; init-golang.el ends here
