;;; init-golang.el --- Support for the Golang language -*- lexical-binding: t; -*-
;;; Commentary:
;;; Modern Go setup: go-ts-mode (tree-sitter, requires the go grammar in
;;; ~/.emacs.d/tree-sitter/), lsp-mode with gopls, and goimports on save.
;;; Code:

(defconst my/go-ts-available
  (and (fboundp 'treesit-ready-p) (treesit-ready-p 'go))
  "Whether the Go tree-sitter grammar can be loaded.")

;;; Major mode -----------------------------------------------------------

(if my/go-ts-available
    (progn
      (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
      (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))
  ;; Fall back to the classic mode when the grammar is missing (e.g. CI).
  (when (maybe-require-package 'go-mode)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))))

(defun my/golang-mode-p ()
  "Return t when the current buffer is any flavour of Go major mode."
  (derived-mode-p 'go-ts-mode 'go-mode))

;;; Formatting with goimports --------------------------------------------

(defun my/go-format-buffer ()
  "Format the current Go buffer with goimports."
  (when (and (my/golang-mode-p) (executable-find "goimports"))
    (let ((point-pos (point))
          (window-pos (window-start)))
      (call-process-region (point-min) (point-max) "goimports" t t nil
                           "/dev/stdin")
      (goto-char point-pos)
      (set-window-start (selected-window) window-pos))))

(defun my/go-setup-buffer ()
  "Common setup for Go buffers."
  (setq tab-width 8
        indent-tabs-mode t)
  (add-hook 'before-save-hook #'my/go-format-buffer nil t))

(add-hook 'go-ts-mode-hook #'my/go-setup-buffer)
(add-hook 'go-mode-hook #'my/go-setup-buffer)

;;; LSP (gopls) ----------------------------------------------------------

(when (maybe-require-package 'lsp-mode)
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))
  (setq lsp-go-use-placeholders t
        lsp-gopls-staticcheck t
        ;; Complete function calls with argument placeholders.
        lsp-gopls-complete-unimported t)
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none))
  (when (executable-find "gopls")
    (add-hook 'go-ts-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'lsp-deferred))

  ;; On-save formatting/imports via LSP when gopls is running; the
  ;; goimports hook above already covers this, so keep LSP formatting off
  ;; to avoid double work.

  ;; Navigation that works both with and without LSP.
  (defun my/go-jump-def ()
    "Jump to the definition at point via LSP when available."
    (interactive)
    (if (and (fboundp 'lsp-mode) (bound-and-true-p lsp-mode))
        (call-interactively #'lsp-find-definition)
      (call-interactively #'xref-find-definitions)))
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "M-.") #'my/go-jump-def))
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "M-.") #'my/go-jump-def)))

(provide 'init-golang)
;;; init-golang.el ends here
