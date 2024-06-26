;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; base https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

(when (maybe-require-package 'rustic)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (when (maybe-require-package 'lsp-mode)
    ;; uncomment for less flashiness
    ;; (setq lsp-eldoc-hook nil)
    ;; (setq lsp-enable-symbol-highlighting nil)
    ;; (setq lsp-signature-auto-activate nil)

    ;; todo 是否为左侧的的渲染显示
    (setq lsp-eldoc-render-all t)

    (setq lsp-diagnostics-provider :none)
    (setq lsp-eldoc-enable-hover nil)
    (setq lsp-idle-delay 0.6)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
    (setq lsp-rust-analyzer-display-chaining-hints t)
    (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
    (setq lsp-rust-analyzer-display-closure-return-type-hints t)
    (setq lsp-rust-analyzer-display-parameter-hints nil)
    (setq lsp-rust-analyzer-display-reborrow-hints nil)
    (setq lsp-rust-analyzer-cargo-watch-enable nil)
    ;; what to use when checking on-save. "check" is default, I prefer clippy
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-signature-render-documentation nil)
    (with-eval-after-load 'rustic
      (define-key rustic-mode-map (kbd "TAB") 'company-indent-or-complete-common)
      (define-key rustic-mode-map (kbd "C-c C-c e") 'lsp-execute-code-action)
      (define-key rustic-mode-map (kbd "C-c C-c r") 'lsp-rename)
      (define-key rustic-mode-map (kbd "C-c C-c q") 'lsp-workspace-restart)
      (define-key rustic-mode-map (kbd "C-c C-c Q") 'lsp-workspace-shutdown)
      (define-key rustic-mode-map (kbd "C-c C-c s") 'lsp-rust-analyzer-status))

    (add-hook 'rustic-mode-hook #'lsp)

    (when (maybe-require-package 'lsp-ui)
      (with-eval-after-load 'rustic
        (define-key rustic-mode-map (kbd "C-c i") 'lsp-ui-doc-focus-frame)
        (define-key rustic-mode-map (kbd "C-q") 'lsp-ui-imenu)
        (define-key rustic-mode-map (kbd "M-?") 'lsp-find-references))
      (add-hook 'lsp-ui-mode-hook
                (lambda ()
                  (setq lsp-ui-peek-always-show t)
                  (setq lsp-ui-sideline-show-hover t)
                  (setq lsp-ui-sideline-show-code-actions nil)
                  (setq lsp-ui-sideline-delay 3)
                  (set-face-attribute 'lsp-ui-sideline-global t :height 0.75)
                  (when (display-graphic-p)
                    (setq lsp-ui-doc-enable t)
                    (setq lsp-ui-doc-max-height 20)
                    (setq lsp-ui-doc-position 'at-point)
                    (setq lsp-ui-doc-show-with-cursor t)
                    (setq lsp-ui-doc-delay 3))))
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

  (when (maybe-require-package 'company)
    ;; how long to wait until popup
    (setq company-idle-delay 3)
    (setq company-tooltip-align-annotations t)


    ;; uncomment to disable popup
    (setq company-begin-commands nil)

    (with-eval-after-load 'rustic
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map (kbd "M-<") 'company-select-first)
      (define-key company-active-map (kbd "M->") 'company-select-last)
      (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)
      (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))
    (add-hook 'rustic-mode #'company))
  (when (maybe-require-package 'toml-mode)))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(when (maybe-require-package 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (with-eval-after-load 'rustic
    (yas-reload-all))
  (maybe-require-package 'yasnippet-snippets))

;;; add for org
(maybe-require-package 'ob-rust)

(when (maybe-require-package 'flycheck-rust)
  (with-eval-after-load 'rustic
    (define-key rustic-mode-map (kbd "C-c C-c l") 'flycheck-list-errors)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)
;;; init-rust.el ends here
