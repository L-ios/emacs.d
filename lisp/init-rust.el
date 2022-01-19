;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; base https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

(when (maybe-require-package 'rustic)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)

  (when (maybe-require-package 'lsp-mode)
    ;; what to use when checking on-save. "check" is default, I prefer clippy
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")
    ;; todo 是否为左侧的的渲染显示
    (setq lsp-eldoc-render-all t)
    (setq lsp-idle-delay 0.6)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (add-hook 'rustic-mode-hook #'lsp)

    (when (maybe-require-package 'lsp-ui)
      (setq lsp-ui-peek-always-show t)
      (setq lsp-ui-sideline-show-hover t)
      (setq lsp-ui-doc-enable nil)
      (add-hook 'lsp-mode-hook #'lsp-ui)))

  (when (maybe-require-package 'company)
    ;; how long to wait until popup
    (setq company-idle-delay 0.5)
    ;; uncomment to disable popup
    ;; (setq company-begin-commands nil)

    (with-eval-after-load 'rustic-mode
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map (kbd "M-<") 'company-select-first)
      (define-key company-active-map (kbd "M->") 'company-select-last)
      (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)
      (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))
    (add-hook 'rustic-mode #'company)))


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
  ;;(yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode))
;;; add for org
(maybe-require-package 'ob-rust)

(when (maybe-require-package 'flycheck-rust)
  (with-eval-after-load 'rustic-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)
;;; init-rust.el ends here
