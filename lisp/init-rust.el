;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; base https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/


(when (maybe-require-package 'rustic)
  ;; comment to disable rustfmt on save
  (setq rust-rustfmt-switches '("--edition" "2021"))
  (setq rustic-rustfmt-args '("--edition" "2021"))
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer)
  (with-eval-after-load 'rustic
    (define-key rustic-mode-map (kbd  "M-?") 'lsp-find-references)
    (define-key rustic-mode-map (kbd "C-c C-c a")  'lsp-execute-code-action)
    (define-key rustic-mode-map (kbd  "C-c C-c d") 'dap-hydra)
    (define-key rustic-mode-map (kbd  "C-c C-c h") 'lsp-ui-doc-glance)
    ;; 使用corfu替代
    ;; (define-key rustic-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (define-key rustic-mode-map (kbd "C-c C-c e") 'lsp-execute-code-action)
    (define-key rustic-mode-map (kbd "C-c C-c m") 'lsp-rust-analyzer-expand-macro)
    (define-key rustic-mode-map (kbd "C-c C-c l") 'flycheck-list-errors)
    (define-key rustic-mode-map (kbd "C-c C-c r") 'lsp-rename)
    (define-key rustic-mode-map (kbd "C-c C-c q") 'lsp-workspace-restart)
    (define-key rustic-mode-map (kbd "C-c C-c Q") 'lsp-workspace-shutdown)
    (define-key rustic-mode-map (kbd "C-c C-c s") 'lsp-rust-analyzer-status)
    ;; lsp-ui
    (define-key rustic-mode-map (kbd "C-c i") 'lsp-ui-doc-focus-frame)
    (define-key rustic-mode-map (kbd  "M-j") 'lsp-ui-imenu)
    (define-key rustic-mode-map (kbd "C-c C-c d") 'lsp-ui-peek-find-definitions)
    (define-key rustic-mode-map (kbd "C-c C-c c") 'lsp-ui-peek-find-references)
    ;; lsp-treemacs
    (define-key rustic-mode-map (kbd "C-c C-o r") 'lsp-treemacs-references)
    (define-key rustic-mode-map (kbd "C-c C-o i") 'lsp-treemacs-implementations)
    (define-key rustic-mode-map (kbd "C-c C-o c") 'lsp-treemacs-call-hierarchy)
    (define-key rustic-mode-map (kbd "C-c C-o t") 'lsp-treemacs-type-hierarchy)
    (define-key rustic-mode-map (kbd "C-c C-o e") 'lsp-treemacs-errors-list)
    ))

;; company 和 corfu-mode有冲突
(when (maybe-require-package 'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none))

  ;; uncomment for less flashiness
  (when *is-a-win*
    (setq lsp-eldoc-hook nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-signature-auto-activate nil)
    (setq lsp-eldoc-enable-hover nil))

  (setq lsp-headerline-breadcrumb-enable nil)

  ;; 是否在底部进行显示doc
  ;; (setq lsp-eldoc-render-all t)
  ;; (setq lsp-idle-delay 3.6)
  ;; (setq lsp-inlay-hints-enable t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (if *is-a-win*
      (progn
        (setq lsp-rust-analyzer-display-parameter-hints nil)
        (setq lsp-rust-analyzer-cargo-watch-enable nil))
    (progn
      (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
      (setq lsp-rust-analyzer-display-parameter-hints t)
      (setq lsp-rust-analyzer-display-reborrow-hints "mutable")
      (setq lsp-rust-analyzer-cargo-watch-enable t)))

  (when *is-a-win*
    (setq lsp-signature-render-documentation nil))
  (add-hook 'rustic-mode-hook 'lsp-deferred))

(when (maybe-require-package 'lsp-ui)

  ;; todo 是否为左侧的的渲染显示
  ;; lsp-ui-sideline
  (if *is-a-win*
      (progn
        (message "disable lsp-ui-sideline on windows")
        (setq lsp-ui-sideline-show-diagnostics nil))
    (progn
      (setq lsp-ui-sideline-show-hover t)
      (setq lsp-ui-sideline-show-code-actions t)
      (setq lsp-ui-sideline-delay 1)))
  ;; lsp-ui-peek
  (if *is-a-win*
      (progn
        (message "disable lsp-ui-peek-mode on windows"))
    (progn
      (setq lsp-ui-peek-show-directory t)
      (setq lsp-ui-peek-always-show t)
      ))

  ;; lsp-ui-doc
  (when (display-graphic-p)
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-max-height 20)
    (setq lsp-ui-doc-position 'at-point)
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-ui-doc-delay 1))

  ;; lsp-ui-imenu
  (when (not *is-a-win*)
    (setq lsp-ui-imenu-window-fix-width t)
    (setq lsp-ui-imenu-auto-refresh t)
    (setq lsp-ui-imenu-refresh-delay 3))

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; lsp-treemacs
(when (maybe-require-package 'lsp-treemacs))

(when (maybe-require-package 'rust-playground))

(when (maybe-require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (when (maybe-require-package 'dap-mode)
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (when ((maybe-require-package 'dap-lldb)
           (maybe-require-package 'dap-gdb-lldb))
      (dap-gdb-lldb-setup)
      (dap-register-debug-template
       "Rust::LLDB Run Configuration"
       (list :type "lldb"
             :request "launch"
             :name "LLDB:run"
             :gdbpath "rust-lldb"
             ;; uncomment if lldb is not in PATH
             ;; :lldbmipath "path/to/lldb-mi"
             )
       )
      )
    )
  )

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
  (add-hook 'rustic-mode-hook #'yas-reload-all)
  (maybe-require-package 'yasnippet-snippets))


;;; add for org
(maybe-require-package 'ob-rust)

(when (maybe-require-package 'flycheck-rust)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust.el ends here
