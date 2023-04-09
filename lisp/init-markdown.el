;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; more usage see https://github.com/defunkt/markdown-mode#Usage

(setq markdown-command "multimarkdown")

(when (maybe-require-package 'gfm-mode)
  (add-auto-mode 'gfm-mode "README\\.md\\'")
  (add-auto-mode 'gfm-mode "README\\.MD\\'"))

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(when (maybe-require-package 'markdown-preview-mode)
  (setq markdown-open-command 'markdown-preview-mode)
  (with-eval-after-load 'markdown-preview-mode
    (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
    ;; override theme completely
    ;; (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
    (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
    ;; comment up and uncomment blow for async
    ;; (add-to-list 'markdown-preview-javascript '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))
    ))


(provide 'init-markdown)
;;; init-markdown.el ends here
