;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; more usage see https://github.com/defunkt/markdown-mode#Usage

(setq markdown-command "multimarkdown")

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'gfm-mode "README\\.MD\\'")
  (add-auto-mode 'gfm-mode "README\\.md\\'")
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  ;; Fontify fenced code blocks with their native major modes.
  (setq markdown-fontify-code-blocks-natively t)
  (with-eval-after-load 'markdown-mode
    ;; ```mermaid blocks get mermaid-mode highlighting via the name
    ;; fallback (mermaid-mode is fboundp), so no mapping entry needed.
    (when (fboundp 'mermaid-mode)
      (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))))
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))


;;; Mermaid support ------------------------------------------------------

(defvar my/mmdc-executable
  (or (executable-find "mmdc") "mmdc")
  "Path to the mermaid CLI used for rendering.")

(defun my/mermaid-render-region-to-png (beg end)
  "Render mermaid source between BEG and END to a PNG and display it.
Uses mmdc; the image is written next to the current file (or in
`temporary-file-directory' for non-file buffers)."
  (interactive "r")
  (let* ((src (buffer-substring-no-properties beg end))
         (base (if (buffer-file-name)
                   (file-name-sans-extension (buffer-file-name))
                 (expand-file-name
                  (file-name-nondirectory
                   (or (buffer-name) "mermaid"))
                  temporary-file-directory)))
         (mmd-file (make-temp-file "mermaid-" nil ".mmd"))
         (png-file (concat base "-" (format-time-string "%H%M%S") ".png")))
    (with-temp-file mmd-file
      (insert src))
    (let ((exit-code
           (call-process my/mmdc-executable nil "*mmdc*" nil
                         "-i" mmd-file "-o" png-file
                         "-b" "transparent")))
      (if (zerop exit-code)
          (progn
            (message "mermaid: rendered %s" png-file)
            (find-file-other-window png-file))
        (pop-to-buffer "*mmdc*")))))

(defun my/markdown-mermaid-at-point ()
  "Return (BEG . END) of the fenced ```mermaid block at point, or nil."
  (save-excursion
    (let ((case-fold-search nil))
      (when (or (looking-at "```mermaid")
                (re-search-backward "^```mermaid[ \t]*$" nil t))
        (let ((start (match-beginning 0)))
          (when (re-search-forward "^```[ \t]*$" nil t)
            ;; Include the fences so markdown's native fontify keeps working.
            (cons start (match-end 0))))))))

(defun my/markdown-mermaid-render ()
  "Render the ```mermaid fenced block at point via mmdc and show the PNG."
  (interactive)
  (let ((block (my/markdown-mermaid-at-point)))
    (if (not block)
        (message "Not inside a ```mermaid block")
      ;; Extract just the diagram source, skipping the fences.
      (my/mermaid-render-region-to-png
       (save-excursion (goto-char (car block)) (forward-line 1) (point))
       (save-excursion (goto-char (cdr block)) (forward-line -1) (point))))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-command-map (kbd "C-c C-c m") #'my/markdown-mermaid-render)
  ;; mermaid-mode-map exists after the package loads; its C-c C-c compiles .mmd files.
  (maybe-require-package 'mermaid-mode))


;;; Preview with mermaid.js rendering ------------------------------------

(when (maybe-require-package 'markdown-preview-mode)
  (setq markdown-open-command 'markdown-preview-mode)
  (with-eval-after-load 'markdown-preview-mode
    (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
    (add-to-list 'markdown-preview-stylesheets "http://thomasf.github.io/solarized-css/solarized-light.min.css")
    ;; override theme completely
    ;; (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
    (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
    ;; comment up and uncomment blow for async
    ;; (add-to-list 'markdown-preview-javascript '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))
    ;; Mermaid live rendering in the browser preview.
    (add-to-list 'markdown-preview-javascript "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js")
    (setq markdown-preview-script-oninit
          "window.mermaid.initialize({ startOnLoad: false, securityLevel: 'loose' });")
    (setq markdown-preview-script-onupdate
          "if (window.mermaid) { window.mermaid.run({ querySelector: '.language-mermaid, .lang-mermaid, pre code mermaid, pre.mermaid, div.mermaid' }); }")))


(provide 'init-markdown)
;;; init-markdown.el ends here
