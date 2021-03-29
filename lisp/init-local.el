;;; init-local.el --- Defaults for local -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun disable-themes-on-terminal ()
  "Disable theme on terminal with -nw."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified")))

(set 'process-connection-type nil)
;; '(org-table ((t (:foreground "#c397d8" :height 1.05 :width normal :family "UbuntuMono Nerd Font"))))

(defun capture-report-data-file (path)
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s/%s.org"
                              (format-time-string "%Y-%m")
                              name) path)))

(add-to-list 'org-capture-templates
             '("j" "Journal" entry (file (capture-report-data-file "~/Documents/org-jekyll/journal"))
               "* %U - %^{heading}\n  %?"))

(add-to-list 'org-capture-templates
             '("p" "Problem" entry (file (capture-report-data-file "~/Documents/org-jekyll/problem"))
               "* %U - %^{heading}\n  %?"))

(add-to-list 'org-capture-templates
             `("b" "Blog" plain (file ,(concat "~/Dropbox/org/blog/"
                                               (format-time-string "%Y-%m-%d.org")))
               ,(concat "#+startup: showall\n"
                        "#+options: toc:nil\n"
                        "#+begin_export html\n"
                        "---\n"
                        "layout     : post\n"
                        "title      : %^{标题}\n"
                        "categories : %^{类别}\n"
                        "tags       : %^{标签}\n"
                        "---\n"
                        "#+end_export\n"
                        "#+TOC: headlines 2\n")))
;;(require 'rime)
;;(setq rime-user-data-dir "~/.config/ibus/rime")
;;(setq rime-posframe-properties
;;      (list :background-color "#333333"
;;            :foreground-color "#dcdccc"
;;            :font "WenQuanYi Micro Hei Mono-14"
;;            :internal-border-width 10))
;;(setq rime-show-candidate 'posframe)
;;(setq rime-posframe-style 'vertical)

(add-hook 'after-init-hook 'disable-themes-on-terminal)

(provide 'init-local)

;;; init-local.el ends here
