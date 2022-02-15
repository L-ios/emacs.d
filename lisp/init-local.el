;;; init-local.el --- Defaults for local -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun disable-themes-on-terminal ()
  "Disable theme on terminal with -nw."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified")))

(setq org2jekyll-source-directory  (expand-file-name "d:/jekyll-blog/Note/org"))
(setq org2jekyll-jekyll-directory  (expand-file-name "d:/jekyll-blog"))

(defun set-font (english chinese english-size chinese-size)
  "set chinese, english font and size"
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" english english-size)
                      :height (* english-size 10))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size)))
  (set-frame-font (format "%s-%s" english english-size t t)))

(defun fix-font ()
  "set font"
  (when (display-graphic-p)
    (when *is-a-win*
      (set-font "BlexMono Nerd Font Mono" "Microsoft YaHei UI" 12 24))))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (fix-font)))

(fix-font)

(setq desktop-save-mode nil)

(setq auto-save-default t)

(set 'process-connection-type nil)
;; '(org-table ((t (:foreground "#c397d8" :height 1.05 :width normal :family "UbuntuMono Nerd Font"))))

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
