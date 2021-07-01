;;; init-local.el --- Defaults for local -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun disable-themes-on-terminal ()
  "Disable theme on terminal with -nw."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified")))

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
