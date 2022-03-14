;;; init-local.el --- Defaults for local -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar emacs-english-font "BlexMono Nerd Font Mono"
  "The font name of English.")

(defvar emacs-cjk-font "思源黑体 CN Light"
  "The font name for CJK.")

(defvar emacs-font-size-pair '(20 . 24)
  "Default font size pair for (english . chinese)")

(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."

  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

;; Setup font size based on emacs-font-size-pair
;;(set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
;;(set-frame-font (format "%s:pixelsize=%d" "等距更纱黑体 Slab SC" 12) t)

(defun fix-font ()
  "set font"
  (when (display-graphic-p)
    (when *is-a-win*
      (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)
      (set-font "BlexMono Nerd Font Mono" "思源黑体 CN Light" 20 24))
    (when (eq system-type 'gnu/linux)
      (setq fonts '("Source Code Pro" "思源黑体"))
      (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
      (set-font "BlexMono Nerd Font Mono" "思源黑体" 16 22))))

(setq auto-save-default t)

(set 'process-connection-type nil)

(defconst *use-emacs-rime* nil)
(unless (or (display-graphic-p) *use-emacs-rime*)
  (require 'rime)
  (setq rime-user-data-dir "~/.config/ibus/rime")
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "WenQuanYi Micro Hei Mono-14"
              :internal-border-width 10))
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-style 'vertical))

(provide 'init-local)

;;; init-local.el ends here
