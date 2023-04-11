;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Stop C-z from minimizing windows under OS X

(when *is-a-win*
  (setq inhibit-compacting-font-caches t))

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)


;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; Window size and features

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(unless *is-a-mac*
  (menu-bar-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(defun disable-themes-on-terminal ()
  "Disable theme on terminal with -nw."
  (unless (display-graphic-p)
    (lambda () (interactive) (sanityinc/adjust-opacity nil -100))))

(add-hook 'after-init-hook 'disable-themes-on-terminal)

(when *is-a-mac*
  (when (maybe-require-package 'ns-auto-titlebar)
    (ns-auto-titlebar-mode)))

(when *is-a-mac*
  (add-to-list 'image-types 'svg))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily

(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)


(require-package 'disable-mouse)

(when (display-graphic-p)
  (when *is-a-win*
    (dolist (charset '(kana han cjk-misc bopomofo unicode))
      (set-fontset-font t charset "Sarasa Fixed Slab SC"))
    (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono")
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend))
  (when (eq system-type 'gnu/linux)))


(when (display-graphic-p)
  (let ((charlist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                    (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                    (36 . ".\\(?:>\\)")
                    (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                    (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                    (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                    (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                    (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                    (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                    (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                    (48 . ".\\(?:x[a-zA-Z]\\)")
                    (58 . ".\\(?:::\\|[:=]\\)")
                    (59 . ".\\(?:;;\\|;\\)")
                    (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                    (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                    (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                    (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                    (91 . ".\\(?:]\\)")
                    (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                    (94 . ".\\(?:=\\)")
                    (119 . ".\\(?:ww\\)")
                    (123 . ".\\(?:-\\)")
                    (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                    (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                    )
                  ))
    (dolist (char-regexp charlist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

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
;;(set-frame-font (format "%s:pixelsize=%d" "等距更纱黑体 Slab SC" 12) to)



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

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
