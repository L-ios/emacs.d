;;; init-plantuml.el --- Plantuml support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-exec-mode "jar")
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-indent-level 2)
  (setq plantuml-jar-args "-charset UTF-8")
  (maybe-require-package 'flycheck-plantuml)
  (add-auto-mode 'plantuml-mode "\\.puml\\.plantuml\\")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'plantuml-mode)))


;;; init-plantuml.el ends here
