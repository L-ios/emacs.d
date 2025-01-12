;;; init-plantuml.el --- Plantuml support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-exec-mode "jar")
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-indent-level 2)
  (setq plantuml-jar-args "-charset UTF-8")
  (maybe-require-package 'flycheck-plantuml)
  (progn
    (add-to-list 'auto-mode-alist '("\\.\\(puml\\|plantuml\\)\\'" . plantuml-mode)))
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'plantuml-mode)))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
