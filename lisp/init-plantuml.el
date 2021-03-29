;;; init-plantuml.el --- Plantuml support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-exec-mode "jar")
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-indent-level 4)
  (setq plantuml-jar-args "-charset UTF-8")
  (maybe-require-package 'flycheck-plantuml)
  (add-auto-mode 'plantuml-mode "\\.puml\\.plantuml\\")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'plantuml-mode)))
;; (zeal-at-point xterm-color xcscope websocket web-server systemd swiper ssh-config-mode sqlite3 smex smarty-mode shelldoc rustic rust-playground rust-auto-use rime posframe popup plantuml-mode php-mode pandoc-mode pandoc package+ ox-qmd ox-jekyll-md ox-html5slide ox-bibtex-chinese ox-asciidoc orgalist org2jekyll org-tree-slide org-translate org-preview-html org-link-beautify org-index org-edit-latex org-download org ob-sql-mode ob-rust ob-kotlin ob-http ob-go nixpkgs-fmt nixos-options nix-sandbox nix-mode nix-buffer monokai-pro-theme markdown-preview-mode lv ivy-xref ivy-rich ivy hydra flycheck-plantuml expand-region edit-indirect dash-functional darkokai-theme counsel company-php company-nixos-options compact-docstrings auctex all-the-icons ac-php-core

;;; init-plantuml.el ends here
