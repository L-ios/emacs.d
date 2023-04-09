;;; init-org-roam.el --- Org-Roam config -*- lexical-binding: t -*-
;;; Code:

;; https://www.orgroam.com/manual.html
;; author config https://config.daviwil.com/emacs
;; org roam 也有capture模式

(when (maybe-require-package 'org-roam)

  (setq org-roam-directory (file-truename "~/.emacs.d/org-roam"))
  (with-eval-after-load 'org-roam
    (define-key org-roam-mode-map (kbd "C-c n l") org-roam-buffer-toggle)
    (define-key org-roam-mode-map (kbd "C-c n f") org-roam-node-find)
    (define-key org-roam-mode-map (kbd "C-c n g") org-roam-graph)
    (define-key org-roam-mode-map (kbd "C-c n i") org-roam-node-insert)
    (define-key org-roam-mode-map (kbd "C-c n c") org-roam-capture)
    (define-key org-roam-mode-map (kbd "C-c n j") org-roam-dailies-capture-today)
    )
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  (when (maybe-require-package 'org-roam-server)
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 9090
          org-roam-server-export-inline-images t
          org-roam-server-authenticate nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)
    (with-eval-after-load 'org-roam
      (org-roam-server-mode))
    ))

;; org-roam-capture https://www.zmonster.me/2020/06/27/org-roam-introduction.html
;; https://jethrokuan.github.io/org-roam-guide/


(provide 'init-org-roam)
;;; init-org-roam.el ends here
