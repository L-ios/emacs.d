;;; init-blog.el --- My blog setup for org-mode and org2jekyll

;;; Commentary:

;;; Code:

(defun blogroom-setup ()
  "Load Blog Room setup"
  ;;   (interactive)
  (setq org2jekyll-blog-author "L-ios")
  (setq org2jekyll-source-directory (expand-file-name "~/Documents/org-jekyll/org/"))
  (setq org2jekyll-jekyll-directory (expand-file-name "~/Documents/L-ios.github.io/"))
;;  (setq org2jekyll-jekyll-drafts-dir "")
  (setq org2jekyll-jekyll-posts-dir "_posts/")
  (setq org2jekyll-default-template-entries-extra '(("language" "zh")
                                                    ("permailink" "https://l-ios.github.io")))
  (setq org-html-htmlize-output-type 'inline-css)
  (setq org-publish-project-alist
        `(("default"
           :base-directory ,(org2jekyll-input-directory)
           :base-extension "org"
           :publishing-directory ,(org2jekyll-output-directory)
           :publishing-function org-html-publish-to-html
           :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
           :auto-preamble t
           :recursive t
           :html-extension "html"
           :body-only t)
          ("post"
           :base-directory ,(org2jekyll-input-directory)
           :base-extension "org"
           :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
           :publishing-function org-html-publish-to-html
           :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
           :auto-preamble t
           :recursive t
           :html-extension "html"
           :body-only t)

          ("images"
           :base-directory ,(org2jekyll-input-directory "img")
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory ,(org2jekyll-output-directory "img")
           :publishing-function org-publish-attachment
           :recursive t)

          ("js"
           :base-directory ,(org2jekyll-input-directory "js")
           :base-extension "js"
           :publishing-directory ,(org2jekyll-output-directory "js")
           :publishing-function org-publish-attachment
           :recursive t)

          ("css"
           :base-directory ,(org2jekyll-input-directory "css")
           :base-extension "css\\|el"
           :publishing-directory ,(org2jekyll-output-directory "css")
           :publishing-function org-publish-attachment
           :recursive t)

          ("assets"
           :base-directory ,(org2jekyll-input-directory "assets")
           :base-extension "asc\\|txt"
           :publishing-directory ,(org2jekyll-output-directory "assets")
           :publishing-function org-publish-attachment
           :recursive t)

          ("web" :components ("images" "js" "css" "assets"))))
  (message "blog setup loaded...."))


(when (maybe-require-package 'org2jekyll)
  (add-hook 'org2jekyll-mode (blogroom-setup))
  (add-hook 'org-mode-hook 'org2jekyll-mode))

(provide 'init-blog)
;;; blog-pack.el ends here
