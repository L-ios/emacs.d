;;; init-blog.el --- My blog setup for org-mode and org2jekyll

;;; Commentary:

;;; Code:

(maybe-require-package 'org2jekyll)

(defgroup blog-pack nil "blog pack"
  :tag "blog-pack"
  :version "0.0.3"
  :group 'org)

;;;###autoload
(defun blog-pack-load-setup ()
  "Load blog pack setup"
  (interactive)
  (custom-set-variables
   '(org2jekyll-blog-author       "L-ios")
   '(org2jekyll-source-directory  (expand-file-name "~/Documents/org-jekyll/org"))
   '(org2jekyll-jekyll-directory  (expand-file-name "~/Documents/org-jekyll/jekyll"))
   '(org2jekyll-jekyll-drafts-dir "_drafts/")
   '(org2jekyll-jekyll-posts-dir  "_posts/")
   '(org2jekyll-default-template-entries-extra '(("language" "zh")
                                                 ("permailink" "/xxx")))
   '(org-html-htmlize-output-type 'inline-css)
   '(org-publish-project-alist
     `(("default"
        :base-directory ,(org2jekyll-input-directory)
        :base-extension "org"
        ;; :publishing-directory "/ssh:user@host:~/html/notebook/"
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

       ("web" :components ("images" "js" "css" "assets")))))
  (message "blog-pack setup loaded!"))

(defvar blog-pack-mode-map nil
  "Keymap for blog-pack mode.")
(setq blog-pack-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c b l") 'blog-pack-load-setup)
        map))

;;;###autoload
(define-minor-mode blog-pack-mode
  "Functionality for publishing the current org-mode post to jekyll.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{blog-pack-mode-map}"

  :init-value nil
  :lighter " bp"
  :group 'blog-pack
  :keymap blog-pack-mode-map)
(maybe-require-package 'ox-publish)
(maybe-require-package 'ox-jekyll-md)

(add-hook 'org2jekyll-mode-hook 'blog-pack-load-setup)
(add-hook 'org2jekyll-mode-hook 'blog-pack-mode)
(add-hook 'org-mode-hook 'org2jekyll-mode)

(provide 'init-blog)
;;; blog-pack.el ends here
