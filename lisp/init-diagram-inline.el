;;; init-diagram-inline.el --- Inline diagram rendering (mermaid/plantuml) in markdown -*- lexical-binding: t -*-

;;; Commentary:
;;; Render ```mermaid / ```plantuml fenced blocks to images displayed
;;; inline in the markdown buffer, similar to org-mode's inline images.
;;;
;;; Usage:
;;;   M-x my/diagram-inline-render      (also C-c C-c i in markdown)
;;;     Render the block at point and display the image inline,
;;;     overlaying the source text.  C-c C-c i again or
;;;     M-x my/diagram-inline-toggle to hide/show the image.
;;;   M-x my/diagram-inline-render-all  (also C-c C-c I)
;;;     Render and display all diagram blocks in the buffer.
;;;   M-x my/diagram-inline-clear
;;;     Remove all inline images in the buffer.

;;; Code:

(require 'cl-lib)

(defvar my/diagram-inline-renderers
  '(("mermaid" . my/diagram-inline--render-mermaid)
    ("plantuml" . my/diagram-inline--render-plantuml))
  "Alist of language -> function to render source to an image file.
Each renderer receives (SOURCE OUTPUT-FILE) and returns non-nil on success.")

(defvar my/diagram-inline--overlays (make-hash-table :test 'equal)
  "Buffer-local-ish map: buffer -> list of image overlays.")

(defun my/diagram-inline--mmdc ()
  (or (executable-find "mmdc") "mmdc"))

(defun my/diagram-inline--plantuml-jar ()
  (expand-file-name "~/.emacs.d/plantuml.jar"))

(defun my/diagram-inline--render-mermaid (source output-file)
  "Render SOURCE with mmdc to OUTPUT-FILE (png)."
  (let ((mmd-file (make-temp-file "diagram-mermaid-" nil ".mmd")))
    (with-temp-file mmd-file (insert source))
    (zerop (call-process (my/diagram-inline--mmdc) nil "*diagram-mmdc*" nil
                         "-i" mmd-file "-o" output-file
                         "-b" "transparent"))))

(defun my/diagram-inline--render-plantuml (source output-file)
  "Render SOURCE with plantuml.jar to OUTPUT-FILE (png)."
  (let* ((puml-file (make-temp-file "diagram-plantuml-" nil ".puml"))
         (out-dir (make-temp-file "diagram-plantuml-out-" t)))
    (with-temp-file puml-file (insert source))
    (let ((exit (call-process "java" nil "*diagram-plantuml*" nil
                              "-jar" (my/diagram-inline--plantuml-jar)
                              "-charset" "UTF-8" "-tpng"
                              "-o" out-dir
                              puml-file)))
      (if (zerop exit)
          ;; plantuml writes <basename>.png into out-dir
          (let ((png (expand-file-name
                      (concat (file-name-base puml-file) ".png") out-dir)))
            (when (file-exists-p png)
              (rename-file png output-file t)
              t))
        nil))))

(defun my/diagram-inline--proc-buffer ()
  (get-buffer-create "*diagram-render*"))

(defun my/diagram-inline--block-at-point ()
  "Return (LANG . BEG-END) of fenced block at point, or nil."
  (save-excursion
    (let ((case-fold-search nil))
      (cond
       ((and (looking-at "```\\(mermaid\\|plantuml\\)\\(?:[ \t]*.*\\)?$")
             (match-beginning 0))
        (my/diagram-inline--block-end-from (match-beginning 0)))
       ((save-excursion
          (beginning-of-line)
          (when (looking-at "```\\(mermaid\\|plantuml\\)\\(?:[ \t]*.*\\)?$")
            (my/diagram-inline--block-end-from (match-beginning 0)))))
       (t
        ;; Inside the block: search back for the opening fence.
        (let ((start (save-excursion
                       (and (re-search-backward "^```\\(mermaid\\|plantuml\\)\\(?:[ \t]*.*\\)?$" nil t)
                            (match-beginning 0)))))
          (when start
            (my/diagram-inline--block-end-from start))))))))

(defun my/diagram-inline--block-end-from (start)
  "From opening fence at START, return (LANG . (BEG . END)) with END at closing fence."
  (save-excursion
    (goto-char start)
    (let ((lang (match-string 1)))
      (if (re-search-forward "^```[ \t]*$" nil t)
          (cons lang (cons start (match-end 0)))
        nil))))

(defun my/diagram-inline--source-of (beg end)
  "Source text of block BEG..END, without fences."
  (buffer-substring-no-properties
   (save-excursion (goto-char beg) (line-beginning-position 2))
   ;; END is at/after the closing fence's newline; take that line's beginning.
   (save-excursion (goto-char end) (line-beginning-position))))

(defun my/diagram-inline--display (beg end image-file)
  "Overlay IMAGE-FILE on BEG..END."
  (my/diagram-inline--remove-overlays beg end)
  (let* ((ov (make-overlay beg end))
         (img (create-image image-file)))
    (overlay-put ov 'display img)
    (overlay-put ov 'my/diagram-inline t)
    (overlay-put ov 'evaporate t)
    ;; keep map global: buffer name -> overlays
    (puthash (current-buffer) (cons ov (gethash (current-buffer) my/diagram-inline--overlays))
             my/diagram-inline--overlays)))

(defun my/diagram-inline--remove-overlays (&optional beg end)
  "Remove my overlays in region BEG..END, or all in buffer."
  (let ((ovs (gethash (current-buffer) my/diagram-inline--overlays)))
    (dolist (ov ovs)
      (when (and (overlay-buffer ov)
                 (or (and (not beg) (not end))
                     (and beg end (<= beg (overlay-start ov)) (>= end (overlay-end ov)))))
        (delete-overlay ov)))
    (puthash (current-buffer)
             (cl-remove-if (lambda (ov) (not (overlay-buffer ov))) ovs)
             my/diagram-inline--overlays)))

(defun my/diagram-inline--output-file (lang)
  "Cache dir for rendered images."
  (let ((dir (expand-file-name (concat "diagrams-" (md5 (or (buffer-file-name) (buffer-name))))
                              temporary-file-directory)))
    (make-directory dir t)
    (expand-file-name (concat (symbol-name (intern lang)) "-"
                              (format-time-string "%Y%m%d-%H%M%S") ".png")
                      dir)))

(defun my/diagram-inline--render-block (lang beg end)
  "Render block LANG BEG END, then display inline."
  (let* ((source (my/diagram-inline--source-of beg end))
         (out (my/diagram-inline--output-file lang))
         (renderer (cdr (assoc lang my/diagram-inline-renderers)))
         (proc-buf (my/diagram-inline--proc-buffer)))
    (when renderer
      (let ((ok (funcall renderer source out)))
        (if ok
            (progn
              (my/diagram-inline--display beg end out)
              (message "diagram[%s]: rendered inline (%s)" lang out))
          (progn
            (pop-to-buffer proc-buf)
            (error "diagram[%s]: render failed, see %s" lang proc-buf)))))))

(defun my/diagram-inline-render ()
  "Render the diagram block at point and display it inline."
  (interactive)
  (let ((block (my/diagram-inline--block-at-point)))
    (if (not block)
        (message "Not inside a ```mermaid or ```plantuml block")
      (my/diagram-inline--render-block (car block) (cadr block) (cddr block)))))

(defun my/diagram-inline-toggle ()
  "Toggle inline image display for the diagram block at point."
  (interactive)
  (let* ((block (my/diagram-inline--block-at-point))
         (ov (and block
                  (cl-find-if (lambda (o)
                                (and (overlay-buffer o)
                                     (<= (overlay-start o) (point))
                                     (>= (overlay-end o) (point))))
                              (gethash (current-buffer) my/diagram-inline--overlays)))))
    (cond
     ((and block ov)
      ;; hide: delete overlay
      (my/diagram-inline--remove-overlays)
      (message "diagram: hidden"))
     ((not block)
      (message "Not inside a ```mermaid or ```plantuml block"))
     (t
      (my/diagram-inline--render)))))

(defun my/diagram-inline-render-all ()
  "Render and display all diagram blocks in the buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^```\\(mermaid\\|plantuml\\)\\(?:[ \t]*.*\\)?$" nil t)
        (let ((start (match-beginning 0))
              (lang (match-string 1)))
          (when (re-search-forward "^```[ \t]*$" nil t)
            (my/diagram-inline--render-block lang start (match-end 0))
            (cl-incf count)))))
    (message "diagram: rendered %d block(s)" count)))

(defun my/diagram-inline-clear ()
  "Remove all inline diagram images in this buffer."
  (interactive)
  (my/diagram-inline--remove-overlays)
  (message "diagram: cleared"))

;; ---- minor mode keymap for markdown -------------------------------------

(defvar my/diagram-inline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c i") #'my/diagram-inline-render)
    (define-key map (kbd "C-c C-c I") #'my/diagram-inline-render-all)
    (define-key map (kbd "C-c C-c x") #'my/diagram-inline-clear)
    map))

(define-minor-mode my/diagram-inline-mode
  "Minor mode to render mermaid/plantuml blocks inline in markdown."
  :lighter " Diagram"
  :keymap my/diagram-inline-mode-map
  (unless my/diagram-inline-mode
    (my/diagram-inline-clear)))

;; Hook into markdown-mode
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'my/diagram-inline-mode)
  (add-hook 'gfm-mode-hook #'my/diagram-inline-mode))

(provide 'init-diagram-inline)
;;; init-diagram-inline.el ends here
