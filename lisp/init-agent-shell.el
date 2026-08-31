;;; init-agent-shell.el --- ACP-powered coding agents in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; agent-shell (https://github.com/xenodium/agent-shell) talks the Agent
;; Client Protocol (ACP) to coding agents.  This file wires it up to
;; `jcode acp' (https://github.com/1jehuang/jcode), which runs a jcode
;; session behind a stdio ACP adapter.
;;
;; Usage: M-x agent-shell-jcode-start-agent  (or M-x agent-shell and pick jcode)
;;
;; This file also bootstraps the agent-shell ecosystem extensions that are
;; not on MELPA.  Packages are cloned into site-lisp/ automatically when
;; missing (self-healing across Emacs upgrades and clean reinstalls) and
;; updated with M-x `agent-shell-extras-update-all'.
;;
;;   UI:        agent-shell-sidebar, agent-shell-hq
;;   Sessions:  agent-shell-manager, agent-shell-desktop
;;   Workflow:  agent-shell-queue, agent-review
;;   Org:       agent-shell-org-transcript, ob-agent-shell

;;; Code:

;; agent-shell requires Emacs 29.1+ (its deps declare (emacs "29.1")); skip
;; gracefully on older Emacsen instead of failing startup.
(when (version<= "29.1" emacs-version)
  (require-package 'agent-shell))

(with-eval-after-load 'agent-shell
  ;; Restore sessions with full history replay (jcode supports session/load
  ;; with replay; `minimal' shows only the title, which reads as empty).
  (setq agent-shell-session-restore-verbosity 'full)
  ;; jcode is the default agent everywhere: plain M-x agent-shell starts it
  ;; directly without the agent-selection prompt.  Use '(preselect . jcode)
  ;; instead to keep the picker with jcode preselected.
  (setq agent-shell-preferred-agent-config 'jcode))

(with-eval-after-load 'agent-shell
  (defcustom agent-shell-jcode-command "jcode"
    "Path to the jcode executable used by the ACP adapter."
    :type 'string
    :group 'agent-shell)

  (defcustom agent-shell-jcode-acp-args '("acp")
    "Arguments passed to `agent-shell-jcode-command' to start the ACP adapter."
    :type '(repeat string)
    :group 'agent-shell)

  (defcustom agent-shell-jcode-environment nil
    "Extra environment variables for the jcode ACP process.
Built with `agent-shell-make-environment-variables'."
    :type '(repeat string)
    :group 'agent-shell)

  (defun agent-shell-jcode-make-agent-config ()
    "Create a jcode agent configuration for agent-shell."
    (agent-shell-make-agent-config
     :identifier 'jcode
     :mode-line-name "jcode"
     :buffer-name "jcode"
     :shell-prompt "jcode> "
     :shell-prompt-regexp "jcode> "
     :client-maker (lambda (buffer)
                     (agent-shell--make-acp-client
                      :command agent-shell-jcode-command
                      :command-params agent-shell-jcode-acp-args
                      :environment-variables agent-shell-jcode-environment
                      :context-buffer buffer))
     :install-instructions
     "Install jcode (brew install jcode or see https://github.com/1jehuang/jcode)."))

  ;; Add jcode alongside the built-in agents in `M-x agent-shell'.
  (add-to-list 'agent-shell-agent-configs #'agent-shell-jcode-make-agent-config)

  ;;;###autoload
  (defun agent-shell-jcode-start-agent ()
    "Start an interactive jcode agent shell via ACP."
    (interactive)
    (agent-shell--dwim :config (agent-shell-jcode-make-agent-config)
                       :new-shell t))

  ;; Optional: make jcode the default when calling plain `M-x agent-shell'.
  ;; (setq agent-shell-preferred-agent-config 'jcode)
  )

;;; ---------------------------------------------------------------------
;;; Ecosystem extensions (auto-cloned into site-lisp)
;;; ---------------------------------------------------------------------

(defvar agent-shell-extras-repos
  '(("agent-shell-sidebar" . "https://github.com/cmacrae/agent-shell-sidebar")
    ("agent-shell-hq" . "https://github.com/SreenivasVRao/agent-shell-hq")
    ("agent-shell-manager" . "https://github.com/jethrokuan/agent-shell-manager")
    ("agent-shell-desktop" . "https://github.com/timfel/agent-shell-desktop.el")
    ("agent-shell-queue" . "https://github.com/tychoish/agent-shell-queue")
    ("agent-review" . "https://github.com/nineluj/agent-review")
    ("agent-shell-org-transcript" . "https://github.com/lllShamanlll/agent-shell-org-transcript")
    ("ob-agent-shell" . "https://github.com/eddof13/ob-agent-shell")
    ("agent-shell-math-renderer" . "https://github.com/alberti42/agent-shell-math-renderer")
    ("latex-to-svg-backend" . "https://github.com/alberti42/latex-to-svg-backend"))
  "Alist of (DIR . URL) packages kept as shallow clones under site-lisp/.")

(defun agent-shell-extras-dir (name)
  "Return the site-lisp directory for package NAME."
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun agent-shell-extras-ensure (name url)
  "Clone URL into site-lisp/NAME if missing.  Return non-nil when present."
  (let ((dir (agent-shell-extras-dir name)))
    (unless (file-directory-p dir)
      (message "agent-shell-extras: cloning %s ..." name)
      (make-directory (file-name-directory dir) t)
      (unless (zerop (call-process "git" nil (get-buffer-create "*agent-shell-extras-clone*") t
                                   "clone" "--depth" "1" url dir))
        (message "agent-shell-extras: FAILED to clone %s from %s" name url)
        (when (file-directory-p dir) (delete-directory dir t))))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(defun agent-shell-extras-ensure-all ()
  "Ensure every package in `agent-shell-extras-repos' is present."
  (dolist (entry agent-shell-extras-repos)
    (agent-shell-extras-ensure (car entry) (cdr entry))))

(defun agent-shell-extras-update-all ()
  "Run `git pull' in every managed package directory."
  (interactive)
  (dolist (entry agent-shell-extras-repos)
    (let ((dir (agent-shell-extras-dir (car entry))))
      (when (file-directory-p dir)
        (message "agent-shell-extras: updating %s ..." (car entry))
        (call-process "git" nil nil t "-C" dir "pull" "--ff-only")))))

;; Make everything present before the `locate-library' checks below.
(agent-shell-extras-ensure-all)

;; UI: sidebar — side-by-side layout.  M-x agent-shell-sidebar-toggle
(with-eval-after-load 'agent-shell
  (when (locate-library "agent-shell-sidebar")
    (require 'agent-shell-sidebar)))

;; UI: hq — floating HUD for multi-session management.  M-x agent-shell-hq
;; Requires posframe and persp-mode (auto-installed).
(when (locate-library "agent-shell-hq")
  (maybe-require-package 'posframe)
  (maybe-require-package 'persp-mode)
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-hq)))

;; Sessions: manager — tabulated buffer list.  M-x agent-shell-manager-toggle
(when (locate-library "agent-shell-manager")
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-manager)))

;; Sessions: desktop — restore shells across Emacs restarts.
(when (locate-library "agent-shell-desktop")
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-desktop)
    (agent-shell-desktop-mode 1)))

;; Workflow: queue — persistent prompt queue + transient menus.
;; Requires annotated-completing-read (auto-installed).
(when (locate-library "agent-shell-queue")
  (maybe-require-package 'annotated-completing-read)
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-queue)
    (when (locate-library "agent-shell-menu")
      (require 'agent-shell-menu))))

;; Workflow: agent-review — review agent diffs.  M-x agent-review-start
(when (locate-library "agent-review")
  (require 'agent-review))

;; Org: transcripts — auto-save conversations as org files.
(when (locate-library "agent-shell-org-transcript")
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-org-transcript)
    (setq agent-shell-org-transcript-directory
          (expand-file-name "note/agent-transcripts/" user-emacs-directory))))

;; Org: babel backend — run agent prompts from org source blocks.
;;   #+begin_src agent-shell
;;   What is the capital of France?
;;   #+end_src
(when (locate-library "ob-agent-shell")
  (with-eval-after-load 'org
    (require 'ob-agent-shell)
    (add-to-list 'org-babel-load-languages '(agent-shell . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))

;; ---------------------------------------------------------------------------
;; Inline rendering: LaTeX math + Mermaid diagrams in agent-shell buffers
;; ---------------------------------------------------------------------------
;; Both hook into `agent-shell-markdown-render-functions': math via
;; agent-shell-math-renderer (needs latex-to-svg via org-latex-preview
;; machinery), mermaid via the local renderer below (needs mmdc on PATH).

;; LaTeX: renders $$...$$ / \(...\) as SVG images.
;; Depends on latex-to-svg-backend (cloned above) and a TeX toolchain
;; (latex + dvisvgm); shows placeholder panels when the toolchain is absent.
;; The renderer is a buffer-local minor mode: it must be enabled per
;; agent-shell buffer via `agent-shell-mode-hook'.
(when (locate-library "agent-shell-math-renderer")
  (with-eval-after-load 'agent-shell
    (require 'agent-shell-math-renderer)
    (add-hook 'agent-shell-mode-hook #'agent-shell-math-renderer-mode)))

;; Mermaid: render ```mermaid blocks to PNG via mmdc, then display inline.
;; Async: incomplete blocks are skipped and retried as streaming continues.
(defun my/agent-shell-mermaid--replace (start end png body)
  "Overlay PNG on region START..END in current agent-shell buffer."
  (let ((inhibit-read-only t)
        (img (create-image png 'png nil
                           :max-width (floor (* 0.9 (window-pixel-width))))))
    (put-text-property start end 'display img)
    (put-text-property start end 'agent-shell-markdown-frozen t)
    (put-text-property start end 'agent-shell-markdown-source
                       (concat "```mermaid\n" body "\n```"))))

(defun my/agent-shell-mermaid-render (context)
  "Render complete mermaid source blocks in CONTEXT via mmdc.
Intended for `agent-shell-markdown-render-functions'.
`agent-shell-markdown--source-blocks' returns alists, so access with
`alist-get' (plist-get silently returns nil on them)."
  (when-let* ((mmdc (executable-find "mmdc"))
              (blocks (cdr (assq :source-blocks context))))
    (dolist (desc blocks)
      (when (and (equal (cdr (assq :language desc)) "mermaid")
                 (cdr (assq :complete desc)))
        (let* ((block (cdr (assq :block desc)))
               (start (marker-position (cdr (assq :start block))))
               (end (marker-position (cdr (assq :end block))))
               (body (cdr (assq :body desc)))
               (buf (current-buffer)))
          (when (and (number-or-marker-p start)
                     (number-or-marker-p end)
                     (> end start)
                     (not (text-property-search-forward
                           start end 'agent-shell-markdown-frozen t)))
            (let* ((cache-dir (make-temp-file "agent-shell-mermaid-" t))
                   (png (expand-file-name "diagram.png" cache-dir))
                   (source (expand-file-name "diagram.mmd" cache-dir)))
              (with-temp-file source (insert body))
              ;; Replace asynchronously so streaming never blocks on mmdc.
              (make-process
               :name "agent-shell-mermaid"
               :connection-type 'pipe
               :command (list mmdc "-i" source "-o" png "-b" "transparent")
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (if (and (zerop (process-exit-status proc))
                            (file-exists-p png))
                       (run-with-timer
                        0 nil
                        (lambda ()
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (my/agent-shell-mermaid--replace start end png body)))))
                     (message "agent-shell mermaid render failed (exit %s)"
                              (process-exit-status proc))))))))))))
  nil)

(with-eval-after-load 'agent-shell-markdown
  (add-hook 'agent-shell-markdown-render-functions #'my/agent-shell-mermaid-render))

;; jcode resume helper: jcode's ACP adapter does not implement session/list,
;; so agent-shell's session picker never gets populated.  Offer local
;; completion over ~/.jcode/sessions/ files instead, feeding the chosen id
;; straight to `agent-shell--start' with the jcode config (no agent-selection
;; prompt).
(defun agent-shell-jcode-picker-session ()
  "Pick a jcode session from ~/.jcode/sessions and resume it via agent-shell.
Candidates are annotated with title, message count, and recency; empty
sessions (no user/assistant messages) are hidden.  Resumes directly with
the jcode agent config, skipping agent selection."
  (interactive)
  (let* ((dir (expand-file-name "~/.jcode/sessions"))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\`session_.*\\.json\\'")))
         (candidates nil))
    (dolist (f (seq-sort (lambda (a b) (string> a b)) files))
      (let* ((path (expand-file-name f dir))
             (meta (condition-case nil
                       (let* ((json-object-type 'alist)
                              (data (json-read-file path))
                              (msgs (cdr (assq 'messages data)))
                              (real 0) (title nil) (mtime (file-attribute-modification-time
                                                          (file-attributes path))))
                         (dolist (m (if (vectorp msgs) (append msgs nil) msgs))
                           (when (member (cdr (assq 'role m)) '("user" "assistant"))
                             (cl-incf real)))
                         (cons real (cons (or (cdr (assq 'title data)) "") mtime)))
                     (error nil))))
        (when (and meta (> (car meta) 0))
          (let* ((real (car meta))
                 (title (cadr meta))
                 (mtime (cddr meta))
                 (time-str (if mtime (format-time-string "%m-%d %H:%M" mtime) ""))
                 (label (format "%s  %s  [%d msgs]"
                                time-str
                                (if (string-empty-p title)
                                    (file-name-sans-extension f)
                                  title)
                                real)))
            (push (cons label (file-name-sans-extension f)) candidates)))))
    (if (null candidates)
        (message "No jcode sessions (with content) found in %s" dir)
      (let* ((choice (completing-read "Resume jcode session: "
                                      (mapcar #'car candidates) nil t))
             (session-id (cdr (assoc choice candidates))))
        (when (and session-id (not (string-empty-p session-id)))
          (agent-shell--start :config (agent-shell-jcode-make-agent-config)
                              :session-id session-id
                              :new-session t))))))

(provide 'init-agent-shell)
;;; init-agent-shell.el ends here
