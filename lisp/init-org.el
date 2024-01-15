;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)

(maybe-require-package 'org-preview-html)
(maybe-require-package 'org-link-beautify)
(maybe-require-package 'ox-reveal)

;; Various preferences
(setq org-log-done t
      org-startup-with-inline-images t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-startup-truncated nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 100)


(when (maybe-require-package 'org-journal)
  ;; org-journal doc: https://github.com/bastibe/org-journal
  (setq org-journal-dir "~/.emacs.d/journal/")
  (setq org-journal-date-format "%A, %d %B %Y"))

;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-default-notes-file "~/.emacs.d/note/index.org")
;; (setq org-capture-templates
;;       `(("t" "TODO" entry (file+datetree "~/Documents/org/tasks.org"  "Tasks")
;;          "* TODO [#C] %?\n   SCHEDULED: <%<%Y-%m-%d %a>>\n  [%<%Y-%m-%d %a>]\n  %a")
;;         ("T" "Travel" entry (file+datetree+prompt "~/Documents/org/travel.org")
;;          "* %?\n  :PROPERTIES:\n  :LOCATION:\n  :END:\n  %t\n  %a")
;;         ("j" "Journal Note" entry (file+olp+datetree
;;          "* Event: %?\n %i\n  From: %a")
;;         )
;;       )

;; 参考 https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el
(setq org-capture-templates `())
(setq org-capture-templates-contexts `())
;;; default notes file is org-default-notes-file  ; "" => `org-default-notes-file'
;;; https://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
;;; https://orgmode.org/manual/Agenda-Files.html
;;; https://emacsdocs.org/docs/org/Capture-templates
;;; https://orgmode.org/guide/Capture.html#Capture-templates
;;; https://www.gnu.org/software/emacs/manual/html_mono/org.html#Capture
(add-to-list 'org-capture-templates '("t" "todo" entry (file "")
                                      "* NEXT %?\n%U\n" :clock-resume t))
(add-to-list 'org-capture-templates '("n" "note" entry (file "")
                                      "* %? :NOTE:\n%U\n%a\n" :clock-resume t))
(add-to-list 'org-capture-templates `("j" "Journal Note" entry (file+olp+datetree
                                                                ,(concat org-journal-dir (format-time-string "journal-%m-%d-%M-%S.org")))
                                      "* Event: %?\n %i\n  From: %a"))
(setq org-capture-templates-example
      '(

        ;; Templates for the TASKS keyword sequence
        ("t" "Tasks")

        ;; TODO     (t) Todo template
        ("tt" "TODO      (t) Todo" entry (file "ref.org")
         "* TODO %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; WAITING  (w) Waiting template
        ("tw" "WAITING   (w) Waiting" entry (file "ref.org")
         "* WAITING %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"WAITING\"    from \"\"           %U
  :END:" :empty-lines 1)

        ;; DELEGATED(e) Delegated template
        ("te" "DELEGATED (e) Delegated" entry (file "ref.org")
         "* DELEGATED %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DELEGATED\"  from \"\"           %U
  :END:" :empty-lines 1)

        ;; CANCELLED(x) Cancelled template
        ("tx" "CANCELLED (x) Cancelled" entry (file "ref.org")
         "* CANCELLED %
  CLOSED: %U
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; DONE     (d) Done template
        ("td" "DONE      (d) Done" entry (file "ref.org")
         "* DONE %?
  CLOSED: %U
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DONE\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; Templates for the POSSESSIONS keyword sequence
        ("p" "Possessions")

        ;; PURCHASE (p) Purchase template
        ("pp" "PURCHASE  (p) Purchase" entry (file "ref.org")
         "* PURCHASE %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"PURCHASE\"   from \"\"           %U
  :END:")

        ;; PURCHASED(j) Purchased template
        ("pj" "PURCHASED (j) Purchased" entry (file "ref.org")
         "* PURCHASED %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"PURCHASED\"  from \"\"           %U
  :END:" :empty-lines 1)

        ;; TRANSIT  (u) Transit template
        ("pu" "TRANSIT   (u) Transit" entry (file "ref.org")
         "* TRANSIT %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TRANSIT\"    from \"\"           %U
  :END:" :empty-lines 1)

        ;; GIFT     (h) Gift    template
        ("ph" "GIFT      (h) Gift" entry (file "ref.org")
         "* GIFT %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"GIFT\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; SELL     (k) Sell template
        ("pk" "SELL      (k) Sell" entry (file "ref.org")
         "* SELL %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"SELL\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; LOANED   (n) Loaned template
        ("pj" "LOANED    (n) Loaned" entry (file "ref.org")
         "* LOANED %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"LOANED\"     from \"\"           %U
  :END:" :empty-lines 1)

        ;; UNWANTED (j) Unwanted template
        ("pa" "UNWANTED  (a) Unwanted" entry (file "ref.org")
         "* UNWANTED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
    :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"UNWANTED\"   from \"\"           %U
  :END:" :empty-lines 1)

        ;; OWN      (o) Own template
        ("po" "OWN       (o) Own" entry (file "ref.org")
         "* OWN %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"OWN\"        from \"\"           %U
  :END:" :empty-lines 1)

        ;; GIFTED   (g) Gifted template
        ("pg" "GIFTED    (g) Gifted" entry (file "ref.org")
         "* GIFTED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"GIFTED\"     from \"\"           %U
  :END:" :empty-lines 1)

        ;; SOLD     (k) Sold template
        ("pc" "SOLD      (c) Sold" entry (file "ref.org")
         "* SOLD %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Merchant:
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"SOLD\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; DISCARDED(q) Purchased template
        ("pq" "DISCARDED (q) Purchased" entry (file "ref.org")
         "* DISCARDED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DISCARDED\"  from \"\"           %U
  :END:" :empty-lines 1)

        ;; Templates for the MULTIMEDIA keyword sequence
        ("m" "Multimedia")

        ;; CONSUME  (r) Consume template
        ("mr" "CONSUME   (r) Consume org-protocol" entry (file "ref.org")
         "* CONSUME [[%:link][%:description]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUME\"    from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; SUBSCRIBE(r) Subscribe template
        ("mb" "SUBSCRIBE (b) Subscribe org-protocol" entry (file "ref.org")
         "* SUBSCRIBE [[%:link][%:description
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SUBSCRIBE\"  from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; CONSUMING(l) Consuming template
        ("ml" "CONSUMING (l) Consuming org-protocol" entry (file "ref.org")
         "* CONSUMING [[%:link][%:description]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUMING\"  from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; SHARE    (s) Share template
        ("ms" "SHARE     (s) Share org-protocol" entry (file "ref.org")
         "* SHARE [[%:link][%:description
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SHARE\"      from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; IGNORED  (r) Ignored template
        ("mi" "IGNORED   (i) Ignored org-protocol" entry (file "ref.org")
         "* IGNORED [[%:link][%:description]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"IGNORED\"    from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; REFERENCE(f) Reference template
        ("mf" "REFERENCE (f) Reference org-protocol" entry (file "ref.org")
         "* REFERENCE [[%:link][%:description]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"REFERENCE\"  from \"\"           %U
  :END:
  %:initial" :empty-lines 1)

        ;; These templates are used with the EVENTS TODO sequence
        ("e" "Events")

        ;; VISIT    (v) Visit template
        ("ev" "VISIT     (v) Visit" entry (file "ref.org")
         "* VISIT %?
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"VISIT\"      from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

        ;; DIDNOTGO (z) Didnotgo template
        ("ez" "DIDNOTGO  (z) Didnotgo" entry (file "ref.org")
         "* DIDNOTGO %?
  CLOSED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DIDNOTGO\"   from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

        ;; MEETING  (m) Meeting template
        ("em" "MEETING   (m) Meeting" entry (file "ref.org")
         "* MEETING %?
  CLOSED: %^U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"MEETING\"    from \"\"           %U
  :END:
  %^T--%^T" :empty-lines 1)

        ;; VISITED  (y) Visited template
        ("ey" "VISITED   (y) Visited" entry (file "ref.org")
         "* VISITED %?
  CLOSED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"VISITED\"    from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

        ("n" "Non-TODO States")
        ;;          (a) Account template
        ("na" "          (a) Account" entry (file+headline "org.org" "Accounts")
         "* %?
  :PROPERTIES:
  :Website:
  :Username:
  :Email:
  :Password: %(my/generate-openssl-password)
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

        ;;          (b) Business template
        ("nb" "          (b) Business" entry (file+headline "peo.org" "Businesses")
         "* %^{Company} %?
  :PROPERTIES:
  :Company:  %\\1
  :Phone:    %^{Phone}
  :Email:    %^{Email}
  :Website:  %^{Website}
  :Address:  %^{Address}
  :City:     %^{City}
  :State:    %^{State}
  :Zip:      %^{Zip}
  :Map:      [[google-maps:%\\5+%\\6+%\\7+%\\8][Google Maps]]
  :Wifi:
  :Pass:
  :Hours:
  :Yelp:     [[yelp-business:%^{Yelp}][%\\9]]
  :Facebook:
  :G_Plus:
  :Instagram:
  :Linkedin:
  :Twitter:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

        ;;          (c) Contact template
        ("nc" "          (c) Contact" entry (file+headline "peo.org" "People")
         "* %^{First} %^{Last}%?
  :PROPERTIES:
  :First:    %\\1
  :Middle:
  :Last:     %\\2
  :Birthday: %^{Birth Date}u
  :Phone:    %^{Phone}
  :Email:    %^{Email}
  :Website:
  :Address:  %^{Address}
  :City:     %^{City}
  :State:    %^{State}
  :Zip:      %^{Zip}
  :Map:      [[google-maps:%\\5+%\\6+%\\7+%\\8][Google Maps]]
  :Company:
  :W-Group:
  :W-Title:
  :W-Phone:
  :W-Email:
  :W-Website:
  :W-Address:
  :W-Office:
  :W-City:
  :W-State:
  :W-Zip:
  :W-Map:
  :Facebook:
  :G:
  :G-Plus:
  :G-Scho:
  :Github:
  :Instagram:
  :Linkedin:
  :OkCupid:
  :Reddit:
  :Twitter:
  :Yelp:
  :YouTube:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
** Wish List
   :LOGBOOK:
   - State \"TODO\"       from \"\"           %U
   :END:
** Tasks
   :LOGBOOK:
   - State \"TODO\"       from \"\"           %U
   :END:
*** TODO Wish %\\1 %\\2 a Happy Birthday
    DEADLINE: %^{Birthday}t
    :PROPERTIES:
    :Via:
    :Note:
    :END:
    :LOGBOOK:
    - State \"TODO\"       from \"\"           %U
    :END:
**** TODO Buy %\\1 %\\2 a Birthday Gift
     SCHEDULED: %^{Buy Gift By}t DEADLINE: %^{Birthday}t
     :PROPERTIES:
     :Via:
     :Note:
     :END:
     :LOGBOOK:
     - State \"TODO\"       from \"\"           %U
     :END:
*** TODO Buy %\\1 %\\2 a Christmas Gift
    SCHEDULED: <2016-12-01 Tue +1y> DEADLINE: <2016-12-25 Fri +1y>
    :PROPERTIES:
    :Via:
    :Note:
    :END:
    :LOGBOOK:
    - State \"TODO\"       from \"\"           %U
    :END:" :empty-lines 1)

        ;;          (e) Payment template
        ("ne" "          (e) Payment" entry (file "ref.org")
         "* Paid %? :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %^{Paid}
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\3]]
  :Merchant: [[peo:%^{Merchant}][%\\4]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

        ;;          (f) Fuel template
        ("nf" "          (f) Fuel" entry (file+headline "fin.org" "Fuel Up")
         "* Fuel Up at %^{Merchant|Costco Poway|Costco Mission Valley San Diego} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %\\2
  :Method:   [[fin:%^{Method|Wells Fargo Debit Account|Wells Fargo Credit Account}][%\\3]]
  :Per_Gal:  %^{Per Gallon}
  :Gallons:  %^{Gallons}
  :Beg_Mil:  %?
  :End_Mil:  %^{End Miles}
  :Tot_Mil:
  :MPG:
  :PPM:
  :Merchant: [[peo:%\\1][%\\1]]
  :Link:     [[val:fin/Receipts/%<%Y-%m-%d> %^{Merchant Short Name}.pdf][%<%Y-%m-%d> %\\7.pdf]]
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

        ;;          (n) Note template
        ("nn" "          (n) Note" entry (file "ref.org")
         "* %? :note:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

        ;;          (h) Heading template
        ("nh" "          (h) Heading" entry (file "ref.org")
         "* %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

        ;;          (j) Journal template
        ("nj" "          (j) Journal" entry (file+datetree "jnl.org")
         "* Journal :org:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %t\n\n  %?" :empty-lines 1)

        ;;          (p) Paycheck template
        ("np" "          (p) Paycheck" entry (file+headline "fin.org" "Paycheck")
         "* Paycheck :fin:
  :PROPERTIES:
  :Cost:     -%^{Amount}
  :Paid:     -%\\1
  :Method:   [[fin:Wells Fargo Debit Account][Wells Fargo Debit Account]]
  :Merchant: [[peo:General Atomics Aeronautical Systems Inc.][General Atomics Aeronautical Systems Inc.]]
  :Link:     [[val:fin/Banking/Work/General Atomics Aeronautical Systems Inc./Paycheck/%<%Y-%m-%d>.pdf][%<%Y-%m-%d>.pdf]]
  :Note:     %?
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

        ;;          (s) Shopping template
        ("ns" "          (s) Shopping" entry (file "ref.org")
         "* %^{Action|Paid|Shopped at|Ate at|Drank at} %^{Merchant|Sprouts Hillcrest San Diego|Trader Joe's Hillcrest San Diego|Trader Joe's Mira Mesa San Diego|Farmer's Market Hillcrest San Diego|Costco Poway|Costco Mission Valley San Diego|Target Mission Valley San Diego|Poncho Villa North Park San Diego|VONS Poway|Ralphs Hillcrest San Diego|Whole Foods Hillcrest San Diego} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %^{Paid}
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\5]]
  :Merchant: [[peo:%\\2][%\\2]]
  :Link:     %?
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T
  | Item                           | Price ($) | Amount    | Total ($) |
  |                                | <9>       | <9>       | <9>       |
  |--------------------------------+-----------+-----------+-----------|
  |                                |           |           |           |
  |                                |           |           |           |
  |--------------------------------+-----------+-----------+-----------|
  | Tax                            |           | 1         |           |
  | Total                          |           |           |           |
  #+TBLFM: $4=$2*$3;%.2f::@>$4=vsum(@3..@-1);%.2f
  " :empty-lines 1)

        ;;          (t) Transfer template
        ("nt" "          (t) Transfer" entry (file "ref.org")
         "* Transferred %? :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     0.00
  :Method:   [[fin:%^{Method|Wells Fargo Debit Account|Wells Fargo Savings Account|Wells Fargo Credit Account|Wells Fargo Checking Account|GE Capital Credit Card}][%\\2]]
  :Merchant: [[fin:%^{Merchant|Wells Fargo Checking Account|Wells Fargo Savings Account|Wells Fargo Credit Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\3]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

        ("o" "Org-Protocol")
        ;; TODO     (t) Org-protocol todo template
        ;; Alternatively use [[%:link][%:description]] for :Via:
        ("ot" "TODO      (t) Org-Protocol Todo" entry (file "ref.org")
         "* TODO %?
  :PROPERTIES:
  :Via:      %:annotation
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

        ;; MEETING  (m) Meeting template
        ("om" "MEETING   (m) Org-Protocol Meeting" entry (file "ref.org")
         "* MEETING %:description
  CLOSED: %^U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location: %?
  :Via:      %:annotation
  :Note:
  :END:
  :LOGBOOK:
  - State \"MEETING\"    from \"\"           %U
  :END:
  %^T" :empty-lines 1)

        ;; REFERENCE(f) Reference template
        ("ow" "Web site" entry
         (file "ref.org")
         "* %a :website:\n\n%U %?\n\n%:initial")

        ))

;;; Refiling

(setq org-refile-use-cache nil)
;; see https://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
(setq org-agenda-files (directory-files-recursively "~/.emacs.d/journal/" "\\.org$"))

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; org mode adapt indentation default is nil
(setq org-adapt-indentation t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))


;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!


;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")


(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (setq org-confirm-babel-evaluate nil)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link)))

(when (maybe-require-package 'plantuml-mode)
  (with-eval-after-load 'plantuml-mode
    (setq plantuml-exec-mode 'jar)
    (setq plantuml-default-exec-mode 'jar)
    (setq plantuml-indent-level 2)
    (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
    (setq org-plantuml-exec-mode 'jar)
    (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
    (setq org-plantuml-executable-args "-headless -charset UTF-8")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (go . t)
      (groovy . t)
      (haskell . nil)
      (http . t)
      (java . t)
      (kotlin . t)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (table . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t)))))

(setq org-export-backends '(ascii html latex md))

(when (display-graphic-p)
  (when (maybe-require-package 'valign)
    (add-hook 'org-mode-hook #'valign-mode))
  (when (maybe-require-package 'org-modern)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-mode)))

(provide 'init-org)
;;; init-org.el ends here
