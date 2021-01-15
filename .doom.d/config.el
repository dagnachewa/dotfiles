;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; History
;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; UTF-8
;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Menu bar
(menu-bar-mode -1)

;; A secure Emacs environment
;; Great article why Your editor is malware. The following basically sets up the configuration to adhere to the articles recommendations.

(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")
                 ))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Define Constants
(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Do we have python3?") 
  
(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Do we have pip3?")
 

(defconst eaf-env-p
  (and *sys/linux* (display-graphic-p) python-p pip-p
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dagnachew Argaw"
      user-mail-address "dagnachewa@gmail.com")
      
;; Authinfo
(setq auth-sources '("~/.authinfo.gpg"))


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 21)
      doom-variable-pitch-font (font-spec :family "sans"))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/data/www/org.git/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq diary-file "/data/www/org.git/diary.org")
(setq org-directory "/data/www/org.git/")
(setq projectile-project-search-path "/data/www/")

;; Org-Mode
(require 'org-habit)
(require 'org-id)
(require 'org-checklist)

;; Reveal

(require 'ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-title-slide nil)

(after! org (setq org-archive-location "/data/www/org.git/gtd/archives.org::* %s"
                  ;org-image-actual-width (truncate (* (display-pixel-width) 0.15))
                  org-link-file-path-type 'relative
                  org-log-state-notes-insert-after-drawers t
                  org-catch-invisible-edits 'error
                  org-refile-targets '((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 4))
                  org-refile-use-outline-path 'buffer-name
                  org-outline-path-complete-in-steps nil
                  org-refile-allow-creating-parent-nodes 'confirm
                  org-startup-indented 'indent
                  org-insert-heading-respect-content t
                  org-startup-folded 'content
                  org-src-tab-acts-natively t
                  org-list-allow-alphabetical nil))

(add-hook 'org-mode-hook 'auto-fill-mode)
;(add-hook 'org-mode-hook 'hl-todo-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Agenda

(setq org-agenda-todo-ignore-scheduled nil
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-fontify-priorities t)

(setq org-agenda-custom-commands nil)
(push '("o" "overview"
        ((agenda ""
                 ((org-agenda-span '1)
                  (org-agenda-files '(list "/data/www/org.git/"))
                  (org-agenda-start-day (org-today))))
         (tags-todo "-SOMEDAY-@delegated/+NEXT"
                    ((org-agenda-overriding-header "Next Tasks")
                     (org-agenda-todo-ignore-scheduled t)
                     (org-agenda-todo-ignore-deadlines t)
                     (org-agenda-todo-ignore-with-date t)
                     (org-agenda-sorting-strategy
                      '(category-up))))
         (tags-todo "-SOMEDAY/+READ"
                    ((org-agenda-overriding-header "To Read")
                     (org-agenda-todo-ignore-scheduled t)
                     (org-agenda-todo-ignore-deadlines t)
                     (org-agenda-todo-ignore-with-date t)
                     (org-agenda-sorting-strategy
                      '(category-up))))
         (tags-todo "-@delegated-SOMEDAY/-NEXT-REFILE-READ"
                    ((org-agenda-overriding-header "Other Tasks")
                     (org-agenda-todo-ignore-scheduled t)
                     (org-agenda-todo-ignore-deadlines t)
                     (org-agenda-todo-ignore-with-date t)
                     (org-agenda-sorting-strategy
                      '(category-up)))))) org-agenda-custom-commands)

(push '("b" "bullet"
        ((agenda ""
                 ((org-agenda-span '2)
                  (org-agenda-files (append (file-expand-wildcards "/data/www/org.git/bullet/*.org")))
                  (org-agenda-start-day (org-today))))
         (tags-todo "-someday/"
                    ((org-agenda-overriding-header "Task Items")
                     (org-agenda-files (append (file-expand-wildcards "/data/www/org.git/bullet/*.org")))
                     (org-agenda-todo-ignore-scheduled t)
                     (org-agenda-todo-ignore-deadlines t)
                     (org-agenda-todo-ignore-with-date t)))
         (tags "note"
               ((org-agenda-overriding-header "Notes")
                (org-agenda-files (append (file-expand-wildcards "/data/www/org.git/bullet/*.org"))))))) org-agenda-custom-commands)

(push '("g" "goals"
        ((tags-todo "Goal=\"prof-python\"/")
         (tags-todo "Goal=\"prof-datascience\"/"))) org-agenda-custom-commands)

(push '("i" "inbox"
        ((todo "REFILE"
               ((org-tags-match-list-sublevels nil)
                                        ;(org-agenda-skip-function 'nm/tasks-refile)
                (org-agenda-overriding-header "Ready to Refile"))))) org-agenda-custom-commands)

;; Capture Templates

(setq org-capture-templates '(("c" " checklist")
                              ("g" " gtd")
                              ("b" " bullet journal")
                              ("n" " notes")
                              ("r" " resources")
                              ("p" " projects")))

(push '("pt" " task" entry (function nm/find-project-task) "* REFILE %^{task} %^g" :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)
(push '("pr" " define requirements" item (function nm/find-project-requirement) "" :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)
(push '("pn" " note" entry (function nm/find-project-note) "* " :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)
(push '("pf" " timeframe" entry (function nm/find-project-timeframe) "* %^{timeframe entry} [%<%Y-%m-%d %a %H:%M>]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?" :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)

(push '("cs" " simple checklist" checkitem (file+olp "~/projects/orgmode/gtd/tasks.org" "Checklists") "- [ ] %?") org-capture-templates)
(push '("cd" " checklist [date]" checkitem (file+function "~/projects/orgmode/gtd/tasks.org" nm/org-capture-to-task-file) "- [ ] %?") org-capture-templates)

(push '("gs" " simple task" entry (file+olp "~/projects/orgmode/gtd/tasks.org" "Inbox") "* REFILE %^{task} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n") org-capture-templates)
(push '("gk" " task [kill-ring]" entry (file+olp "~/projects/orgmode/gtd/tasks.org" "Inbox") "* REFILE %^{task} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%c") org-capture-templates)
(push '("gg" " task with goal" entry (file+olp "~/projects/orgmode/gtd/tasks.org" "Inbox") "* REFILE %^{task}%^{GOAL}p %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n") org-capture-templates)

(push '("bt" " bullet task" entry (file+function "~/projects/orgmode/gtd/bullet.org" nm/capture-bullet-journal) "* REFILE %^{task} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)

(push '("nj" " journal" entry (function nm/capture-to-journal) "* %^{entry}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?") org-capture-templates)
(push '("na" " append" plain (function nm/org-capture-log) " *Note added:* [%<%Y-%m-%d %a %H:%M>]\n%?" :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)
(push '("nn" " new note" plain (function nm/create-notes-file) "%?" :unnarrowed t :empty-lines-before 1 :empty-lines-after 1) org-capture-templates)

(push '("rr" " research literature" entry (file+function "~/projects/orgmode/gtd/websources.org" nm/enter-headline-websources) "* READ %(get-page-title (current-kill 0))") org-capture-templates)
(push '("rf" " rss feed" entry (file+function "~/projects/orgmode/elfeed.org" nm/return-headline-in-file) "* %^{link}") org-capture-templates)

;; This function is used in conjuction with the capture template "new note" which will find or generate a note based off the folder and filename.
(defun nm/create-notes-file ()
  "Function for creating a notes file under org-capture-templates."
  (nm/find-file-or-create t org-directory "note"))

(defun nm/find-project-task ()
  "Function for creating a project file under org-capture-templates."
  (nm/find-file-or-create t "~/projects/orgmode/gtd/projects" "project" "Tasks"))

(defun nm/find-project-timeframe ()
  "Function for creating a project file under org-capture-templates."
  (nm/find-file-or-create t "~/projects/orgmode/gtd/projects" "project" "Timeframe"))

(defun nm/find-project-requirement ()
  "Function for creating a project file under org-capture-templates."
  (nm/find-file-or-create t "~/projects/orgmode/gtd/projects" "project" "Requirements"))

(defun nm/find-project-note ()
  "Function for creating a project file under org-capture-templates."
  (nm/find-file-or-create t "~/projects/orgmode/gtd/projects" "project" "Notes"))

(defun nm/return-headline-in-file ()
  "Returns the headline position."
  (let* ((org-agenda-files "~/projects/orgmode/elfeed.org")
         (location (nth 3 (org-refile-get-location nil nil 'confirm))))
    (goto-char location)
    (org-end-of-line)))

(defun nm/enter-headline-websources ()
  "This is a simple function for the purposes when using org-capture to add my entries to a custom Headline, and if URL is not in clipboard it'll return an error and cancel the capture process."
  (let* ((file "~/projects/orgmode/gtd/websources.org")
         (headline (read-string "Headline? ")))
    (progn
      (nm/check-headline-exist file headline)
      (goto-char (point-min))
      (re-search-forward (format "^\*+\s%s" (upcase headline))))))

(defun nm/check-headline-exist (file-arg headline-arg)
  "This function will check if HEADLINE-ARG exists in FILE-ARG, and if not it creates the headline."
  (save-excursion (find-file file-arg) (goto-char (point-min))
                  (unless (re-search-forward (format "* %s" (upcase headline-arg)) nil t)
                    (goto-char (point-max)) (insert (format "* %s" (upcase headline-arg))) (org-set-property "CATEGORY" (downcase headline-arg)))) t)

;; Clock Settings
(after! org (setq org-clock-continuously t)) ; Will fill in gaps between the last and current clocked-in task.

(setq org-tags-column 0)

(setq org-tag-alist '(("@home")
                      ("@computer")
                      ("@email")
                      ("@call")
                      ("@brainstorm")
                      ("@write")
                      ("@read")
                      ("@code")
                      ("@research")
                      ("@purchase")
                      ("@payment")
                      ("@place")))

(push '("delegated") org-tag-alist)
(push '("waiting") org-tag-alist)
(push '("someday") org-tag-alist)
(push '("remember") org-tag-alist)

;; Export Settings

(after! org (setq org-html-head-include-scripts t
                  org-export-with-toc t
                  org-export-with-author t
                  org-export-headline-levels 4
                  org-export-with-drawers nil
                  org-export-with-email t
                  org-export-with-footnotes t
                  org-export-with-sub-superscripts nil
                  org-export-with-latex t
                  org-export-with-section-numbers nil
                  org-export-with-properties nil
                  org-export-with-smart-quotes t
                  org-export-backends '(pdf ascii html latex odt md pandoc)))

;; Embed images into the exported HTML files.

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun org-html--format-image (source attributes info)
  (progn
    (setq source (replace-in-string "%20" " " source))
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
            (or (file-name-extension source) "")
            (base64-encode-string
             (with-temp-buffer
               (insert-file-contents-literally source)
              (buffer-string)))
            (file-name-nondirectory source))))

;; Keywords

(custom-declare-face '+org-todo-next '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-next '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
(custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do.
           "READ(R)" ; Task item that needs to be read.
           "NEXT(n)" ; Task items that are ready to be worked.
           "REFILE(r)" ; Signifies a new task that needs to be categorized and bucketed.
           "PROJ(p)"  ; Project with multiple task items.
           "WAIT(w)"  ; Something external is holding up this task.
           "|"
           "DONE(d)"  ; Task successfully completed.
           "KILL(k)")) ; Task was cancelled, aborted or is no longer applicable.
        org-todo-keyword-faces
        '(("WAIT" . +org-todo-onhold)
          ("NEXT" . +org-todo-next)
          ("READ" . +org-todo-active)
          ("REFILE" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("TODO" . +org-todo-active)))

;; Loading agenda settings

(after! org (setq org-agenda-diary-file "/data/www/org.git/diary.org"
                  org-agenda-dim-blocked-tasks t ; grays out task items that are blocked by another task (EG: Projects with subtasks)
                  org-agenda-use-time-grid nil
                  org-agenda-tags-column 0
;                  org-agenda-hide-tags-regexp "\\w+" ; Hides tags in agenda-view
                  org-agenda-compact-blocks nil
                  org-agenda-block-separator " "
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-skip-deadline-if-done t
                  org-agenda-window-setup 'current-window
                  org-enforce-todo-checkbox-dependencies nil ; This has funny behavior, when t and you try changing a value on the parent task, it can lead to Emacs freezing up. TODO See if we can fix the freezing behavior when making changes in org-agenda-mode.
                  org-enforce-todo-dependencies t
                  org-habit-show-habits t))

(after! org (setq org-agenda-files (append (file-expand-wildcards "/data/www/org.git/gtd/*.org") (file-expand-wildcards "/data/www/org.git/gtd/*/*.org"))))

;; Logging and Drawers

(after! org (setq org-log-into-drawer t
                  org-log-done 'time
                  org-log-repeat 'time
                  org-log-redeadline 'note
                  org-log-reschedule 'note))

;; Looks and Feels

(after! org (setq org-hide-emphasis-markers t
                  org-hide-leading-stars t
                  org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))))

(when (require 'org-superstar nil 'noerror)
  (setq org-superstar-headline-bullets-list '("#")
        org-superstar-item-bullet-alist nil))

(when (require 'org-fancy-priorities nil 'noerror)
  (setq org-fancy-priorities-list '("⚑" "❗" "⬆")))

;; Properties

(after! org (setq org-use-property-inheritance t))

;; Publishing

(after! org (setq org-publish-project-alist
                  '(("attachments"
                     :base-directory "/data/www/org.git/"
                     :recursive t
                     :base-extension "jpg\\|jpeg\\|png\\|pdf\\|css"
                     :publishing-directory "~/publish_html"
                     :publishing-function org-publish-attachment)
                    ("Markdown-to-Orgmode"
                     :base-directory "/data/www/org.git/notes/"
                     :publishing-directory "/data/www/org.git/www/"
                     :base-extension "md"
                     :recursive t
                     :publishing-function org-md-publish-to-org)
                    ("notes"
                     :base-directory "/data/www/org.git/notes/"
                     :publishing-directory "/data/www/org.git/www/"
                     :section-numbers nil
                     :base-extension "org"
                     :with-properties nil
                     :with-drawers (not "LOGBOOK")
                     :with-timestamps active
                     :recursive t
                     :exclude "journal/.*"
                     :auto-sitemap t
                     :sitemap-filename "index.html"
                     :publishing-function org-html-publish-to-html
                     :html-head "<link rel=\"stylesheet\" href=\"https://raw.githack.com/nmartin84/raw-files/master/htmlpro.css\" type=\"text/css\"/>"
;                     :html-head "<link rel=\"stylesheet\" href=\"https://codepen.io/nmartin84/pen/RwPzMPe.css\" type=\"text/css\"/>"
;                     :html-head-extra "<style type=text/css>body{ max-width:80%;  }</style>"
                     :html-link-up "../"
                     :with-email t
                     :html-link-up "../../index.html"
                     :auto-preamble t
                     :with-toc t)
                    ("myprojectweb" :components("attachments" "notes" "ROAM")))))



;; Anki Editor
(use-package anki-editor
  :after org-noter
  :config
  ; I like making decks
  (setq anki-editor-create-decks 't))

;; DEFT

(setq deft-use-projectile-projects t)
(defun zyro/deft-update-directory ()
  "Updates deft directory to current projectile's project root folder and updates the deft buffer."
  (interactive)
  (if (projectile-project-p)
      (setq deft-directory (expand-file-name (doom-project-root)))))
(when deft-use-projectile-projects
  (add-hook 'projectile-after-switch-project-hook 'zyro/deft-update-directory)
  (add-hook 'projectile-after-switch-project-hook 'deft-refresh))

(use-package deft
  :bind (("<f8>" . deft))
  :commands (deft deft-open-file deft-new-file-named)
  :config
  (setq deft-directory "~/projects/orgmode/"
        deft-auto-save-interval 0
        deft-recursive t
        deft-current-sort-method 'title
        deft-extensions '("md" "txt" "org")
        deft-use-filter-string-for-filename t
        deft-use-filename-as-title nil
        deft-markdown-mode-title-level 1
        deft-file-naming-rules '((nospace . "-"))))

(defun my-deft/strip-quotes (str)
  (cond ((string-match "\"\\(.+\\)\"" str) (match-string 1 str))
        ((string-match "'\\(.+\\)'" str) (match-string 1 str))
        (t str)))

(defun my-deft/parse-title-from-front-matter-data (str)
  (if (string-match "^title: \\(.+\\)" str)
      (let* ((title-text (my-deft/strip-quotes (match-string 1 str)))
             (is-draft (string-match "^draft: true" str)))
        (concat (if is-draft "[DRAFT] " "") title-text))))

(defun my-deft/deft-file-relative-directory (filename)
  (file-name-directory (file-relative-name filename deft-directory)))

(defun my-deft/title-prefix-from-file-name (filename)
  (let ((reldir (my-deft/deft-file-relative-directory filename)))
    (if reldir
        (concat (directory-file-name reldir) " > "))))

(defun my-deft/parse-title-with-directory-prepended (orig &rest args)
  (let ((str (nth 1 args))
        (filename (car args)))
    (concat
      (my-deft/title-prefix-from-file-name filename)
      (let ((nondir (file-name-nondirectory filename)))
        (if (or (string-prefix-p "README" nondir)
                (string-suffix-p ".txt" filename))
            nondir
          (if (string-prefix-p "---\n" str)
              (my-deft/parse-title-from-front-matter-data
               (car (split-string (substring str 4) "\n---\n")))
            (apply orig args)))))))

(provide 'my-deft-title)

(advice-add 'deft-parse-title :around #'my-deft/parse-title-with-directory-prepended)

;; Beancount
(use-package! beancount
  :defer t
  :bind
  ("C-M-b" . (lambda ()
               (interactive)
               (find-file "~/Dropbox/beancount/main.bean")))
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/Dropbox/beancount/accounts/"
                         'full
                         (rx ".bean" eos))))


;; Elfeed

(use-package elfeed-org
  :defer
  :config
  (setq rmh-elfeed-org-files (list "/data/www/org.git/elfeed.org")))
(use-package elfeed
  :defer
  :config
  (setq elfeed-db-directory "~/.elfeed/"))

;; Mu4e
(after! mu4e
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-account-alist
        '(("gmail"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior delete)
           (user-mail-address "dagnachewa@gmail.com")
           (user-full-name "Dagnachew Argaw"))
          ))

  (setq mu4e-attachment-dir "/mnt/goyaves-home-data/downloads")
  (setq mu4e-maildir "/mnt/goyaves-home-data/maildir")
  (setq mu4e-trash-folder "/Trash")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-get-mail-command "mbsync -Va")
  ;;(mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mu4e-update-interval 300)
  ;;completion
  (setq mu4e-completing-read-function 'ivy-completing-read)
  ;;mu4e-headers
  (setq mu4e-headers-has-child-prefix '("+" . "")
        mu4e-headers-empty-parent-prefix '("-" . "")
        mu4e-headers-first-child-prefix '("-" . "")
        mu4e-headers-duplicate-prefix '("-" . "")
        mu4e-headers-default-prefix '("-" . "")
        mu4e-headers-draft-mark '("-" . "")
        mu4e-headers-flagged-mark '("-" . "")
        mu4e-headers-new-mark '("-" . "")
        mu4e-headers-passed-mark '("-" . "")
        mu4e-headers-replied-mark '("-" . "")
        mu4e-headers-trashed-mark '("-" . "")
        mu4e-headers-attach-mark '("-" . "")
        mu4e-headers-encrypted-mark '("-" . "")
        mu4e-headers-signed-mark '("-" . "")
        mu4e-headers-unread-mark '("-" . "")
        mu4e-headers-first-child-prefix '("\\" . "")
        mu4e-headers-duplicate-prefix '("=" . "")
        mu4e-headers-default-prefix '("|" . "")
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . "")
  )
  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
   '(("/gmail/INBOX" . ?i)
     ("/gmail/Drafts" . ?D)
     ("/gmail/Sent" . ?s)
     ("/gmail/Trash" . ?T)))

  (setq mu4e-headers-results-limit 5000)
  ;;(setq mu4e-enable-mode-line t)
  (use-package mu4e-alert
    :after mu4e
    :hook ((after-init . mu4e-alert-enable-mode-line-display)
           (after-init . mu4e-alert-enable-notifications))
    :config (mu4e-alert-set-default-style 'libnotify))

  (use-package smtpmail)

  (setq
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   user-mail-address "dagnachewa@gmail.com"
   user-full-name "dagnachew argaw"
   smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
   smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-debug-info t
   starttls-extra-arguments nil
   starttls-gnutls-program "/usr/bin/gnutls-cli"
   starttls-extra-arguments nil
   starttls-use-gnutls t
   )
  )

;; EAF

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki


;; Graphs and Chart Modules

;; Eventually I would like to have org-mind-map generating charts like Sacha’s evil-plans.

(after! org (setq org-ditaa-jar-path "~/.doom.d/site-lisp/ditaa.jar"))

;; Org-mind-map
(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(use-package gnuplot
  :defer
  :config
  (setq gnuplot-program "gnuplot"))

;; MERMAID
(use-package mermaid-mode
  :defer
  :config
  (setq mermaid-mmdc-location "/node_modules/.bin/mmdc"
        ob-mermaid-cli-path "/node-modules/.bin/mmdc"))

;; PLANTUML
(setq org-plantuml-jar-path (expand-file-name "~/.doom.d/site-lisp/plantuml.jar"))

;; Journal

(after! org (setq org-journal-dir "~/projects/orgmode/gtd/journal/"
                  org-journal-enable-agenda-integration t
                  org-journal-file-type 'monthly
                  org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"PROJ\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\""))

;; Pandoc

(setq org-pandoc-options '((standalone . t) (self-contained . t)))

;; Reveal

;;(require 'ox-reveal)
;;(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
;;(setq org-reveal-title-slide nil)

;; ROAM

(setq org-roam-tag-sources '(prop last-directory))
(setq org-roam-db-location "/data/www/org.git/roam.db")
(setq org-roam-directory "/data/www/org.git/roam/")
(setq org-roam-buffer-position 'right)
(setq org-roam-completion-everywhere t)

(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain (function org-roam-capture--get-point) ""
         :immediate-finish t
         :file-name "journal/%<%Y-%m-%d-%a>"
         :head "#+TITLE: %<%Y-%m-%d %a>\n#+STARTUP: content\n\n")))

(setq org-roam-capture-templates
      '(("l" "literature" plain (function org-roam-capture--get-point)
         :file-name "literature/%<%Y%m%d%H%M>-${slug}"
         :head "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+roam_tags: %^{roam_tags}\n\nsource: \n\n%?"
         :unnarrowed t)
        ("f" "fleeting" plain (function org-roam-capture--get-point)
         :file-name "fleeting/%<%Y%m%d%H%M>-${slug}"
         :head "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n\n%?"
         :unnarrowed t)
        ("p" "permanent in nested folder" plain (function org-roam-capture--get-point)
         :file-name "%(read-string \"string: \")/%<%Y%m%d%H%M>-${slug}"
         :head "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+roam_tags: %(read-string \"tags: \")\n\n"
         :unnarrowed t
         "%?")))

(push '("x" "Projects" plain (function org-roam-capture--get-point)
        :file-name "gtd/projects/%<%Y%m%d%H%M>-${slug}"
        :head "#+title: ${title}\n#+roam_tags: %^{tags}\n\n%?"
        :unnarrowed t) org-roam-capture-templates)




;; Sql
(setq sql-connection-alist
        '((server1 (sql-product 'postgres)
                   (sql-port 5432)
                   (sql-server "localhost")
                   (sql-user "perrierjouet")
                   (sql-database "postgres"))))

(defun my-sql-connect (product connection)
    (require my-password "~/.doom.d/my-password.el.gpg")

    (let ((connection-info (assoc connection sql-connection-alist))
          (sql-password (car (last (assoc connection my-sql-password)))))
      (delete sql-password connection-info)
      (nconc connection-info `((sql-password ,sql-password)))
      (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
      (add-to-list 'sql-connection-alist connection-info))

    (setq sql-product product)
    (sql-connect connection))

  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

;; Ocaml
(add-to-list 'load-path "/www/perrierjouet/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; Rmsbolt
(setq rmsbolt-disassemble t)

;; +bindings evil-mode disabeled so I need actions for buttons on the dashboard
(map! "C-x C-r" #'recentf-open-files
      "C-x a" #'org-agenda
      "C-x p" #'doom/open-private-config
      "C-x C-p" #'projectile-switch-project
      "C-x C-L" #'doom/quickload-session)

;; Ispell
(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US")

(bind-key "C-c F"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "fr_FR")
            (flyspell-buffer)))

(bind-key "C-c E"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "en_US")
            (flyspell-buffer)))

