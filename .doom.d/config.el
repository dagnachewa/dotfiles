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

;; Org-mode
;;(require 'ox')

;; Org-crypt
(setq org-crypt-key (expand-file-name "~/org-crypt-key.gpg"))

;; Org-pandoc-import
(use-package! org-pandoc-import :after org)

;;(load "~/.emacs.d/site-lisp/org-pandoc-import.el")
;;(load "~/.emacs.d/site-lisp/org-pandoc-transient.el")

;; authinfo
(use-package! authinfo-color-mode
  :mode ("authinfo.gpg\\'" . authinfo-color-mode)
  :init (advice-add 'authinfo-mode :override #'authinfo-color-mode))


;; abbrev mode
(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))


;; Calc
(add-hook 'calc-mode-hook #'calctex-mode)

;; Default directories
(setq org-directory (expand-file-name "/data/www/org.git"))
(setq org-default-notes-file (concat org-directory "/data/www/org.git/notes/notes.org"))
(setq org-agenda-files '(list "/data/www/org.git/"))

;; Todo and tags
(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
        ))

(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold))
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME" . ?h)
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o)
        ("DEV" . ?d)
        ("WWW" . ?w)
        (:endgroup . nil)
        (:startgroup . nil)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        (:endgroup . nil)
        ("UCANCODE" . ?c)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("BONUS" . ?b)
        ("noexport" . ?x)
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("WWW" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("KEY" . (:foreground "Red" :weight bold))
        ("EASY" . (:foreground "OrangeRed" :weight bold))
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
        ("HARD" . (:foreground "OrangeRed" :weight bold))
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("UCANCODE" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))
        )
)

;; Selection
(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)

;; Capture
(setq org-reverse-note-order t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "/www/org/mygtd.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("i" "Idea" entry (file+headline "/www/org/mygtd.org" "Someday/Maybe")
         "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
        )
      )

;; Speed Commands
(setq org-use-speed-commands t)

;; Fontification
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Evaluation
;; org-babel-default-header-args (for all)
;; org-babel-default-header-args:<lang>   (language specific)

;; Stop Org from evaluating code blocks to speed exports
(setq org-babel-default-header-args '((:eval . "never-export")))

;; File wide using PROPERTY
;;#+PROPERTY: header-args :eval never-export

;; Tangle a single code block
;; Define a function to tangle a single code block.

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)
))

;; Export
;; With smart quotes

(setq org-export-with-smart-quotes t)

;; To Text

;; Fix missing links in ASCII export

(setq org-ascii-links-to-notes nil)

;; Adjust the number of blank lines inserted around headlines

(setq org-ascii-headline-spacing (quote (1 . 1)))

;; To Github Flavored Markdown

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; To HTML

(setq org-html-coding-system 'utf-8-unix)

;; Remove validation link

;; html-validation-link was not available as a projects setting in Org-mode 8.2.

(setq org-html-validation-link nil)

;; These are required to view math properly.
(use-package! cdlatex
    :after (:any org-mode LaTeX-mode)
    :hook
    ((LaTeX-mode . turn-on-cdlatex)
     (org-mode . turn-on-org-cdlatex)))

(use-package! company-math
    :after (:any org-mode TeX-mode)
    :config
    (set-company-backend! 'org-mode 'company-math-symbols-latex)
    (set-company-backend! 'TeX-mode 'company-math-symbols-latex)
    (set-company-backend! 'org-mode 'company-latex-commands)
    (set-company-backend! 'TeX-mode 'company-latex-commands)
    (setq company-tooltip-align-annotations t)
    (setq company-math-allow-latex-symbols-in-faces t))


;; MathJax CDN

;; The defaults use an old MathJax version

(setf org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml nil))
      )
(setf org-html-mathjax-template
      "<script type=\"text/javascript\" src=\"%PATH\"></script>")

;; Table
(setq org-html-table-default-attributes
      '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))

;; Fix Literals Formatting

;; An extra line is added when exporting literals, i.e. line prefixed by :. The following modified org-export function trims the content before the export

(require 'subr-x)

(defun org-html-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "<pre class=\"example\">\n%s</pre>"
          (string-trim
           (org-html-do-format-code
           (org-remove-indentation
            (org-element-property :value fixed-width))))))

;; Publishing the Websites
;; Activation
(eval-after-load "org"
  '(require 'ox-publish nil t))

;; Configure Projects

(
 setq org-publish-project-alist
      '(
      	("references-attachments"
         :base-directory "/www/org.git/notes/images/"
         :base-extension "jpg\\|jpeg\\|png\\|pdf\\|css"
         :publishing-directory "/data/www/org.git/www/publish_html/references/images"
         :publishing-function org-publish-attachment)
        ("references-md"
         :base-directory "/www/org.git/notes/"
         :publishing-directory "/www/org.git/notes/publish_md"
         :base-extension "org"
         :recursive t
         :headline-levels 5
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :html-head "<link rel=\"stylesheet\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" type=\"text/css\"/>"
         :infojs-opt "view:t toc:t ltoc:t mouse:underline buttons:0 path:http://thomas.github.io/solarized-css/org-info.min.js"
         :with-email t
         :with-toc t)
        ("tasks"
         :base-directory "/www/org.git/gtd/"
         :publishing-directory "/www/org.git/gtd/publish_tasks"
         :base-extension "org"
         :recursive t
         :auto-sitemap t
         :sitemap-filename "index"
         :html-link-home "../index.html"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
;         :html-head "<link rel=\"stylesheet\"
;href=\"https://codepen.io/nmartin84/pen/MWWdwbm.css\"
;type=\"text/css\"/>"
         :with-email t
         :html-link-up ".."
         :auto-preamble t
         :with-toc t)
        ("pdf"
         :base-directory "/www/org.git/gtd/references/"
         :base-extension "org"
         :publishing-directory "/www/org.git/publish"
         :preparation-function somepreparationfunction
         :completion-function  somecompletionfunction
         :publishing-function org-latex-publish-to-pdf
         :recursive t
         :latex-class "koma-article"
         :headline-levels 5
         :with-toc t)
         ("myprojectweb" :components("references-attachments" "pdf" "references-md" "tasks"))

        ;; Web-site
        ("web-site"
         :base-directory "/data/www/org.git/"
         :base-extension "org"
         :publishing-directory "/data/www/org.git/www/"
         :recursive t
         :exclude ".*-template\.org\\|README\.org"        ; exclude org-reveal slides and other files
         :publishing-function org-html-publish-to-html
         :headline-levels 2               ; Just the default for this project.
         :auto-sitemap t                  ; Generate sitemap.org automagically...
         :sitemap-filename "org-sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Plan du site"         ; ... with title 'Sitemap'.
         :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
         :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
         :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
         :auto-preamble t;         ; Enable auto preamble
         :auto-postamble t         ; Enable auto postamble
         :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :toc-levels 1               ; Just the default for this project.
         :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
         :html-head-include-default-style nil ;Enable the default css style
         :html-head-include-scripts nil ;Disable the default javascript snippet
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<link rel=\"stylesheet\" type=\"text/css\" href=\"css/org.css\"/>\n<script type=\"text/javascript\" src=\"js/ga.min.js\"></script>" ;Enable custom css style and other tags
         :html-link-home "index.html"    ; Just the default for this project.
         :html-link-up "misc.html"    ; Just the default for this project.
         )

        ("org-static"
         :base-directory "/data/www/org.git/"
         :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R\\|el"
         :include (".htaccess")
         :publishing-directory "~/data/www/org.git/www"
         :recursive t
         :publishing-function org-publish-attachment
         :exclude "Rplots.pdf\\|README\\|LICENSE\\|\\.gitignore"
         )

        ("org"
         :components ("org-notes" "org-static")
         )
        )
      )

;; Org-mu4e
;; store org-mode links to messages
(require 'org-mu4e)

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

(setq org-capture-templates
    '(("t" "todo" entry (file+headline "/data/www/org.git/todo.org" "Tasks")
       "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
;;(define-key mu4e-headers-mode-map (kbd "c") 'org-capture)
;; (define-key mu4e-view-mode-map (kbd "c") 'org-capture)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'org-bullets-mode 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'auto-complete-mode 'append)

;; org-mu4e end

;; org-journal
(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir "/www/org.git/journal/")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
    (setq org-journal-enable-agenda-integration t)

;; org-roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/data/www/org.git/notes/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; org-roam-capture-ref-templates
(after! org-roam
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))


;; deft
(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "/data/www/org.git/notes/"))

;; Link Abbreviations

(setq org-link-abbrev-alist
      '(("doom-repo" . "https://github.com/hlissner/doom-emacs/%s")
        ("wolfram" . "https://wolframalpha.com/input/?i=%s")
        ("duckduckgo" . "https://duckduckgo.com/?q=%s")
        ("gmap" . "https://maps.google.com/maps?q=%s")
        ("gimages" . "https://google.com/images?q=%s")
        ("google" . "https://google.com/search?q=")
        ("youtube" . "https://youtube.com/watch?v=%s")
        ("youtu" . "https://youtube.com/results?search_query=%s")
        ("github" . "https://github.com/%s")
        ("attachments" . "~/.org/.attachments/")))

;; Logging & Drawers

(setq org-log-state-notes-insert-after-drawers nil
      org-log-into-drawer t
      org-log-done 'time
      org-log-repeat 'time
      org-log-redeadline 'note
      org-log-reschedule 'note)

;; Refiling

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-hide-emphasis-markers nil
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;; Tags

(setq org-tags-column -80
      org-tag-persistent-alist '(("@email" . ?e) ("@write" . ?W) ("@phone" . ?p) ("@configure" . ?C) ("@work" . ?w) ("@personal" . ?l) ("@read" . ?r) ("@watch" . ?W) ("@computer" . ?c) ("@bills" . ?b) ("@purchase" . ?P)))


;; Super Agenda Groups
;;(org-super-agenda-mode t)
(after! org-agenda (setq org-agenda-custom-commands
                         '(("t" "Tasks"
                            ((agenda ""
                                     ((org-agenda-files '(list "/data/www/org.git/"))
                                      (org-agenda-overriding-header "What's on my calendar")
                                      (org-agenda-span 'day)
                                      (org-agenda-start-day (org-today))
                                      (org-agenda-current-span 'day)
                                      (org-super-agenda-groups
                                       '((:name "[[/data/www/org.git/gtd/habits.org][Habits]]"
                                                :habit t
                                                :order 1)
                                         (:name "[[/data/www/org.git/gtd/recurring.org][Bills]]"
                                                :tag "@bills"
                                                :order 4)
                                         (:name "Today's Schedule"
                                                :time-grid t
                                                :scheduled t
                                                :deadline t
                                                :order 13)))))
                             (todo "TODO|NEXT|REVIEW|WAITING|IN-PROGRESS"
                                   ((org-agenda-overriding-header "[[/data/www/org.git/gtd/tasks.org][Task list]]")
                                    (org-agenda-files '("/www/org/gtd/tasks.org"))
                                    (org-super-agenda-groups
                                     '((:name "CRITICAL"
                                              :priority "A"
                                              :order 1)
                                       (:name "NEXT UP"
                                              :todo "NEXT"
                                              :order 2)
                                       (:name "Emacs Reading"
                                              :and (:category "Emacs" :tag "@read")
                                              :order 3)
                                       (:name "Emacs Config"
                                              :and (:category "Emacs" :tag "@configure")
                                              :order 4)
                                       (:name "Emacs Misc"
                                              :category "Emacs"
                                              :order 5)
                                       (:name "Task Reading"
                                              :and (:category "Tasks" :tag "@read")
                                              :order 6)
                                       (:name "Task Other"
                                              :category "Tasks"
                                              :order 7)
                                       (:name "Projects"
                                              :category "Projects"
                                              :order 8)))))
                             (todo "DELEGATED"
                                   ((org-agenda-overriding-header "Delegated Tasks by WHO")
                                    (org-agenda-files '("/data/www/org.git/gtd/tasks.org"))
                                    (org-super-agenda-groups
                                     '((:auto-property "WHO")))))
                             (todo ""
                                   ((org-agenda-overriding-header "References")
                                    (org-agenda-files '("/data/www/org.git/gtd/references.org"))
                                    (org-super-agenda-groups
                                     '((:auto-ts t)))))))
                           ("i" "Inbox"
                            ((todo ""
                                   ((org-agenda-files '("/data/www/org.git/gtd/inbox.org"))
                                    (org-agenda-overriding-header "Items in my inbox")
                                    (org-super-agenda-groups
                                     '((:auto-ts t)))))))
                           ("x" "Get to someday"
                            ((todo ""
                                        ((org-agenda-overriding-header "Projects marked Someday")
                                         (org-agenda-files '("/data/www/org.git/gtd/someday.org"))
                                         (org-super-agenda-groups
                                          '((:auto-ts t))))))))))


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

;; Org-mind-map
(load "~/.emacs.d/site-lisp/org-mind-map.el")

;; Anki Editor
(use-package anki-editor
  :after org-noter
  :config
  ; I like making decks
  (setq anki-editor-create-decks 't))

;; EAF  

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :if eaf-env-p
  :custom
  (eaf-find-alternate-file-in-dired t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-browser-continue-where-left-off t)
  :config
  (require 'eaf-org)
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-setq eaf-browser-default-zoom "1.25")
  (eaf-setq eaf-browser-dark-mode "false")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-pdf-dark-mode "false")
  (eaf-setq eaf-browser-enable-autofill "true")
  ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
  ;; Comment this line out of you don't want to use EAF to open available files in dired.
  ;; (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key clear_cookies "C-M-q" eaf-browser-keybinding)
  (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key eaf-send-key-sequence "M-]" eaf-terminal-keybinding)
  )

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

;; Plantuml
(setq org-plantuml-jar-path (expand-file-name "/home/perrierjouet/plantuml.jar"))

;; Ocaml
(add-to-list 'load-path "/www/perrierjouet/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; Rmsbolt
(setq rmsbolt-disassemble t)

;; +bindings disabeled so I need actions for buttons on the dashboard
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
