;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

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


;;  (if (fboundp 'gnutls-available-p)
;;      (fmakunbound 'gnutls-available-p))

;; A secure Emacs environment
;; Great article why Your editor is malware. The following basically sets up the configuration to adhere to the articles recommendations.

;; menu bar
(menu-bar-mode -1)

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

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Dagnachewa Argaw"
      user-mail-address "dagnachewa@gmail.com")

;; authinfo
(setq auth-sources '("~/.authinfo.gpg"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "Fira Code" :size 21)
      doom-variable-pitch-font (font-spec :family "sans"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

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


;;=== modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;=== end modeline

;;=== mu4e
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
   smtpmail-auth-credentials (expand-file-name "~/gmail.pass.gpg")
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-debug-info t
   starttls-extra-arguments nil
   starttls-gnutls-program "/usr/bin/gnutls-cli"
   starttls-extra-arguments nil
   starttls-use-gnutls t
   )

  ;;org-mu4
  ;;store org-mode links to messages
  (require 'org-mu4e)

  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)

  ;; when mail is sent, automatically convert org body to HTML
  (setq org-mu4e-convert-to-html t)

  (setq org-capture-templates
      '(("t" "todo" entry (file+headline "/www/org/todo.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
  (define-key mu4e-headers-mode-map (kbd "c") 'org-capture)
  ;; (define-key mu4e-view-mode-map (kbd "c") 'org-capture)

  (add-hook 'message-mode-hook 'orgstruct++-mode 'append)
  (add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
  (add-hook 'message-mode-hook 'org-bullets-mode 'append)
  (add-hook 'message-mode-hook 'orgtbl-mode 'append)
  (add-hook 'message-mode-hook 'auto-complete-mode 'append)

  ;; org-mu4e end

  )
;;=== end mu4e config



;;=== org-mode

;; Agenda

(setq org-agenda-files '("/www/org/gtd/" "/www/org/notes/")
      org-agenda-diary-file "/www/org/diary.org"
      org-agenda-use-time-grid nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-habit-show-habits t)

;; Captures

(after! org (setq org-capture-templates
      '(("g" "Getting things done")
        ("r" "References")
        ("d" "Diary")
        ("p" "Graph Data")
        ("t" "Data Tracker"))))

;; GTD Recurring Tasks

(after! org (add-to-list 'org-capture-templates
             '("gx" "Recurring Task" entry (file "/www/org/gtd/recurring.org")
               "* TODO %^{description}
:PROPERTIES:
:CREATED:    %U
:END:
:RESOURCES:
%^{url}
:END:

\** notes
%?")))

;; GTD Projects

(after! org (add-to-list 'org-capture-templates
             '("gp" "Project" entry (file+headline"/www/org/gtd/tasks.org" "Projects")
"* TODO %^{Description}
:PROPERTIES:
:SUBJECT: %^{subject}
:GOAL:    %^{goal}
:END:
:RESOURCES:
[[%^{url}]]
:END:

\*requirements*:
%^{requirements}

\*notes*:
%?

\** TODO %^{task1}")))

;; GTD Capture

(after! org (add-to-list 'org-capture-templates
             '("gt" "Task" entry (file"/www/org/gtd/inbox.org")
"** TODO %^{description}
:PROPERTIES:
:CREATED:    %U
:END:
:RESOURCES:
[[%^{url}]]
:END:

\*next steps*:
- [ ] %^{next steps}

\*notes*:
%?")))

;; Reference - Yank Example

(after! org (add-to-list 'org-capture-templates
             '("re" "Yank new Example" entry(file+headline"/www/org/notes/examples.org" "INBOX")
"* %^{example}
:PROPERTIES:
:SOURCE:  %^{source|Command|Script|Code|Usage}
:SUBJECT: %^{subject}
:END:

\#+BEGIN_SRC
%x
\#+END_SRC
%?")))

;; Reference - New Entry

(after! org (add-to-list 'org-capture-templates
             '("rn" "Yank new Example" entry(file+headline"~/.org/notes/references.org" "INBOX")
"* %^{example}
:PROPERTIES:
:CATEGORY: %^{category}
:SUBJECT:  %^{subject}
:END:
:RESOURCES:
:END:

%?")))

;; Diary - Daily Log

(after! org (add-to-list 'org-capture-templates
             '("dn" "New Diary Entry" entry(file+olp+datetree"/www/org/diary.org" "Dailies")
"* %^{example}
:PROPERTIES:
:CATEGORY: %^{category}
:SUBJECT:  %^{subject}
:MOOD:     %^{mood}
:END:
:RESOURCES:
:END:

\*What was one good thing you learned today?*:
- %^{whatilearnedtoday}

\*List one thing you could have done better*:
- %^{onethingdobetter}

\*Describe in your own words how your day was*:
- %?")))

;; Directories

(setq org-directory "/www/org/"
      org-image-actual-width nil
      +org-export-directory "~/.export/"
      org-archive-location "/www/org/gtd/archive.org::datetree/"
      org-default-notes-file "/www/org/gtd/inbox.org"
      projectile-project-search-path '("~/"))

;; Exports

(setq org-html-head-include-scripts t
      org-export-with-toc t
      org-export-with-author t
      org-export-headline-levels 5
      org-export-with-drawers t
      org-export-with-email t
      org-export-with-footnotes t
      org-export-with-latex t
      org-export-with-section-numbers nil
      org-export-with-properties t
      org-export-with-smart-quotes t)

;;(after! org (add-to-list 'org-export-backends 'pandoc))
;;(after! org (add-to-list 'org-export-backends 'pdf))

;; TODO Faces

;; Need to add condition to adjust faces based on theme select.

(after! org (setq org-todo-keyword-faces
      '(("TODO" :foreground "tomato" :weight bold)
        ("WAITING" :foreground "light sea green" :weight bold)
        ("STARTED" :foreground "DodgerBlue" :weight bold)
        ("DELEGATED" :foreground "Gold" :weight bold)
        ("NEXT" :foreground "violet red" :weight bold)
        ("DONE" :foreground "slategrey" :weight bold))))

;; Keywords

(after! org (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w!)" "STARTED(s!)" "NEXT(n!)" "DELEGATED(d!)" "|" "INVALID(I!)" "DONE(d!)"))))

;; TODO Latex Exports



(with-eval-after-load 'ox-latex
        (add-to-list 'org-latex-classes
                     '("assign"
                       "\\documentclass{article}
  \\usepackage{amsmath,amsfonts,stmaryrd,amssymb}
  \\usepackage{enumerate} 
  \\usepackage[ruled]{algorithm2e}
  \\usepackage[framemethod=tikz]{mdframed} 
  \\usepackage{listings}
  \\usepackage[footnote]{snotez} 
  \\usepackage[french]{babel}
  \\lstset{
        basicstyle=\\ttfamily, 
  }


    \\usepackage{geometry}

    \\geometry{
        paper=a4paper, 
        top=40pt, 
        bottom=3cm, 
        left=30pt,
        textwidth=417pt, 
        headheight=14pt,
        marginparsep=20pt,
        marginparwidth=100pt,
        footskip=30pt, 
        headsep=0cm,
    }


    \\usepackage[utf8]{inputenc} 
    \\usepackage{sansmathfonts} 
    \\usepackage[T1]{fontenc} 
    \\renewcommand*\\familydefault{\\sfdefault}
  \\mdfdefinestyle{commandline}{
      leftmargin=10pt,
      rightmargin=10pt,
      innerleftmargin=15pt,
      middlelinecolor=black!50!white,
      middlelinewidth=2pt,
      frametitlerule=false,
      backgroundcolor=black!5!white,
      frametitle={Ligne de commande},
      frametitlefont={\\normalfont\\sffamily\\color{white}\\hspace{-1em}},
      frametitlebackgroundcolor=black!50!white,
      nobreak,
  }


  \\newenvironment{commandline}{
      \\medskip
      \\begin{mdframed}[style=commandline]
  }{
      \\end{mdframed}
      \\medskip
  }


  \\mdfdefinestyle{question}{
      innertopmargin=1.2\\baselineskip,
      innerbottommargin=0.8\\baselineskip,
      roundcorner=5pt,
      nobreak,
      singleextra={
          \\draw(P-|O)node[xshift=1em,anchor=west,fill=white,draw,rounded corners=5pt]{
          Question \\theQuestion\\questionTitle};
      },
  }

  \\newcounter{Question} 


  \\newenvironment{question}[1][\\unskip]{
      \\bigskip
      \\stepcounter{Question}
      \\newcommand{\\questionTitle}{~#1}
      \\begin{mdframed}[style=question]
  }{
      \\end{mdframed}
      \\medskip
  }



  \\mdfdefinestyle{warning}{
      topline=false, bottomline=false,
      leftline=false, rightline=false,
      nobreak,
      singleextra={
          \\draw(P-|O)++(-0.5em,0)node(tmp1){};
          \\draw(P-|O)++(0.5em,0)node(tmp2){};
          \\fill[black,rotate around={45:(P-|O)}](tmp1)rectangle(tmp2);
          \\node at(P-|O){\\color{white}\\scriptsize\\bf !};
          \\draw[very thick](P-|O)++(0,-1em)--(O);
      }
  }


  \\newenvironment{warning}[1][Attention:]{ 
      \\medskip
      \\begin{mdframed}[style=warning]
          \\noindent{\\textbf{#1}}
  }{
      \\end{mdframed}
  }



  \\mdfdefinestyle{info}{
      topline=false, bottomline=false,
      leftline=false, rightline=false,
      nobreak,
      singleextra={
          \\fill[black](P-|O)circle[radius=0.4em];
          \\node at(P-|O){\\color{white}\\scriptsize\\bf i};
          \\draw[very thick](P-|O)++(0,-0.8em)--(O);
      }
  }

  \\newenvironment{info}[1][Info:]{ 
      \\medskip
      \\begin{mdframed}[style=info]
          \\noindent{\\textbf{#1}}
  }{
      \\end{mdframed}
  }"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
)


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

;; Prettify

(setq org-bullets-bullet-list '("✖" "✚")
      org-ellipsis "▼")

;; Publishing

(setq org-publish-project-alist
      '(("references-attachments"
         :base-directory "/www/org/notes/images/"
         :base-extension "jpg\\|jpeg\\|png\\|pdf\\|css"
         :publishing-directory "~/publish_html/references/images"
         :publishing-function org-publish-attachment)
        ("references-md"
         :base-directory "/www/org/notes/"
         :publishing-directory "/www/org/notes/publish_md"
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
         :base-directory "/www/org/gtd/"
         :publishing-directory "/www/org/gtd/publish_tasks"
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
         :base-directory "/www/org/gtd/references/"
         :base-extension "org"
         :publishing-directory "/www/org/publish"
         :preparation-function somepreparationfunction
         :completion-function  somecompletionfunction
         :publishing-function org-latex-publish-to-pdf
         :recursive t
         :latex-class "koma-article"
         :headline-levels 5
         :with-toc t)
         ("myprojectweb" :components("references-attachments" "pdf" "references-md" "tasks"))))

;; Refiling

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-hide-emphasis-markers nil
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;; Tags

(setq org-tags-column -80
      org-tag-persistent-alist '(("@email" . ?e) ("@write" . ?W) ("@phone" . ?p) ("@configure" . ?C) ("@work" . ?w) ("@personal" . ?l) ("@read" . ?r) ("@watch" . ?W) ("@computer" . ?c) ("@bills" . ?b) ("@purchase" . ?P)))

;; Org-Mind-Map
(use-package org-mind-map
  :init
  (require 'ox-org)
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

;; Super Agenda Groups
;;(org-super-agenda-mode t)
(after! org-agenda (setq org-agenda-custom-commands
                         '(("t" "Tasks"
                            ((agenda ""
                                     ((org-agenda-files '("/www/org/gtd/tasks.org" "/www/org/gtd/tickler.org" "/www/org/gtd/projects.org"))
                                      (org-agenda-overriding-header "What's on my calendar")
                                      (org-agenda-span 'day)
                                      (org-agenda-start-day (org-today))
                                      (org-agenda-current-span 'day)
                                      (org-super-agenda-groups
                                       '((:name "[[/www/org/gtd/habits.org][Habits]]"
                                                :habit t
                                                :order 1)
                                         (:name "[[/www/org/gtd/recurring.org][Bills]]"
                                                :tag "@bills"
                                                :order 4)
                                         (:name "Today's Schedule"
                                                :time-grid t
                                                :scheduled t
                                                :deadline t
                                                :order 13)))))
                             (todo "TODO|NEXT|REVIEW|WAITING|IN-PROGRESS"
                                   ((org-agenda-overriding-header "[[/www/org/gtd/tasks.org][Task list]]")
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
                                    (org-agenda-files '("/www/org/gtd/tasks.org"))
                                    (org-super-agenda-groups
                                     '((:auto-property "WHO")))))
                             (todo ""
                                   ((org-agenda-overriding-header "References")
                                    (org-agenda-files '("/www/org/gtd/references.org"))
                                    (org-super-agenda-groups
                                     '((:auto-ts t)))))))
                           ("i" "Inbox"
                            ((todo ""
                                   ((org-agenda-files '("/www/org/gtd/inbox.org"))
                                    (org-agenda-overriding-header "Items in my inbox")
                                    (org-super-agenda-groups
                                     '((:auto-ts t)))))))
                           ("x" "Get to someday"
                            ((todo ""
                                        ((org-agenda-overriding-header "Projects marked Someday")
                                         (org-agenda-files '("/www/org/gtd/someday.org"))
                                         (org-super-agenda-groups
                                          '((:auto-ts t))))))))))



;;=== org-mode end

;; org-roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/www/org/roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; deft
(setq deft-directory "/www/org/roam"
                deft-extensions '("md" "org"))

;; sql
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

;;=== end sql config

;; ocaml
(add-to-list 'load-path "/www/perrierjouet/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; rmsbolt
(setq rmsbolt-disassemble t)

;; +bindings disabeled so I need actions for buttons on the dashboard
(map! "C-x C-r" #'recentf-open-files
      "C-x a" #'org-agenda
      "C-x p" #'doom/open-private-config
      "C-x C-p" #'projectile-switch-project
      "C-x C-L" #'doom/quickload-session)

;; ispell
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
