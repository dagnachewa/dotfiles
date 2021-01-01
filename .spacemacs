;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     erc 
     emacs-lisp
     git
     (ivy +fuzzy
	        +childframe
	        +prescient
	        +icons)

     ;; lang
     lsp 
     asciidoc
     asm
     (c-c++ :variables c-c++-backend 'lsp-clangd)
     (clojure :variables clojure-enable-fancify-symbols t)
     common-lisp
     coq
     csv
     crystal
     elixir
     emacs-lisp
     erlang
     ess
     go 
     gpu
     haskell
     html
     idris
     java
     javascript
     json
     (julia :variables julia-backend 'lsp)
     latex 
     (markdown :variables markdown-live-preview-engine 'vmd)
     ocaml
     (plantuml :variables plantuml-jar-path "~/plantuml.jar" org-plantuml-jar-path "~/plantuml.jar")
     prolog
     protobuf
     (python :variables python-backend 'anaconda)
     racket
     rust 
     (scala :variables scala-backend 'scala-metals)
     scheme
     sql
     (yaml :variables yaml-enable-lsp t)

     mu4e
     multiple-cursors
     org
     (shell :variables
             shell-default-height 30
             shell-default-position 'bottom) 
     spell-checking
     syntax-checking
     themes-megapack
     version-control
     treemacs

     ;; tools
     ansible
     (conda :variables conda-anaconda-home "/data/anaconda3")
     (docker :variables docker-dockerfile-backend 'lsp)
     elasticsearch
     fasd
     finance
     import-js
     ipython-notebook
     pandoc
     pass
     (ranger :variables
             ranger-override-dired 'ranger
             ranger-show-preview t)
     restclient
     (shell :variables shell-default-shell 'eshell)
     tern
     terraform
     tmux
     vagrant
     web-beautify

     ;; source control
     git
     github
     version-control

     ;; web-services
     search-engine

     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(ewal-doom-themes,org-pandoc-import,org-mind-map,(org-roam :location (recipe :fetcher github :repo "jethrokuan/org-roam")))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(emacsql-sqlite3)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (bookmarks . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-palenight
   			                 spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(all-the-icons :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Fira Code"
                               :size 19.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

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

;; Menu bar
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

;; Authinfo
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
(setq doom-theme 'doom-palenight)
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



;; Modeline
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

;;=== end modeline

  ;;=== Mu4e

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
  
;;=== end mu4e config

;; Org-mode
(use-package org
  :config
  ;; Default directories
  (setq org-directory "/data/www/org.git/")
  (setq org-default-notes-file (concat org-directory "/notes/notes.org"))

  (require 'ox)

  ;; Org-crypt
  (setq org-crypt-key "/home/perrierjouet/org-crypt-key.gpg")

  ;; authinfo
  (use-package authinfo-color-mode
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
    (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
    (setq save-abbrevs 'silently))

  ;; Calc 
  (add-hook 'calc-mode-hook #'calctex-mode)

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
      '(("t" "todo" entry (file+headline "/www/org/todo.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

  ;; Speed Commands
  (setq org-use-speed-commands t)

  ;; Fontification
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Evaluation
  ;;org-babel-default-header-args (for all)
  ;;org-babel-default-header-args:<lang>   (language specific)

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
           :base-directory "/data/www/org.git/notes/images/"
           :base-extension "jpg\\|jpeg\\|png\\|pdf\\|css"
           :publishing-directory "/data/data/www/org.git/www/publish_html/references/images"
           :publishing-function org-publish-attachment)
          ("references-md"
           :base-directory "/data/www/org.git/notes/"
           :publishing-directory "/data/www/org.git/notes/publish_md"
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
           :base-directory "/data/www/org.git/gtd/"
           :publishing-directory "/data/www/org.git/gtd/publish_tasks"
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
           :base-directory "/data/www/org.git/gtd/references/"
           :base-extension "org"
           :publishing-directory "/data/www/org.git/publish"
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
           :base-directory "/data/data/www/org.git/"
           :base-extension "org"
           :publishing-directory "/data/data/www/org.git/www/"
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
           :base-directory "/data/data/www/org.git/"
           :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R\\|el"
           :include (".htaccess")
           :publishing-directory "~/data/data/www/org.git/www"
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
  
  ;;(define-key mu4e-headers-mode-map (kbd "c") 'org-capture)
  
  ;; (define-key mu4e-view-mode-map (kbd "c") 'org-capture)

  (add-hook 'message-mode-hook 'orgstruct++-mode 'append)
  (add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
  (add-hook 'message-mode-hook 'org-bullets-mode 'append)
  (add-hook 'message-mode-hook 'orgtbl-mode 'append)
  (add-hook 'message-mode-hook 'auto-complete-mode 'append)

  ;; org-mu4e end
  
  
  ;; These are required to view math properly.
  (use-package cdlatex
    :after (:any org-mode LaTeX-mode)
    :hook
    ((LaTeX-mode . turn-on-cdlatex)
     (org-mode . turn-on-org-cdlatex)))

  (use-package company-math
    :after (:any org-mode TeX-mode)
    :config
    (set-company-backend! 'org-mode 'company-math-symbols-latex)
    (set-company-backend! 'TeX-mode 'company-math-symbols-latex)
    (set-company-backend! 'org-mode 'company-latex-commands)
    (set-company-backend! 'TeX-mode 'company-latex-commands)
    (setq company-tooltip-align-annotations t)
    (setq company-math-allow-latex-symbols-in-faces t))

  ;; org-journal
  (use-package org-journal
        :bind
        ("C-c n j" . org-journal-new-entry)
        :custom
        (org-journal-dir "/data/www/org.git/journal/")
        (org-journal-date-prefix "#+TITLE: ")
        (org-journal-file-format "%Y-%m-%d.org")
        (org-journal-date-format "%A, %d %B %Y"))
      (setq org-journal-enable-agenda-integration t)

  ;;(load "~/.emacs.d/private/org-pandoc-import.el")
  ;;(load "~/.emacs.d/private/org-pandoc-import-transient.el")
 
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
        
  ;; Org-Mind-Map
  (load "~/.emacs.d/private/org-mind-map.el")
        
  ;; Super Agenda Groups
  ;;(org-super-agenda-mode t)
  (setq org-agenda-custom-commands
                           '(("t" "Tasks"
                              ((agenda ""
                                       ((org-agenda-files '("/data/www/org/gtd/tasks.org" "/data/www/org/gtd/tickler.org" "/data/www/org/gtd/projects.org"))
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
                                      (org-agenda-files '("/data/www/org.git/gtd/tasks.org"))
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
                                            '((:auto-ts t)))))))))

)

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/data/www/org.git/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; Eaf
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;; EWW

;; Emacs Web Wowser, the HTML-based Emacs Web Browser.
(use-package eww
  :ensure nil
  :commands (eww)
  :hook (eww-mode . (lambda ()
                      "Rename EWW's buffer so sites open in new page."
                      (rename-buffer "eww" t)))
  :config
  ;; I am using EAF-Browser instead of EWW
  
  (setq browse-url-browser-function 'eww-browse-url)) ; Hit & to browse url with system browser

;; Anki Editor
(use-package anki-editor
  :after org-noter
  :config
  ; I like making decks
  (setq anki-editor-create-decks 't))

;;(map! :localleader
;;      :map org-mode-map
;;      (:prefix ("a" . "Anki")
;;        :desc "Push" "p" 'anki-editor-push-notes
;;        :desc "Retry" "r" 'anki-editor-retry-failure-notes
;;        :desc "Insert" "n" 'anki-editor-insert-note
;;        (:prefix ("c" . "Cloze")
;;          :desc "Dwim" "d" 'anki-editor-cloze-dwim
;;          :desc "Region" "r" 'anki-editor-cloze-region
;;          )
;;        )
;; )

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

;; plantuml
(setq org-plantuml-jar-path (expand-file-name "/home/perrierjouet/plantuml.jar"))

;; ocaml
(add-to-list 'load-path "/www/perrierjouet/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; rmsbolt
(setq rmsbolt-disassemble t)

;; +bindings disabeled so I need actions for buttons on the dashboard
;;(map! "C-x C-r" #'recentf-open-files
;;      "C-x a" #'org-agenda
;;      "C-x p" #'doom/open-private-config
;;      "C-x C-p" #'projectile-switch-project
;;      "C-x C-L" #'doom/quickload-session)

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




)
 

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pg emacsql-psql sqlite3 org-caldav ox-leanpub pandoc org-mind-map org-roam zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color x86-lookup ws-butler writeroom-mode winum white-sand-theme which-key wgrep web-mode web-beautify vterm volatile-highlights vmd-mode vi-tilde-fringe vagrant-tramp vagrant uuidgen utop use-package unfill undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tuareg treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired toxi-theme toml-mode toc-org tern terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection sql-indent sphinx-doc spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slime-company slim-mode shell-pop seti-theme scss-mode scala-mode sbt-mode sass-mode ron-mode reverse-theme restart-emacs rebecca-theme ranger rainbow-delimiters railscasts-theme racket-mode racer pytest pyenv-mode py-isort purple-haze-theme pug-mode protobuf-mode proof-general professional-theme prettier-js popwin poetry play-crystal plantuml-mode planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-store-otp password-generator paradox pandoc-mode ox-pandoc overseer orgit organic-green-theme org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-brain opencl-mode open-junk-file omtose-phellack-theme oldlace-theme ocp-indent occidental-theme ocamlformat obsidian-theme ob-restclient ob-http ob-elixir ob-crystal nodejs-repl noctilux-theme nasm-mode naquadah-theme nameless mwim mvn mustang-theme multi-term mu4e-maildirs-extension mu4e-alert move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme merlin-eldoc meghanada maven-test-mode material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme lush-theme lsp-ui lsp-python-ms lsp-pyright lsp-origami lsp-metals lsp-latex lsp-julia lsp-java lsp-ivy lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme ledger-mode kaolin-themes julia-repl json-navigator js2-refactor js-doc jinja2-mode jbeans-theme jazz-theme ivy-yasnippet ivy-xref ivy-rtags ivy-purpose ivy-pass ivy-hydra ivy-avy ir-black-theme inkpot-theme inf-crystal indent-guide importmagic import-js impatient-mode idris-mode hybrid-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-make hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme groovy-mode groovy-imports grip-mode grandshell-theme gotham-theme google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot glsl-mode gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ gist gh-md geiser gandalf-theme fuzzy forge font-lock+ flyspell-correct-ivy flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-ocaml flycheck-ledger flycheck-haskell flycheck-elsa flycheck-crystal flycheck-credo flx-ido flatui-theme flatland-theme fasd farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme ewal-doom-themes evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu ess-R-data-view espresso-theme eshell-z eshell-prompt-extras esh-help es-mode erlang erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emr emmet-mode elisp-slime-nav ein editorconfig ediprolog dune dumb-jump dracula-theme dotenv-mode dockerfile-mode docker django-theme disaster dired-quick-sort diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dante dakrone-theme cython-mode cyberpunk-theme cuda-mode csv-mode cpp-auto-include counsel-projectile counsel-css conda company-ycmd company-web company-terraform company-rtags company-restclient company-reftex company-go company-glsl company-coq company-cabal company-c-headers company-auctex company-ansible company-anaconda common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmm-mode clues-theme clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider chocolate-theme cherry-blossom-theme centered-cursor-mode ccls cargo busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk attrap apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme ameba alect-themes alchemist aggressive-indent afternoon-theme adoc-mode ace-link ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
