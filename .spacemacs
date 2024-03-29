;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;111; o#474957r `spacemacs'. (default 'spacemacs)
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     rust
     octave
     go
     ruby
     html
     csv
     latex
     pdf-tools
     ;; vimscript
     javascript
     ;; octave
     graphviz
     (python :variables
             python-enable-yapf-format-on-save t
             python-sort-imports-on-save t
     )
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     mew
     mozc
     prettier-js
     elscreen
     recentf-ext
     all-the-icons
     restart-emacs
     jedi
     magit
     pangu-spacing
     markdown-preview-mode
     ;; etherpad
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   ;; dotspacemacs-editing-style 'hybrid
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings nil
                                       hybrid-mode-default-state 'insert)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 20
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (add-hook 'python-mode-hook 'jedi:setup)

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."


  ;; delete backward char
  (keyboard-translate ?\C-h ?\C-?)


  ;; window layout toggle
  (global-set-key (kbd "C-c E") 'spacemacs/window-layout-toggle)


  ;; load path
  (setq load-path
        (append (list nil
                      (expand-file-name "~/.emacs.d/lisp"))
                load-path))


  ;; no backup file
  (setq make-backup-files nil) ;; *.~
  (setq auto-save-default nil) ;; .*#
  (setq delete-auto-save-files t)


  ;; Japanese font
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic"))


  ;; add space between Japanese and English characters
  ;; (global-pangu-spacing-mode 1)
  ;; (setq pangu-spacing-real-insert-separtor t)
  ;; (add-hook 'text-mode-hook 'pangu-spacing-mode)


  ;; follow symbolic link
  (setq vc-follow-symlinks t)


  ;; browser
  (setq browse-url-browser-function 'browse-url-chromium)


  ;; chmod after save
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


  ;; org to pdf
  (setq org-latex-pdf-process
        '("uplatex %b.tex" "uplatex %b.tex" "dvipdfmx %b.dvi" "rm -f %b.dvi %b.tex"))


  ;; open file after export
  (eval-after-load "org"
    '(progn
       (delete '("\\.pdf\\'" . default) org-file-apps)
       (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s")))
    )


  ;; pdf-tools
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))


  ;; mew
  ;; (setq mew-prog-ssl "/usr/bin/stunnel")
  (global-set-key "\C-xm" 'mew)
  (setq mew-imap-size 100000000) ;; 100MB
  (setq mew-use-cached-passwd t)
  (setq mew-signature-insert-last t)
  (setq mew-save-dir "~/tmp/mail")
  (setq mew-use-suffix t)
  (defun mew-summary-form-dow ()
    (let ((s (MEW-DATE)))
      (if (>= (length s) 3) (setq s (substring s 0 3)))
      (cond
       ((string= s "Mon") "月")
       ((string= s "Tue") "火")
       ((string= s "Wed") "水")
       ((string= s "Thu") "木")
       ((string= s "Fri") "金")
       ((string= s "Sat") "土")
       ((string= s "Sun") "日")
       (t "？"))))
  (setq mew-summary-form
        '(type (5 date) "(" (2 dow) ")" (5 time) " | " (15 from) " | " t (0 body)))
  (setq mew-summary-form-extract-rule '(address))
  ;; mew-summary-execute-external (C-c C-e)
  (setq mew-prog-pdf-ext "mupdf")
  (setq mew-prog-image/*-ext "mupdf")
  (setq mew-prog-postscript "gv")
  (setq mew-prog-msword-ext "xdg-open")
  (setq mew-prog-msexcel-ext "xdg-open")
  (setq mew-prog-mspowerpoint-ext "xdg-open")


  ;; recentf
  (require 'recentf-ext)
  (global-set-key "\M-r" 'helm-recentf)


  ;; evil
  (define-key evil-emacs-state-map (kbd "C-z") nil)


  ;; elscreen
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-display-tab nil)
  (elscreen-start)
  (set-face-background 'elscreen-tab-current-screen-face "#292B2E")
  (set-face-foreground 'elscreen-tab-current-screen-face "#BC6EC5")
  (define-key elscreen-map "\M-k"    'elscreen-kill)
  (define-key elscreen-map "k" 'elscreen-kill-screen-and-buffers)


  ;; elscreen tab
  ;; get-alist was removed somewhere along the line
  ;; You can try substituting all instances of get-alist with assoc-default
  ;; instead of using defalias and see if that works; I haven't tried.
  (defalias 'get-alist 'assoc-default) ; get-alist is gone

  ;; Put tabs display in your frame title bar instead.
  (defun elscreen-frame-title-update ()
    (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
             (screen-to-name-alist (elscreen-get-screen-to-name-alist))
             (title (concat "| " (mapconcat
                                  (lambda (screen)
                                    (format "%d%s %s |"
                                            screen (elscreen-status-label screen)
                                            (get-alist screen screen-to-name-alist)))
                                  screen-list " "))))
        (if (fboundp 'set-frame-name)
            (set-frame-name title)
          (setq frame-title-format title)))))

  (eval-after-load "elscreen"
    '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))


  ;; elscreen-helm-recenf
  (global-set-key "\C-z\M-r" 'elscreen-helm-recentf)
  (defun elscreen-helm-recentf ()
    (interactive)
    (elscreen-create)
    (helm-recentf))


  ;; elscreen-mew
  (global-set-key "\C-zm" 'elscreen-mew)
  (defun elscreen-mew ()
    (interactive)
    (elscreen-create)
    (mew))


  ;; lookup
  (setq lookup-enable-splash nil)
  (autoload 'lookup "lookup" nil t)
  (autoload 'lookup-region "lookup" nil t)
  (autoload 'lookup-pattern "lookup" nil t)
  (global-set-key "\C-c\C-l" 'lookup)
  (global-set-key "\C-cy" 'lookup-region)
  (global-set-key "\C-c\C-y" 'lookup-pattern)
  (setq lookup-default-dictionary-options
        '((:stemmer .  stem-english)))
  (setq lookup-use-kakasi nil)
  (setq lookup-init-file "~/.lookup.el")


  ;; multiple cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


  ;; neotree
  (require 'neotree)
  (global-set-key "\C-xn" 'neotree-toggle)
  ;; all-the-icons
  (require 'all-the-icons)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))


  ;; auto insert
  (require 'autoinsert)
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-query nil)
  (setq auto-insert-alist nil)
  (setq auto-insert-alist
        (append '(
                  (("\\.sh$" . "shell script template")
                   nil
                   "#!/bin/sh\n"
                   "\n"
                   _
                   )) auto-insert-alist))
  (setq auto-insert-alist
        (append '(
                  (("\\.py$" . "python template")
                   nil
                   "#!/usr/bin/env python3\n"
                   "\n"
                   _
                   )) auto-insert-alist))
  (setq auto-insert-alist
        (append '(
                  (("\\.rb$" . "ruby template")
                   nil
                   "#!/usr/bin/env ruby\n"
                   "\n"
                   _
                   )) auto-insert-alist))


  ;; restart emacs
  (global-set-key "\C-c\C-r\C-r" 'restart-emacs)


  ;; dired list
  (setq dired-listing-switches "-Alh --group-directories-first")


  ;; dired keybinding
  (define-key dired-mode-map (kbd "C-l") 'dired-up-directory)


  ;; open file in external application
  (define-key dired-mode-map (kbd "RET") 'dired-open-file)
  (setq dired-file-apps
        '(("pdf" . "mupdf")
          ("dvi" . "xdvi")
          ("eps" . "gv")
          ("jpg" . "iv")
          ("xls" . "libreoffice")))

  (defun dired-open-file ()
    (interactive)
    (let* ((find-file-run-dired t)
           (file (dired-get-file-for-visit))
           (ext (file-name-extension file))
           (cmd (cdr (assoc ext dired-file-apps))))
      (if cmd
          (start-process cmd nil cmd file)
        ;; call find-file as default action
        (find-file file))))


  ;; dired
  (global-set-key "\C-xd" (lambda ()
                            (interactive)
                            (dired default-directory)))


  ;; elscreen + zsh
  ;; https://uwabami.github.io/cc-env/Emacs.html
  (defun return-current-working-directory-to-shell ()
    (expand-file-name
     (with-current-buffer
         (if (featurep 'elscreen)
             (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
                    (num (nth 1 (assoc 'screen-history frame-confs)))
                    (cur-window-conf
                     (assoc 'window-configuration
                            (assoc num (assoc 'screen-property frame-confs))))
                    (marker (nth 2 cur-window-conf)))
               (marker-buffer marker))
           (nth 1
                (assoc 'buffer-list
                       (nth 1 (nth 1 (current-frame-configuration))))))
       default-directory)))


  ;; doc-annotate
  (setq doc-view-scale-internally nil)
  (add-hook 'doc-view-mode-hook
            '(lambda ()
               (local-set-key "c" 'doc-annotate-add-annotation)
               (local-set-key [mouse-1] 'doc-annotate-add-annotation)))
  (autoload 'doc-annotate-mode "doc-annotate")
  (autoload 'doc-annotate-add-annotation "doc-annotate")
  (add-to-list 'auto-mode-alist '("\\.ant$" . doc-annotate-mode))


  ;; insert-comment
  (global-set-key "\C-cD" 'insert-modification-notice)
  (defun insert-modification-notice ()
    "Insert today's date followed by your full name at the current point
  as a comment."
    (interactive)
    (cond ((or (eq major-mode 'latex-mode)
               (eq major-mode 'outline-mode))
           (save-excursion
             (insert (format "%%  -%s [" (user-login-name))
                     (format-time-string "%Y/%m/%d")
                     "]\n"))
           (forward-char 2))
          (t
           (insert (format "%s%s, %s by %s%s"
                           (comment-start-with-space)
                           (substring (current-time-string) 4 10)
                           (substring (current-time-string) -4)
                           (user-full-name)
                           (or comment-end "")))
           (indent-according-to-mode))))


  ;; markdown preview mode
  (setq markdown-preview-stylesheets 
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))

  ;; stop watch
  (require 'stopwatch)

  ;; math-mode
  ;; (require 'math-mode)

  ;; zf-mode
  ;; (require 'zf)
  ;; (zf-mode 1)

  ;; etherpad
  ;; (use-package etherpad
  ;;   :config (setq etherpad-server "http://localhost:9001"
  ;;                 etherpad-apikey "c72c78943d580dea8eff39dc6e25a922bd846ff4f65efe9c0e0607c4e42648a7"
  ;;                 etherpad-autosync nil)
  ;;   :bind (("C-c e" . etherpad-edit)
  ;;          :map etherpad-mode-map
  ;;          ("C-c c" . etherpad-save))
  ;;   )

  ;; format-paragraph
  (autoload 'format-paragraph "format-paragraph" nil t)
  (global-set-key "\M-p" 'format-paragraph)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (etherpad 0xc parsec toml-mode racer rust-mode flycheck-rust cargo go-guru go-eldoc company-go go-mode markdown-preview-mode web-server websocket rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby pangu-spacing goto-chg bind-map bind-key packed mew undo-tree jedi jedi-core python-environment epc ctable concurrent deferred pdf-tools tablist company-anaconda anaconda-mode disaster company-c-headers cmake-mode clang-format elscreen-mew recentf-ext flyspell-correct-helm flyspell-correct auto-dictionary all-the-icons memoize async gnu-elpa-keyring-update vimrc-mode dactyl-mode company-auctex auctex-latexmk auctex powerline spinner parent-mode pkg-info epl flx highlight iedit anzu f s popup dash mozc hydra lv projectile avy smartparens evil helm helm-core prettier-js elscreen yapfify web-mode web-beautify tagedit slim-mode scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements livid-mode skewer-mode simple-httpd live-py-mode less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc hy-mode helm-pydoc helm-css-scss haml-mode graphviz-dot-mode emmet-mode cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-anac onda coffee-mode pythonic smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#212026" :foreground "salmon")))))
