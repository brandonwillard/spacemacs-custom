;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. Available distributions are `spacemacs-base'
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(html
     markdown
     javascript
     latex
     bibtex
     (ess :variables
          ess-disable-underscore-assign t
          :packages (not ess-R-object-popup))
     (python :variables
             python-test-runner 'pytest
             python-auto-set-local-pyvenv-virtualenv 'on-project-switch
             :packages (not live-py-mode)
             )
     yaml
     sql
     noweb
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode
            ;; company-c-headers-path-user '("../include" "./include" "." "../../include" "../inc" "../../inc")
            )
     helm
     ;; (helm :variables helm-enable-auto-resize t)
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence "C-y"
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual
                      ;; auto-completion-enable-sort-by-usage t
                      ;; :packages (not auto-complete ac-ispell)
                      )
     emacs-lisp
     git
     scheme
     (org :variables
          org-enable-github-support t
          org-projectile-file "TODOs.org")
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;; FIXME: We get `semantic-idle-scheduler-function' errors in `polymode' modes.
     (semantic :enabled-for emacs-lisp common-lisp)
     common-lisp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; elisp file manipulation library
                                      f
                                      ;; elisp string manipulation library
                                      s
                                      ;; elisp list manipulation library
                                      dash
                                      dash-functional

                                      ;; embrace
                                      evil-embrace

                                      ob-ipython
                                      ob-clojure-literate
                                      ;; org-babel-clojure
                                      ;; org-mime
                                      ;; ob-async

                                      ox-jira
                                      ;; ox-confluence

                                      ;; conda

                                      dockerfile-mode

                                      editorconfig

                                      evil-text-object-python
                                      ;; TODO: Setup this package.
                                      evil-extra-operator
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
   dotspacemacs-elpa-https t
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
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
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
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-mode-line-theme 'vim-powerline

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Noto Mono"
                               :size 13.0
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
   dotspacemacs-emacs-leader-key (concat "C-" dotspacemacs-leader-key)
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key (concat "C-" dotspacemacs-major-mode-leader-key)
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
   dotspacemacs-large-file-size 5
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
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 100
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
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
   dotspacemacs-line-numbers '(:relative nil
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   org-mode
                                                   pdf-view-mode
                                                   text-mode
                                                   xwidget-webkit-mode
                               :size-limit-kb 1000)
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

   dotspacemacs-switch-to-buffer-prefers-purpose t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq load-path (append '("~/.spacemacs.d/") load-path))

  ;; Viper is loaded/installed automatically, but we want it disabled.
  (setq package-load-list '(all
                            (viper nil)
                            ))

  ;; Fix for anaconda env interaction with pyvenv.
  (setenv "WORKON_HOME" "~/apps/anaconda3/envs")

  (setq custom-file (concat user-emacs-directory "private/custom-settings.el"))
  (load custom-file)

  ;; (setq browse-url-browser-function 'eww-browse-url)
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq comment-empty-lines t)
  (setq evil-move-beyond-eol t)

  (require 'mode-local)
  (setq-mode-local text-mode scroll-margin 10)
  (setq-mode-local prog-mode scroll-margin 10)

  (setq-default sentence-end-double-space t)

  ;; XXX: This will stop completion with TAB (but also fix annoying noops when
  ;; attempting to indent lines).
  (setq tab-always-indent t)

  (setq compilation-scroll-output #'first-error)

  (with-eval-after-load 'bibtex

    (defun btw/find-project-bib-file (dir)
      "Finds an bib file within a projectile project."
      (let* ((project-root (ignore-errors (projectile-project-root)))
            (file-name (f-glob "src/tex/*.bib" project-root)))
        (when (f-exists? file-name)
          file-name)))

    (setq org-ref-pdf-directory "~/projects/papers/references"
          org-ref-bibliography-notes "~/projects/papers/references/notes.org"))

  (with-eval-after-load 'hideshow
    (setq hs-allow-nesting t)
    ;; Let's not lose the cursor position when folding.
    (advice-add 'hs-hide-block :around #'(lambda (oldfun &rest r)
                                           (save-excursion (apply oldfun r))))
    (advice-add 'hs-show-block :around #'(lambda (oldfun &rest r)
                                           (save-excursion (apply oldfun r))))
    ;; TODO The meaning of "z[r|m]" is "level-folding" in Vim, but `evil-commands' has
    ;; no notion of this.  For `hideshow' we can use `hs-show-level' and `hs-hide-level'
    ;; to better approximate level-folding, but we would still have to work that into
    ;; evil's framework via `evil-fold-list' (e.g. new level-folding properties--perhaps
    ;; with fall-backs, too).
    ;; (evil-global-set-key 'normal "zr" 'hs-show-level)
    (evil-global-set-key 'normal "zm" 'hs-hide-level))

  (evil-global-set-key 'normal "zR" 'evil-open-folds)
  (evil-global-set-key 'normal "zM" 'evil-close-folds)

  (defun btw/messages-auto-tail (&rest _)
    "Make *Messages* buffer auto-scroll to the end after each message.

From https://stackoverflow.com/a/37356659/3006474"
    (let* ((buf-name "*Messages*")
          ;; Create *Messages* buffer if it does not exist
          (buf (get-buffer-create buf-name)))
      ;; Activate this advice only if the point is _not_ in the *Messages* buffer
      ;; to begin with. This condition is required; otherwise you will not be
      ;; able to use `isearch' and other stuff within the *Messages* buffer as
      ;; the point will keep moving to the end of buffer :P
      (when (not (string= buf-name (buffer-name)))
        ;; Go to the end of buffer in all *Messages* buffer windows that are
        ;; *live* (`get-buffer-window-list' returns a list of only live windows).
        (dolist (win (get-buffer-window-list buf-name nil :all-frames))
          (with-selected-window win
            (goto-char (point-max))))
        ;; Go to the end of the *Messages* buffer even if it is not in one of
        ;; the live windows.
        (with-current-buffer buf
          (goto-char (point-max))))))

  (advice-add 'message :after #'btw/messages-auto-tail)

  (with-eval-after-load 'persp-mode
    ;; Add variables containing functions to be called after layout changes.
    (defvar after-switch-to-buffer-functions nil)
    (defvar after-display-buffer-functions nil)
    (progn
      (defun after-switch-to-buffer-adv (&rest r)
        (apply #'run-hook-with-args 'after-switch-to-buffer-functions r))
      (defun after-display-buffer-adv (&rest r)
        (apply #'run-hook-with-args 'after-display-buffer-functions r))
      (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
      (advice-add #'display-buffer   :after #'after-display-buffer-adv))

    ;; Manually add certain buffers to the perspective
    ;; (add-hook 'after-switch-to-buffer-functions
    ;;           #'(lambda (bn) (when (and persp-mode
    ;;                                     (not persp-temporarily-display-buffer))
    ;;                            (persp-add-buffer bn))))

    ;; Add all opened buffers (filter certain ones below).
    (setq persp-add-buffer-on-after-change-major-mode t)
    ;; (add-hook 'persp-common-buffer-filter-functions
    ;;           ;; there is also `persp-add-buffer-on-after-change-major-mode-filter-functions'
    ;;           #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

    ;; Restore eshell buffers.
    ;; See https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-inferior-python-save-load-el
    ;; for more examples (e.g. Python inferior shells).
    (persp-def-buffer-save/load
     :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
     :save-vars '(major-mode default-directory))
    )

  (with-eval-after-load 'pyvenv-mode
    (defun btw/pyvenv-conda-activate-additions ()
      (setenv "CONDA_PREFIX" (string-remove-suffix "/" pyvenv-virtual-env))
      (setenv "CONDA_DEFAULT_ENV" pyvenv-virtual-env-name))

    (defun btw/pyvenv-conda-deactivate-additions ()
      (setenv "CONDA_PREFIX" nil)
      (setenv "CONDA_DEFAULT_ENV" nil))

    (add-hook 'pyvenv-post-activate-hooks #'btw/pyvenv-conda-activate-additions)
    (add-hook 'pyvenv-post-deactivate-hooks #'btw/pyvenv-conda-deactivate-additions)

    (defun btw/pyvenv-conda-env-shell-init (&rest process)
      "Activate the current env in a newly opened shell PROCESS.

From https://github.com/necaris/conda.el/blob/master/conda.el#L339"
      (let* ((activate-command (if (eq system-type 'windows-nt)
                                   '("activate")
                                 ;'("source" "activate")
                                 '("conda" "activate")
                                 ))
             (full-command (append activate-command `(,pyvenv-virtual-env-name "\n")))
             (command-string (combine-and-quote-strings full-command))
             (buffer-or-process (if (not process)
                                    (current-buffer)
                                  process)))
        (progn (message "sending %s to %S" command-string buffer-or-process)
               (term-send-string buffer-or-process command-string))))

    (add-hook 'term-exec-hook #'btw/pyvenv-conda-env-shell-init))

  (with-eval-after-load 'comint
    ;; Make terminals and REPLs read-only.
    ;; https://emacs.stackexchange.com/a/2897
    (setq-default comint-prompt-read-only t)
    (setq-default comint-use-prompt-regexp nil)
    (setq-default inhibit-field-text-motion nil)

    (defun btw/comint-preoutput-turn-buffer-read-only (text)
      (propertize text 'read-only t))

    (add-hook 'comint-output-filter-functions
              #'btw/comint-preoutput-turn-buffer-read-only
              'append))

  (use-package evil-embrace
    :config
    (progn
      (add-hook 'org-mode-hook 'embrace-org-mode-hook)
      (evil-embrace-enable-evil-surround-integration)))

  (use-package dockerfile-mode
    :mode ("Dockerfile\\'" . dockerfile-mode))

  ;; Set conda env based on editorconfig settings.
  (use-package editorconfig
    :ensure t
    :init
    (progn
      (editorconfig-mode 1)

      (defun btw/editorconfig-set-pyvenv (props)
        "Set Anaconda virtual env from entry in editorconfig file.
The config file entry should be the env name, and `pyenv-workon-home' should be
set."
        (let ((env-name (gethash 'conda_env_name props)))
          ;; `pyvenv-workon' seems slow, so only set the bare minimum when
          ;; the mode isn't python-specific.
          (when env-name
            (cond
             ((and env-name (bound-and-true-p python-mode))
              (progn (message "editorconfig setting pyvenv: %s" env-name)
                     (pyvenv-workon env-name)))
             ((and (not (local-variable-p python-shell-virtualenv-root))
                   (getenv "WORKON_HOME"))
              (progn (message "editorconfig setting virtualenv-root") 
                     (require 'pyvenv)
                     (setq-local python-shell-virtualenv-root
                                 (f-join (getenv "WORKON_HOME") env-name))))))))

      (add-hook 'editorconfig-custom-hooks
                #'btw/editorconfig-set-pyvenv))
    )

  (with-eval-after-load 'evil
    (setq-default evil-move-cursor-back nil)
    (setq-default evil-escape-key-sequence nil)
    (setq-default evil-emacs-state-modes nil)
    (setq-default evil-insert-state-modes '(magit-popup-mode))
    (setq evil-kill-on-visual-paste nil)
    ;; (setq-default evil-motion-state-modes nil)

    (add-hook 'python-mode-hook 'evil-text-object-python-add-bindings)

    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'company
    (setq-default company-idle-delay nil)

    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous)

    ;; TODO: Move this to python layer (when it works).
    (defun btw/company-transform-python (candidates)
      "De-prioritize internal variables (i.e. '_blah') in completion list ordering.

See `company-transformers'.

From URL `https://emacs.stackexchange.com/a/12403'"
      (let ((deleted))
        (mapcar #'(lambda (c)
                    (if (or (string-prefix-p "_" c) (string-prefix-p "._" c))
                        (progn
                          (add-to-list 'deleted c)
                          (setq candidates (delete c candidates)))))
                candidates)
        (append candidates (nreverse deleted))))

    (defun btw/python-company-conf()
      (setq-local company-transformers
                  (append company-transformers '(btw/company-transform-python))))

    (add-hook 'python-mode-hook 'btw/python-company-conf t))

  (with-eval-after-load 'helm
    (setq-default helm-follow-mode-persistent t)
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
    )

  (with-eval-after-load 'evil-jumps
    (setq evil-jumps-cross-buffers nil))

  (with-eval-after-load 'org

    (require 'ox-jira)
    ;; (require 'ox-confluence)

    (setq org-default-notes-file
          (f-join user-home-directory "Documents" "notes.org"))

    (defun btw/append-to-list (list other-list &rest append)
      (dolist (it other-list)
        (add-to-list list it append)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (C . t)
       (R . t)
       (sql . t)
       (shell . t)
       (scheme . t)
       (ipython . t)
       (python . t)
       (clojure . t)))

    ;; (btw/append-to-list
    ;;  'org-babel-load-languages
    ;;  '((emacs-lisp . t) (C . t) (ipython . t)))

    (setq org-babel-clojure-backend 'cider)

    (setq org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-window-setup 'current-window
          org-src-fontify-natively t
          org-confirm-babel-evaluate nil
          org-support-shift-select 'always)

    ;; Just in case we want to use the vanilla python babel...
    (setq org-babel-python-command (python-shell-calculate-command))

    (defun btw/org-babel-python-session-buffer (orig-func session)
      "Set org-babel's default python session buffer naming to follow python-mode's."
      (if (eq session :default)
          (format "*%s*" (python-shell-get-process-name nil))
        (funcall orig-func session)))

    (advice-add 'org-babel-python-session-buffer :around
                #'btw/org-babel-python-session-buffer)

    (defun btw/org-export-output-project-file-name (orig-fun extension &optional subtreep pub-dir)
      "Export to a project's corresponding source directory as determined by EXTENSION."
      (let* ((proj-root (projectile-project-root))
             (lang-src-dir (f-join proj-root "src" (s-chop-prefix "." extension)))
             (pub-dir (or pub-dir
                          (and (f-exists? lang-src-dir) lang-src-dir))))
        (message "%s" pub-dir)
        (apply orig-fun extension subtreep (list pub-dir)))
      )

    (advice-add 'org-export-output-file-name :around #'btw/org-export-output-project-file-name)

    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (let ((existing-todos (-filter 'f-exists-p (org-projectile-todo-files))))
      (setq org-agenda-files (append org-agenda-files existing-todos))))

  (with-eval-after-load 'flycheck

    ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

    (defun btw/flycheck-virtualenv-executable-find (executable &rest find-any)
      "Find an EXECUTABLE in the current virtualenv (exclusively) if any."
      (if (bound-and-true-p python-mode)
          (if (bound-and-true-p python-shell-virtualenv-root)
              (let ((exec-path (nth 0 (python-shell-calculate-exec-path))))
                (executable-find executable))
            (when find-any
              (executable-find executable)))
        (executable-find executable)))

    (setq flycheck-executable-find #'btw/flycheck-virtualenv-executable-find)

    (add-hook 'python-mode-hook #'(lambda () (add-to-list 'flycheck-disabled-checkers 'python-pylint)))
    )

  (with-eval-after-load 'python
    ;;; See https://github.com/kaz-yos/eval-in-repl/blob/master/eval-in-repl-python.el
    ;;; for some interesting ideas.

    ;; TODO: In the python layer, `spacemacs/python-toggle-breakpoint' should
    ;; take an `alist' in the `cond' statement; that way, people could add their
    ;; own debuggers via custom variable like this
    ;;(defcustom spacemacs/python-breakpoints '((wdb . "import wdb; wdb.set_trace()")
    ;;                                          (IPython . "from IPython.core.debugger import set_trace; set_trace()"))
    ;;  :group 'python
    ;;  :type '(alist :tag "Backing executable"
    ;;                :key-type
    ;;                (choice
    ;;                 (const :tag "wdb" wdb)
    ;;                 (const :tag "IPython" wdb)
    ;;                 (const :tag "ipdb" ipdb))
    ;;                :value-type (string :tag "Set trace expression")))
    ;; One could then use `alist-get' defaulting to "pdb", as in the existing `cond'.
    ;; from IPython.core.debugger import set_trace; set_trace()

    ;; Stop python from complaining when opening a REPL
    ;; (setq python-shell-completion-native-disabled-interpreters
    ;;       (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython"))
    (setq python-shell-completion-native-output-timeout 3.0)
    (setq python-pdbtrack-activate nil)

    ;; FYI: Could just use a python-mode hook that sets `python-shell-buffer-name'.
    (defun btw/python-shell-get-process-name (orig-func dedicated)
      "Append project name to shell process name.
       Makes shells specific to the active project."
        (let ((proc-name (funcall orig-func dedicated))
              (proj-name (ignore-errors (projectile-project-name))))
          (if (and proj-name
                   (not (s-suffix? (format "(%s)" proj-name) proc-name)))
              (format "%s(%s)" proc-name proj-name)
            proc-name)))

    (advice-add 'python-shell-get-process-name :around
                #'btw/python-shell-get-process-name)

    (defun btw/python-shell-append-to-output (string)
      "Append STRING to `comint' display output."
      (let ((buffer (current-buffer))
            (py-buffer (process-buffer (python-shell-get-process))))
        (unless (eq buffer py-buffer)
          (save-mark-and-excursion
            (with-current-buffer py-buffer
              (let ((oldpoint (point)))
                (goto-char (process-mark (python-shell-get-process)))
                ;; (insert (propertize string 'read-only t))
                (insert string)
                (set-marker (process-mark (python-shell-get-process)) (point))
                (goto-char oldpoint))
              ))
          )))

    (defun python-shell-send-string-echo (string &optional process msg)
      (btw/python-shell-append-to-output string)
      (python-shell-send-string string process msg))

    (defun python-shell-send-line-echo ()
      "Send and echo a literal line to the `comint' buffer.
Ignores beginning white-space."
      (interactive)
      (let (start end line)
        (save-excursion
          (end-of-line)
          ;; or `forward-line'?
          (setq end (point))
          (beginning-of-line-text)
          (setq start (point)))
        (setq line (buffer-substring-no-properties start end))
        (python-shell-send-string-echo (if (s-ends-with? "\n" line)
                                           line
                                           (concat line "\n")))))

    (defun btw/python-shell-send-syntax-line-echo ()
      "Send and echo a \"syntactical\" line to the `comint' buffer."
      (interactive)
      (let (start end line)
        (save-excursion
          (python-nav-end-of-statement)
          (setq end (point))
          (python-nav-beginning-of-statement)
          (setq start (point)))
        (setq line (buffer-substring-no-properties start end))
        (python-shell-send-string-echo line)))

    (defun python-shell-send-region-echo (start end &optional send-main msg)
      (interactive
       (list (region-beginning) (region-end) current-prefix-arg t))
      (btw/python-shell-append-to-output (buffer-substring-no-properties start end))
      (python-shell-send-region start end send-main msg))

    (spacemacs/set-leader-keys-for-major-mode 'python-mode
       "sr" 'python-shell-send-region-echo
       "sl" 'python-shell-send-line-echo)
    )

  (defun btw/clang-format-bindings ()
    (define-key c++-mode-map [tab] 'clang-format-buffer)
    (global-set-key [C-M-tab] 'clang-format-region))

  (add-hook 'c++-mode-hook 'btw/clang-format-bindings)

  (defun btw/setup-term-mode ()
    "From https://github.com/syl20bnr/spacemacs/issues/2345"
    (evil-local-set-key 'insert (kbd "C-r") 'btw/send-C-r))

  (defun btw/send-C-r ()
    (interactive)
    (term-send-raw-string "\C-r"))

  (add-hook 'term-mode-hook
            (function
             (lambda ()
               ;; (setq-local fringe-mode nil)
               ;; (setq-local fringes-outside-margins t)
               ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
               ;; (setq-local mouse-yank-at-point t)
               ;; (setq-local transient-mark-mode nil)
               (auto-fill-mode -1)
               ;; (setq tab-width 8 )
               )))

  (add-hook 'term-mode-hook 'btw/setup-term-mode)

  (defun btw/term-handle-more-ansi-escapes (proc char)
    "Handle additional ansi escapes.
From https://emacs.stackexchange.com/a/10698"
    (cond
     ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
     ((eq char ?G)
      (let ((col (min term-width (max 0 term-terminal-parameter))))
        (term-move-columns (- col (term-current-column)))))
     (t)))

  (advice-add 'term-handle-ansi-escape :before #'btw/term-handle-more-ansi-escapes)
  )
