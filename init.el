;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For noweb-packages available distributions are `spacemacs-base'
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
   '(
     html
     markdown
     javascript
     latex
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

                                      ob-ipython
                                      ;; org-mime
                                      ;; org-jira

                                      conda

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
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Noto Mono"
                               :size 16.0
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
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 100
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
   dotspacemacs-line-numbers '(:size-limit-kb 1000)
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
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq load-path (append '("~/.spacemacs.d/") load-path))

  ;; TODO: Does this actually work?
  ;; Viper is loaded/installed automatically, but we want it disabled.
  (setq package-load-list '(all
                            (viper nil)
                            ))

  ;; Fix for anaconda env interaction with pyvenv.
  (setenv "WORKON_HOME" "~/apps/anaconda3/envs")

  (setq custom-file (concat user-emacs-directory "private/custom-settings.el"))
  (load custom-file)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq-default scroll-margin 10)
  (setq-default sentence-end-double-space t)

  (setq compilation-scroll-output t)
  (setq compilation-scroll-output #'first-error)

  (defun bw/messages-auto-tail (&rest _)
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

  (advice-add 'message :after #'bw/messages-auto-tail)

  (with-eval-after-load "persp-mode"
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
    ;; Restore eshell buffers.  See https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-inferior-python-save-load-el
    ;; for more examples (e.g. Python inferior shells).
    (persp-def-buffer-save/load
     :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
     :save-vars '(major-mode default-directory)))

  (with-eval-after-load 'pyvenv-mode
    (defun bw/pyvenv-conda-activate-additions ()
      (setenv "CONDA_PREFIX" (string-remove-suffix "/" pyvenv-virtual-env))
      (setenv "CONDA_DEFAULT_ENV" pyvenv-virtual-env-name))

    (defun bw/pyvenv-conda-deactivate-additions ()
      (setenv "CONDA_PREFIX" nil)
      (setenv "CONDA_DEFAULT_ENV" nil))

    (add-hook 'pyvenv-post-activate-hooks #'bw/pyvenv-conda-activate-additions)
    (add-hook 'pyvenv-post-deactivate-hooks #'bw/pyvenv-conda-deactivate-additions)

    (defun bw/pyvenv-conda-env-shell-init (&rest process)
      "Activate the current env in a newly opened shell PROCESS.

From https://github.com/necaris/conda.el/blob/master/conda.el#L339"
      (let* ((activate-command (if (eq system-type 'windows-nt)
                                   '("activate")
                                 '("source" "activate")))
             (full-command (append activate-command `(,pyvenv-virtual-env-name "\n")))
             (command-string (combine-and-quote-strings full-command))
             (buffer-or-process (if (not process)
                                    (current-buffer)
                                  process)))
        (progn (message "sending %s to %S" command-string buffer-or-process)
               (term-send-string buffer-or-process command-string))))

    (add-hook 'term-exec-hook #'bw/pyvenv-conda-env-shell-init))

  ;; (use-package conda
;;     :defer t
;;     :init
;;     (progn
;;       (custom-set-variables '(conda-anaconda-home "~/apps/anaconda3")
;;                             '(conda-message-on-environment-switch nil))

;;       (conda-env-initialize-interactive-shells)
;;       (conda-env-initialize-eshell)

;;       (defun bw/conda--get-name-from-env-yml (filename)
;;         "Pull the `name` property out of the YAML file at FILENAME."
;;         (when filename
;;           (let ((env-yml-contents (f-read-text filename)))
;;             ;; We generalized the regex to include `-`.
;;             (if (string-match "name:[ ]*\\([[:word:]-]+\\)[ ]*$" env-yml-contents)
;;                 (match-string 1 env-yml-contents)
;;               nil))))

;;       ;; Could've just overriden this package's function, but Emacs' advice functionality
;;       ;; covers this explicit case *and* make it clear via the help/documentation that the
;;       ;; function has been changed.
;;       (advice-add 'conda--get-name-from-env-yml :override #'bw/conda--get-name-from-env-yml)

;;       (defun bw/conda--find-project-env (dir)
;;         "Finds an env yml file for a projectile project.
;; Defers to standard `conda--find-env-yml' otherwise."
;;         (let* ((project-root (ignore-errors (projectile-project-root)))
;;               (file-name (f-expand "environment.yml" project-root)))
;;           (when (f-exists? file-name)
;;             file-name)))

;;       ;; Avoid unnecessary searches by using *only* a project-centric environment.yml file.
;;       ;; To fallback on an upward directory search, use `:before-until'.
;;       (advice-add 'conda--find-env-yml :override #'bw/conda--find-project-env)

;;       ;; Since `editorconfig-custom-hooks' activates a discovered conda env, and `conda'
;;       ;; sets the buffer-local variable `conda-project-env-name', the env should be found
;;       ;; by `conda-env-autoactivate-mode' (because it checks that variable).
;;       (conda-env-autoactivate-mode)

;;       ;; TODO: Check `window-purpose' for "edit", "general", etc.  Could also use `post-command-hook'
;;       ;; (see the comment about using `(while-no-input (redisplay) CODE)')
;;       ;; This is what auto-activates conda environments after switching layouts:
;;       (advice-add 'select-window :after #'conda--switch-buffer-auto-activate)
;;       ))

  ;; (with-eval-after-load 'spaceline
  ;;   ;; Hijacks existing segment.  Should add cases for both envs.
  ;;   (spaceline-define-segment python-pyenv
  ;;     "The current python env.  Works with `conda'."
  ;;     (when (and active
  ;;                ;; TODO: Consider not restricting to `python-mode', because
  ;;                ;; conda envs can apply to more than just python operations
  ;;                ;; (e.g. libraries, executables).
  ;;                ;; (eq 'python-mode major-mode)
  ;;                ;; TODO: Display `conda-project-env-name' instead?  It's buffer-local.
  ;;                (boundp 'conda-env-current-name)
  ;;                (stringp conda-env-current-name))
  ;;       (propertize conda-env-current-name
  ;;                   'face 'spaceline-python-venv
  ;;                   'help-echo "Virtual environment (via conda)")))
  ;;   (spaceline-compile))

  ;; Make terminals and REPLs read-only.
  ;; https://emacs.stackexchange.com/a/2897

  (with-eval-after-load 'comint
    (setq-default comint-prompt-read-only t)
    (setq-default comint-use-prompt-regexp nil)
    (setq-default inhibit-field-text-motion nil)

    (defun btw/comint-preoutput-turn-buffer-read-only (text)
      (propertize text 'read-only t))

    (add-hook 'comint-output-filter-functions
              #'btw/comint-preoutput-turn-buffer-read-only
              'append))

  ;; Set conda env based on editorconfig settings.
  (use-package editorconfig
    :ensure t
    :config
    (progn
      (editorconfig-mode 1)
      (add-hook 'editorconfig-custom-hooks
                '(lambda (props)
                   (progn
                     (let ((env-name (gethash 'conda_env_name props)))
                       (when env-name
                         (pyvenv-workon env-name)))))))
    )

  (with-eval-after-load 'evil
    (setq-default evil-move-cursor-back nil)
    (setq-default evil-escape-key-sequence nil)
    (setq-default evil-emacs-state-modes nil)
    (setq-default evil-insert-state-modes '(magit-popup-mode))
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
    (setq org-default-notes-file
          (f-join user-home-directory "Documents" "notes.org"))

    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages '((C . t) (ipython . t))))

    (setq org-confirm-babel-evaluate nil)
    (setq org-src-window-setup 'current-window)

    ;; TODO: Configure org-mode export options based on projectile variables
    ;; (i.e. `projectile-project-root').
    ;; org-export-latex-options-plist

    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (push (org-projectile-todo-files) org-agenda-files))

  ;; (use-package python-x
  ;;   :defer t
  ;;   ;; :commands
  ;;   ;; (python-shell-send-line python-shell-print-region-or-symbol)
  ;;   :init
  ;;   (progn
  ;;     (evil-leader/set-key-for-mode 'python-mode
  ;;       "sl" 'python-shell-send-line)
  ;;     (evil-leader/set-key-for-mode 'python-mode
  ;;       "sw" 'python-shell-print-region-or-symbol))
  ;;   ))

  (with-eval-after-load 'python
    ;;; See https://github.com/kaz-yos/eval-in-repl/blob/master/eval-in-repl-python.el
    ;;; for some interesting ideas.

    ;; Stop python from complaining when opening a REPL
    ;; (setq python-shell-completion-native-disabled-interpreters
    ;;       (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython"))
    (setq python-shell-completion-native-output-timeout 3.0)
    (setq python-pdbtrack-activate nil)

    (defun flycheck-virtualenv-executable-find (executable)
      "Find an EXECUTABLE in the current virtualenv if any."
      (if (bound-and-true-p python-shell-virtualenv-root)
          (let ((exec-path (python-shell-calculate-exec-path)))
            (executable-find executable))
        (executable-find executable)))

    (defun flycheck-virtualenv-setup ()
      "Setup Flycheck for the current virtualenv."
      (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

    (add-hook 'python-mode-hook #'flycheck-virtualenv-setup)

    (defun python-shell-append-to-output (string)
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
      (python-shell-append-to-output string)
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

    (defun python-shell-send-syntax-line-echo ()
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
      (python-shell-append-to-output (buffer-substring-no-properties start end))
      (python-shell-send-region start end send-main msg))

    (spacemacs/set-leader-keys-for-major-mode 'python-mode
       "sr" 'python-shell-send-region-echo
       "sl" 'python-shell-send-line-echo)
    )

  (defun btw/clang-format-bindings ()
    (define-key c++-mode-map [tab] 'clang-format-buffer)
    (global-set-key [C-M-tab] 'clang-format-region))

  (add-hook 'c++-mode-hook 'btw/clang-format-bindings)

  ;; (defun btw/tex-mode-settings ()
  ;;   (setq latex-directory "")
  ;;   (setq latex-run-command ""))
  ;; (add-hook 'tex-mode-hook 'btw/tex-mode-settings)

  ;; From https://github.com/syl20bnr/spacemacs/issues/2345
  (defun btw/setup-term-mode ()
    (evil-local-set-key 'insert (kbd "C-r") 'btw/send-C-r))

  (defun btw/send-C-r ()
    (interactive)
    (term-send-raw-string "\C-r"))

  (add-hook 'term-mode-hook 'btw/setup-term-mode)
  )
