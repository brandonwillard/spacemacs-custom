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
     markdown
     javascript
     latex
     python
     yaml
     sql
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode
            ;; company-c-headers-path-user '("../include" "./include" "." "../../include" "../inc" "../../inc")
            )
     helm
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence "C-y"
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-sort-by-usage t
                      :packages
                      (not auto-complete ac-ispell)
                      )
     emacs-lisp
     git
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;; FIXME: We get `semantic-idle-scheduler-function' errors in `polymode' modes.
     ;; (semantic :enabled-for emacs-lisp common-lisp)
     common-lisp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(python-x
                                      polymode
                                      ob-ipython
                                      conda
                                      ;; org-mime
                                      ;; org-jira
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
   dotspacemacs-scratch-mode 'lisp-interaction-mode
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
   dotspacemacs-auto-resume-layouts nil
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
   dotspacemacs-line-numbers t 
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

  (setq scroll-margin 10)
  (setq sentence-end-double-space t)

  ;; TODO: Does this actually work?
  ;; Viper is loaded/installed automatically, but we want it disabled.
  (setq package-load-list '(all
                            (viper nil)
                            ))
  (setq evil-move-cursor-back nil)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Always start in normal state.
  (with-eval-after-load 'evil
    (setq evil-emacs-state-modes nil)
    (setq evil-insert-state-modes nil)
    (setq evil-motion-state-modes nil)
    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (setq compilation-scroll-output t)
  (setq compilation-scroll-output #'first-error)

  ;; Temp fix.
  ;; https://github.com/syl20bnr/spacemacs/issues/9549 
  (require 'helm-bookmark)

  (with-eval-after-load 'company
    (setq-default company-idle-delay nil)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous))

  (with-eval-after-load 'helm
    (setq-default helm-follow-mode-persistent t)
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
    )

  (setq load-path (append '("~/.spacemacs.d/") load-path))

  (with-eval-after-load 'org
    (require 'ob-python)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (python . t)))
    )

  (use-package conda
    :config
    (progn
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)

      (defun conda--get-name-from-env-yml (filename)
        "Pull the `name` property out of the YAML file at FILENAME."
        (when filename
          (let ((env-yml-contents (f-read-text filename)))
            ;; We generalized the regex to include `-`.
            (if (string-match "name:[ ]*\\([[:word:]-]+\\)[ ]*$" env-yml-contents)
                (match-string 1 env-yml-contents)
              nil))))

      (defun conda--env-activate-project (&optional warn-msg)
        (let* ((project-root (ignore-errors (projectile-project-root)))
               (env-file (conda--find-env-yml project-root))
               (env-name (conda--get-name-from-env-yml env-file)))
          (if (not env-name)
              (progn
                (when warn-msg
                  (message "No conda environment for project at %S: %S %S"
                           project-root env-file env-name))
                (conda-env-deactivate))
            (conda-env-activate env-name)))
        )
      (add-hook 'projectile-after-switch-project-hook 'conda--env-activate-project)
      ;; (add-hook 'projectile-before-switch-project-hook 'conda--env-activate-project)

      ;; TODO: Might need something like this, too.
      ;; (advice--add 'switch-to-buffer :after #'conda--env-activate-project)
      ;; TODO: Alternatively, we could wrap/advise the existing autoactivate-mode.
      ;; (advice--add 'conda--switch-buffer-auto-activate :after #'conda--env-activate-project)
      ;; (conda-env-autoactivate-mode t)

      (custom-set-variables
       '(conda-anaconda-home "~/apps/anaconda3"))

      ;; XXX: Hijacks existing segment.  Should add cases for both envs.
      (spaceline-define-segment python-pyenv
        "The current python env.  Works with `conda'."
        (when (and active
                   ;; TODO: Consider not restricting to `python-mode', because
                   ;; conda envs can apply to more than just python operations
                   ;; (e.g. libraries, executables).
                   ;; (eq 'python-mode major-mode)
                   (boundp 'conda-env-current-name)
                   (stringp conda-env-current-name)
                   )
          (propertize conda-env-current-name
                      'face 'spaceline-python-venv
                      'help-echo "Virtual environment (via conda)"))
        )
      (spaceline-compile)
     ))

  (with-eval-after-load 'python
    ;; Stop python from complaining when opening a REPL
    (setq-default python-shell-prompt-detect-failure-warning nil)

    (defun python-shell-append-to-output (string)
      (let ((buffer (current-buffer))
            (py-buffer (process-buffer (python-shell-get-process))))
        (unless (eq buffer py-buffer)
          (with-current-buffer py-buffer
            (let ((oldpoint (point)))
              (goto-char (process-mark (python-shell-get-process)))
              (insert string)
              (set-marker (process-mark (python-shell-get-process)) (point))
              (goto-char oldpoint))
            )
          )))

    (defadvice python-shell-send-string
        (around advice-python-shell-send-string activate)
      (interactive)
      (let* ((append-string1
              (if (string-match "import codecs, os;__pyfile = codecs.open.*$" string)
                  (replace-match "" nil nil string)
                string))
             (append-string2
              (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string1)
                  (replace-match "" nil nil append-string1)
                append-string1))
             (append-string
              (if (string-match "^\n*$" append-string2)
                  (replace-match "" nil nil append-string2)
                append-string2)))
        (python-shell-append-to-output
         (concat (string-trim-right append-string) "\n")))
      (if (called-interactively-p 'any)
          (call-interactively (ad-get-orig-definition 'python-shell-send-string))
        ad-do-it))
    )

  (use-package python-x
    :defer t
    :commands
    (python-shell-send-line
     python-shell-print-region-or-symbol)
    :init
    (progn
      (evil-leader/set-key-for-mode 'python-mode
        "sl" 'python-shell-send-line)
      (evil-leader/set-key-for-mode 'python-mode
        "sw" 'python-shell-print-region-or-symbol))
    )

  (use-package polymode
    :defer t
    :init
    (progn
      ;; `polymode-prefix-key` needs to be set *before* the package loads.
      (setq pm-debug-mode t)
      (setq polymode-prefix-key "\C-P"))
    :config
    (progn
      ;; TODO: Could try `use-package` again.  Perhaps as follows:
      ;; (use-package poly-python
      ;;   :commands (poly-noweb+python-mode)
      ;;   ;; :functions (poly-noweb+python-mode)
      ;;   )
      (require 'poly-python)

      ;; Evil settings.
      (evilified-state-evilify polymode-minor-mode polymode-mode-map)
      (evil-define-key 'normal polymode-mode-map "]c" 'polymode-next-chunk-same-type)
      (evil-define-key 'normal polymode-mode-map "]C" 'polymode-next-chunk)
      (evil-define-key 'normal polymode-mode-map "[c" 'polymode-previous-chunk-same-type)
      (evil-define-key 'normal polymode-mode-map "[C" 'polymode-previous-chunk)
      ;; See https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately
      (add-hook 'polymode-init-host-hook #'evil-normalize-keymaps)
      (add-hook 'polymode-init-inner-hook #'evil-normalize-keymaps)

      (add-hook 'polymode-init-inner-hook #'(lambda () (message "***********entered polymode-minor-mode (inner)!")))
      (add-hook 'polymode-init-host-hook #'(lambda () (message "***********entered polymode-minor-mode (host)!")))

      ;; TODO: Would be great to have this working, but I don't know how best to determine
      ;; a generic[-enough] minor mode that works for all polymode mode subclasses/instances.
      ;; (evil-define-minor-mode-key 'normal 'polymode-minor-mode
      ;;   (kbd "] c") 'polymode-next-chunk-same-type
      ;;   (kbd "] C") 'polymode-next-chunk
      ;;   (kbd "[ c") 'polymode-previous-chunk-same-type
      ;;   (kbd "[ C") 'polymode-previous-chunk
      ;;   )

      (evil-define-motion noweb-chunk-forward (count)
        "Move forward a chunk"
        :type inclusive
        (polymode-next-chunk count))
      (evil-define-motion noweb-chunk-forward-same-type (count)
        "Move forward a chunk (same type of chunk as current location)"
        :type inclusive
        (polymode-next-chunk-same-type count))
      (evil-define-motion noweb-chunk-backward (count)
        "Move backward a chunk."
        :type inclusive
        (polymode-previous-chunk count))
      (evil-define-motion noweb-chunk-backward-same-type (count)
        "Move backward a chunk (same type of chunk as current location)."
        :type inclusive
        (polymode-previous-chunk-same-type count))
      (evil-add-command-properties #'polymode-next-chunk :jump t)
      (evil-add-command-properties #'polymode-next-chunk-same-type :jump t)
      (evil-add-command-properties #'polymode-previous-chunk :jump t)
      (evil-add-command-properties #'polymode-previous-chunk-same-type :jump t)
      ;; TODO
      ;; (evil-define-text-object noweb-chunk-object (count) ...)

      ;; TODO: Create a macro that works for any REPL-send function.
      ;; Also, consider using `pm-execute-narrowed-to-span'.
      ;; Might want to follow `polymode-register-weaver' and `polymode-set-weaver' so that
      ;; the REPL-send function is available where-/whenever.
      ;;
      ;; (defmacro define-polymode-send-chunk (mode sendfunc)
      ;;   "Creates a send-chunk function for a given MODE and shell/comint SENDFUNC"
      ;;   (let* ((mode-name (symbol-name mode))
      ;;          (func-name (concat
      ;;                      mode-name "shell-send-chunk")))
      ;;          ()
      ;;     )
      ;; ;; (interactive)
      ;; ;; (let ((span (pm-get-innermost-span)))
      ;; ;;   (when (eq (nth 0 span) 'body)
      ;; ;;     (send-func 
      ;; ;;      (1+ (nth 1 span)) (1- (nth 2 span))))
      ;; ;;   )
      ;; )

      ;; TODO
      ;; (defun polymode-send-code-from-here ()
      ;;   "Run code from the beginning of the document to the current (cursor) point."
      ;;   (interactive "p")
      ;;   (let* ((sofar 0)
      ;;          (beg (point-min))
      ;;          (end (point)))
      ;;     (condition-case nil
      ;;         (pm-map-over-spans
      ;;          (lambda ()
      ;;            (unless (memq (car *span*) '(head tail))
      ;;              ;; TODO: `this-class` needs to be whatever signifies a "code" chunk.
      ;;              (when (and (equal this-class
      ;;                                (eieio-object-name (car (last *span*))))
      ;;                         (eq this-type (car *span*)))
      ;;                (setq sofar (1+ sofar)))
      ;;              (unless this-class
      ;;                (setq this-class (eieio-object-name (car (last *span*)))
      ;;                      this-type (car *span*)))
      ;;              ;; (when (>= sofar N)
      ;;              ;;   (signal 'quit nil))
      ;;              ))
      ;;          beg end nil nil nil t)
      ;;       (quit (when (looking-at "\\s *$")
      ;;               (forward-line)))
      ;;       (pm-switch-to-buffer))
      ;;     sofar))

      ;; TODO
      ;; (defun polymode-goto-chunk (chunk-num)
      ;;   ())

      ;; TODO
      ;; (defun polymode-next-code-chunk (num)
      ;;   (...))
      )
    :mode
    ("\\.texw" . poly-noweb+python-mode)
    ;; ("\\.Rnw" . poly-noweb+r-mode)
    ;; ("\\.Rmd" . poly-markdown+r-mode)
    )

  (defun poly-noweb+python-mode-settings ()
    "Custom settings for noweb"

    ;; FYI: mode information is kept in `pm/polymode'.

    ;; TODO: Use macro or something.
    ;; TODO: This is more-or-less what R's polymode does (via `ess-mode').
    ;; (when (fboundp 'advice-add)
    ;;   (advice-add 'python-shell-send-buffer :around 'pm-execute-narrowed-to-span))
    ;; (evil-leader/set-key-for-mode 'python-mode
    ;;   "sc" 'python-shell-send-buffer)

    (defun python-shell-send-chunk ()
      "Send chunk under cursor to a mode-specified REPL server."
      (interactive)
      (let ((span (pm-get-innermost-span nil t)))
        (when (eq (nth 0 span) 'body)
          (python-shell-send-region
           (1+ (nth 1 span)) (1- (nth 2 span))))
        ))

    (evil-leader/set-key-for-mode 'python-mode
      "sc" 'python-shell-send-chunk)
    )
  (add-hook 'poly-noweb+python-mode-hook 'poly-noweb+python-mode-settings)

  (defun clang-format-bindings ()
    (define-key c++-mode-map [tab] 'clang-format-buffer)
    (global-set-key [C-M-tab] 'clang-format-region))
  (add-hook 'c++-mode-hook 'clang-format-bindings)

  ;; (defun tex-mode-settings ()
  ;;   (setq latex-directory "")
  ;;   (setq latex-run-command ""))
  ;; (add-hook 'tex-mode-hook 'tex-mode-settings)
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-names-using-follow (quote ("completion-at-point" "Actions")))
 '(package-selected-packages
   (quote
    (company-quickhelp stickyfunc-enhance srefactor flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck auto-dictionary slime-company slime common-lisp-snippets conda mmm-mode markdown-toc markdown-mode gh-md web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode sql-indent disaster company-c-headers cmake-mode clang-format helm-company helm-c-yasnippet fuzzy company-statistics company-auctex company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete auctex-latexmk auctex ob-ipython python-x folding xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help polymode smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic yaml-mode ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
