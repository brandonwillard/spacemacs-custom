;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(javascript
     clojure
     ;; go
     csv
     ;; (javascript :packages (not tern))
     (lsp :packages (not
                     ;; lsp-ui
                     flycheck-lsp))
     html
     markdown
     (latex :variables
            latex-build-command "Make")
     bibtex
     graphviz
     ;; (ess :variables
     ;;      ess-disable-underscore-assign t
     ;;      :packages (not ess-R-object-popup))
     (python :variables
             python-auto-set-local-pyvenv-virtualenv 'on-visit
             ;; NOTE: These can also be .dir-local/project specific.
             python-test-runner '(pytest nose)
             python-backend 'lsp
             ;; python-enable-yapf-format-on-save t
             :packages (not live-py-mode))
     python-extras
     (hy :variables hy-shell-spy-delim "\n--spy-output--\n")
     yaml
     sql
     ;; noweb
     (c-c++ :variables
            ;; company-c-headers-path-user '("../include" "./include" "." "../../include"
            ;;                               "../inc" "../../inc")
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     helm
     (auto-completion :variables
                      ;; auto-completion-enable-sort-by-usage t
                      ;; :packages (not auto-complete ac-ispell)
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence "C-y"
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual)
     emacs-lisp
     git
     (github :variables magit-gh-pulls-pull-detail-limit 50)
     scheme
     racket
     pdf
     (org :variables
          org-enable-github-support t
          org-projectile-file "TODOs.org")
     org-extras

     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)

     spell-checking
     syntax-checking

     ;; slack
     (erc :variables
          erc-server-list
          '(("chat.freenode.net"
             :port "6697"
             :ssl t
             :nick "brandonwillard"
             ))
          erc-enable-notifications t
          erc-enable-sasl-auth t)

     ;; FIXME: We get `semantic-idle-scheduler-function' errors in `polymode' modes.
     (semantic :enabled-for emacs-lisp common-lisp python)
     common-lisp)

   ;; FYI: You can use MELPA recipes here (i.e. https://github.com/melpa/melpa#recipe-format).
   dotspacemacs-additional-packages '(;; elisp file manipulation library
                                      f
                                      ;; elisp string manipulation library
                                      s
                                      ;; elisp list manipulation library
                                      dash
                                      dash-functional
                                      ;; embrace
                                      evil-embrace
                                      ox-jira
                                      ;; ox-confluence
                                      plantuml-mode
                                      org-gcal
                                      dockerfile-mode
                                      evil-extra-operator
                                      kubernetes
                                      kubernetes-evil
                                      ;; (helpful :location (recipe :fetcher github
                                      ;;                            :repo "Wilfred/helpful"))
                                      ;; Override with local versions.
                                      ;; XXX: Make sure package locations are on the `load-path'.
                                      (hy-mode :location local)
                                      (org-ref :location local)
                                      (ob-hy :location local)
                                      sphinx-doc
                                      ;; Use a newer version of python.el.
                                      (python :location elpa :min-version "0.26.1"))
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(company-emoji emoji-cheat-sheet-plus emojify)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   ;; dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-default-font '("Noto Mono"
                               :size 13.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key (concat "C-" dotspacemacs-leader-key)
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key (concat "C-" dotspacemacs-major-mode-leader-key)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 5
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 100
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers '(:relative nil
                               :enabled-for-modes prog-mode
                                                  text-mode
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   pdf-view-mode
                                                   xwidget-webkit-mode
                                                   edebug-mode
                                                   debugger-mode
                               :size-limit-kb 1000)
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-import-env-vars-shell-file-name shell-file-name
   dotspacemacs-switch-to-buffer-prefers-purpose nil))

(defun dotspacemacs/user-init ()

  (setq init-file-debug nil)
  (setq debug-on-error t)

  (cl-defun btw/add-valid-paths-to-list (target-list object-list &optional append)
    (dolist (file (seq-take-while #'file-exists-p object-list))
      (add-to-list target-list (expand-file-name file) append)))

  (btw/add-valid-paths-to-list 'load-path
                               ;; TODO: Use `dotspacemacs-directory'?
                               '("~/.spacemacs.d"
                                 "~/projects/code/emacs/ob-hy"
                                 "~/projects/code/emacs/hy-mode"
                                 "~/projects/code/emacs/org-ref"))

  (btw/add-valid-paths-to-list 'Info-default-directory-list
                               '("/usr/share/info/emacs-27" "/usr/local/share/info"
                                 "/usr/share/info"))

  ;; Viper is loaded/installed automatically, but we want it disabled.
  (setq package-load-list '(all (viper nil)))

  ;; Fix for anaconda env interaction with pyvenv.
  (setq conda-home (or (getenv "ANACONDA_HOME") "~/apps/anaconda3"))
  (setenv "ANACONDA_HOME" conda-home)
  (setenv "WORKON_HOME" (concat conda-home "/" "envs"))

  ;; Just to be sure (and because we're seeing some new problems with PATH var
  ;; output from `shell-command-to-string' under let-bound `shell-file-name'),
  ;; let's make sure the conda binaries are available.
  (add-to-list 'exec-path (expand-file-name (concat conda-home "/" "bin")))

  ;; Hack for `exec-path' not being used by `shell-command-to-string'.
  ;; We're basically setting `process-environment', which is used by those shell commands.
  ;; (seq-filter (lambda (var) (s-starts-with-p "PATH=" var)) process-environment)
  (setenv "PATH" (mapconcat #'identity (delete-dups exec-path) ":"))

  ;; Looks like `spacemacs/loadenv' is cutting off DBUS_SESSION_BUS_ADDRESS at the "="!
  ;; (when-let* (((string-equal (getenv "DBUS_SESSION_BUS_ADDRESS") "unix:path"))
  ;;             (bus-path (format "/run/user/%s/bus" (user-uid)))
  ;;             ((file-exists-p bus-path)))
  ;;   (setenv "DBUS_SESSION_BUS_ADDRESS" (concat "unix:path=" bus-path)))

  (setq custom-file (concat user-emacs-directory "private/custom-settings.el"))

  (setq browse-url-browser-function '((".*slack.*" . browse-url-chrome)
                                      (".*youtube.*" . browse-url-chrome)
                                      ("." . eww-browse-url)))
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

  ;; Helps with delays while handling very long lines.
  (setq-default bidi-display-reordering nil)
  (setq debugger-stack-frame-as-list t)
  (setq edebug-print-circle t)
  (setq edebug-print-level 4)
  (setq print-circle t))

(defun dotspacemacs/user-config ()

  ;; Just a helper function for whatever.
  (defun hash-table-to-alist (hash-table)
    (json-read-from-string (json-encode hash-table)))

  ;; TODO: Hack fix; consider fixing, and then removing, this.
  (defun spacemacs/symbol-highlight-transient-state/body ())

  (setq comment-empty-lines t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-wrap nil)

  (require 'mode-local)

  (setq-mode-local text-mode scroll-margin 10)
  (setq-mode-local prog-mode scroll-margin 10)
  (setq-mode-local makefile-mode indent-tabs-mode t)

  ;; Stop terminals from re-centering!
  (setq scroll-step 1)
  (setq auto-window-vscroll nil)
  (setq scroll-conservatively 10000)

  (defun btw//lightweight-debug-settings ()
    (setq-local truncate-lines t)
    (setq-local print-level 4)
    (setq-local print-length 4)
    (setq-local eval-expression-print-level 4)
    (setq-local eval-expression-print-length 4)
    ;; `global-hl-line-mode' can slow down tracebacks considerably.
    (spacemacs/disable-hl-line-mode))

  (add-hook 'edebug-mode-hook #'btw//lightweight-debug-settings)
  (add-hook 'debugger-mode-hook #'btw//lightweight-debug-settings)

  (add-to-list 'debug-ignored-errors 'search-failed)
  (add-to-list 'debug-ignored-errors "^Nothing to complete$")
  (add-to-list 'debug-ignored-errors "^No such page: ")
  (add-to-list 'debug-ignored-errors
               "^Company: backend \(:?.*?\) error \"Nothing to complete\"")
  (add-to-list 'debug-ignored-errors 'lsp-timed-out-error)
  (add-to-list 'debug-ignored-errors
               "Candidates function ‘helm-ag--do-ag-candidate-process’ should run a process")

  (setq-default sentence-end-double-space t)

  ;; XXX: This will stop completion with TAB (but also fix annoying noops when
  ;; attempting to indent lines).
  (setq tab-always-indent t)
  (setq fill-indent-according-to-mode t)

  (spacemacs/toggle-aggressive-indent-on)

  (setq compilation-scroll-output #'first-error)

  ;; Change default spacemacs keybinding.
  (spacemacs/set-leader-keys "nd" 'narrow-to-defun)
  (unbind-key (kbd "nf") spacemacs-default-map)

  (use-package kubernetes
    ;; :ensure t
    :commands (kubernetes-overview))

  (use-package kubernetes-evil
    ;; :ensure t
    :after kubernetes)

  (spacemacs|define-custom-layout "@Kubernetes"
    :binding "K"
    :body (progn (kubernetes-overview)))

  ;; (use-package helpful)

  (use-package org-gcal
    :config (progn
              (when-let* ((client-info (cdr (car (json-read-file
                                                  (f-join dotspacemacs-directory
                                                          "private"
                                                          "org-gcal-brandonwillard-gmail.json")))))
                          (client-id (alist-get 'client_id client-info))
                          (client-secret (alist-get 'client_secret client-info)))
                ;; TODO: Use `plstore'/authstore
                ;; (add-to-list 'auth-sources "~/.authinfo.json.gpg")
                (setq org-gcal-client-id client-id
                      org-gcal-client-secret client-secret
                      org-gcal-file-alist '(("brandonwillard@gmail.com" .
                                             (f-join dotspacemacs-directory
                                                     "private"
                                                     "brandonwillard-gcal.org"))))
                ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
                (with-eval-after-load 'org-agenda
                  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
                  ;; TODO: Map values and `add-to-list'.
                  (add-to-list 'org-agenda-files
                               (f-join dotspacemacs-directory
                                       "private"
                                       "brandonwillard-gcal.org"))))))

  (use-package plantuml-mode
    :init (setq plantuml-jar-path (expand-file-name "~/apps/plantuml.jar"))
    :config (progn
              (defun btw/plantuml-custom-graphvizdot-location (cmd-list)
                (append cmd-list (list "-graphvizdot" (executable-find "dot"))))
              (advice-add #'plantuml-render-command :filter-return
                          #'btw/plantuml-custom-graphvizdot-location)))

  (use-package embrace
    :init (progn
            (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
            (add-hook 'org-mode-hook 'embrace-org-mode-hook))
    :functions embrace-add-pair-regexp
    :config (progn
              (defun btw/embrace-emacs-lisp-mode-hook ()
                ;; (assq-delete-all ?f embrace--pairs-list)
                (defun embrace-with-function-elisp ()
                  (let ((fname (read-string "Function: ")))
                    (cons (format "(%s "
                                  (or fname "")) ")")))
                (embrace-add-pair-regexp ?f
                                         "(\\(\\sw\\|\\s_\\)+?\\s-+?"
                                         ")"
                                         'embrace-with-function-elisp
                                         (embrace-build-help "(function " ")")
                                         nil))
              (advice-add 'embrace-emacs-lisp-mode-hook
                          :after #'btw/embrace-emacs-lisp-mode-hook)
              (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)))

  (use-package evil-embrace
    :init (evil-embrace-enable-evil-surround-integration))

  (use-package dockerfile-mode
    :mode ("Dockerfile\\'" . dockerfile-mode))

  (use-package sphinx-doc
    :defer t
    :commands (sphinx-doc sphinx-doc-mode)
    :init (progn
            (spacemacs/set-leader-keys-for-major-mode 'python-mode
              "rd" #'sphinx-doc)
            (defun btw/setup-sphinx-doc ()
              (sphinx-doc-mode t))
            (add-hook 'python-mode-hook #'btw/setup-sphinx-doc)))

  (with-eval-after-load 'semantic
    (setq semanticdb-search-system-databases nil)
    (add-to-list 'semanticdb-project-root-functions #'projectile-project-root))

  (with-eval-after-load 'evil-surround
    (setq evil-surround-pairs-alist
          '((?\( . ("(" . ")"))
            (?\[ . ("[" . "]"))
            (?\{ . ("{" . "}"))

            (?\) . ("( " . " )"))
            (?\] . ("[ " . " ]"))
            (?\} . ("{ " . " }"))

            (?# . ("#{" . "}"))
            (?b . ("(" . ")"))
            (?B . ("{" . "}"))
            (?> . ("<" . ">"))
            (?t . evil-surround-read-tag)
            (?< . evil-surround-read-tag)
            (?f . evil-surround-function))))

  (with-eval-after-load 'erc
    (add-to-list 'erc-modules 'notifications)
    (setq erc-track-when-inactive t)
    (setq erc-track-remove-disconnected-buffers t)
    (setq erc-format-query-as-channel-p t
          erc-track-priority-faces-only 'all
          erc-track-faces-priority-list '(erc-error-face
                                          erc-current-nick-face
                                          erc-keyword-face
                                          erc-nick-msg-face
                                          erc-direct-msg-face
                                          erc-dangerous-host-face
                                          erc-notice-face
                                          erc-prompt-face))
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-track-exclude-server-buffer t)
    (setq erc-track-visibility 'selected-visible)
    (setq erc-autojoin-channels-alist '((".*" "#hy")))
    (setq erc-auto-query 'window)
    (setq erc-track-enable-keybindings nil))

  (with-eval-after-load 'magit
    (setq magit-repository-directories '(("~/" . 1)
                                         ("~/projects/code" . 3)
                                         ("~/projects/papers" . 3)
                                         ("~/projects/citybase" . 3))))

  (with-eval-after-load 'magithub-dash
    (setq magithub-dashboard-show-read-notifications nil))

  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-exclude-modes 'help-mode)
    (add-to-list 'editorconfig-exclude-modes 'edebug-mode)
    (add-to-list 'editorconfig-exclude-modes 'debugger-mode))

  (with-eval-after-load 'flycheck
    (setq flycheck-indication-mode 'right-fringe)
    (add-to-list 'flycheck-disabled-checkers 'python-flake8))

  (with-eval-after-load 'python
    (when (fboundp 'purpose-set-extension-configuration)
      ;; NOTE: To delete this configuration...
      ;; (purpose-del-extension-configuration :python)
      (purpose-set-extension-configuration
       :python (purpose-conf :mode-purposes
                             '((python-mode . python)
                               (inferior-python-mode . repl))
                             ;; :regexp-purposes
                             ;; '(("^test-.*\\.py$" . test))
                             )))
    (setq-default python-eldoc-get-doc nil))

  (with-eval-after-load 'hy-mode
    (spacemacs|use-package-add-hook evil-cleverparens
      :pre-init
      (add-to-list 'evil-lisp-safe-structural-editing-modes 'hy-mode))
    (when (fboundp 'purpose-set-extension-configuration)
      ;; NOTE: To delete this configuration...
      ;; (purpose-del-extension-configuration :hy)
      (purpose-set-extension-configuration
       :hy (purpose-conf :mode-purposes
                         '((hy-mode . hy)
                           (inferior-hy-mode . repl))
                         ;; :regexp-purposes
                         ;; '(("^test-.*\\.hy$" . test))
                         ))))

  (when (fboundp 'pyvenv-tracking-mode)
    ;; Set buffer local `pyvenv-workon' values for automatic activation.
    (setq pyvenv-tracking-ask-before-change t)
    (pyvenv-tracking-mode +1))

  (with-eval-after-load 'vim-powerline-theme
    ;; Egh, doesn't really work.
    (setq winum-auto-setup-mode-line t)
    (setq powerline-default-separator 'bar))

  (with-eval-after-load 'bibtex
    (defun btw/find-project-bib-file (dir)
      "Finds an bib file within a projectile project."
      (let* ((project-root (with-demoted-errors "Error: %S" (projectile-project-root)))
             (file-name (f-glob "src/tex/*.bib" project-root)))
        (when (f-exists? file-name)
          file-name))))

  (with-eval-after-load 'org-ref
    (setq org-ref-pdf-directory "~/projects/papers/references"
          org-ref-bibliography-notes "~/projects/papers/references/notes.org"
          org-ref-prefer-bracket-links t))

  (with-eval-after-load 'lsp-mode
    (defun btw/lsp-python-workspace-root ()
      (or (when (fboundp 'projectile-project-root)
            (let ((projectile-require-project-root t))
              (condition-case nil
                  (projectile-project-root)
                (error nil))))
          ;; Based on `lsp-make-traverser'.
          (lambda ()
            (let ((dir
                   ;; TODO: ".git" and lib paths?
                   (directory-files "." nil "\\(__init__\\|setup\\)\\.py")))
              (if dir (file-truename dir)))
            (if lsp-message-project-root-warning
                (message "Couldn't find project root, using the current directory as the root.")
              (lsp-warn "Couldn't find project root, using the current directory as the root.")
              default-directory))))
    (lsp-define-stdio-client lsp-python "python"
			                       #'btw/lsp-python-workspace-root
			                       '("pyls"))
    (setq lsp-message-project-root-warning t)
    (setq lsp-enable-eldoc nil))

  (with-eval-after-load 'lsp-ui
    (setq lsp-eldoc-render-all nil)
    (setq lsp-ui-doc-enable nil)
    (setq lsp-enable-eldoc nil)
    (setq lsp-ui-sideline-delay nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-enable nil))

  (with-eval-after-load 'org
    ;; TODO: Consider this...
    ;; (org-babel-make-language-alias "python" "ipython")

    (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
    ;; (setq org-plantuml-jar-path plantuml-jar-path)

    (add-to-list 'org-babel-load-languages
                 '(plantuml . t))
    (add-to-list 'org-babel-load-languages
                 '(dot . t))
    (add-to-list 'org-babel-load-languages
                 '(scheme . t))

    (defun spacemacs//org-latex-pdf-process (file-name)
      "XXX: This will err-out because of org-export's assumption that the output
    should be in the local directory"
      (let* ((projectile-require-project-root nil)
             (proj-root (projectile-project-root))
             (pdf-out-dir (when proj-root
                            (f-join proj-root org-projectile-output-dir)))
             (pub-dir (if (and pdf-out-dir
                               (f-exists? pdf-out-dir))
                          pdf-out-dir
                        (f-dirname file-name)))
             (TeX-process-asynchronous nil))
        ;; Runs `compile', which starts an asych process.  We need it to finish
        ;; *before* this function finishes; otherwise, `org-mode' will read any
        ;; existing output immediately and the update times will not differ,
        ;; making it believe that the compilation failed.
        ;; `TeX-command-buffer' is the output buffer.
        (TeX-command latex-build-command
                     (lambda (&rest _)
                       (f-filename (f-swap-ext file-name "pdf")))
                     -1)))

    (setq org-capture-templates
          '(("t" "Tasks" entry
             (file+headline org-default-notes-file "Tasks"))))

    (setq org-highlight-latex-and-related '(latex entities))

    ;; Most often, we'll use inline src statements (e.g. src_python{...}) to
    ;; simply display formatted text.
    (setq org-babel-default-inline-header-args
          '((:exports . "code")
            (:eval . "never")
            (:results . "none")))

    (setq org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-window-setup 'current-window
          org-src-fontify-natively t
          org-support-shift-select 'always)

    (setq org-latex-pdf-process #'spacemacs//org-latex-pdf-process)

    ;; What to allow before and after markup
    ;; See https://emacs.stackexchange.com/a/13828
    ;; (setcar (nthcdr 1 org-emphasis-regexp-components)
    ;;         (concat (nth 0 org-emphasis-regexp-components) "s"))
    (setq org-link-file-path-type 'relative)
    (setq org-confirm-babel-evaluate nil)
    (setq org-default-notes-file (f-join user-home-directory "Documents" "notes.org")))

  (with-eval-after-load 'tex
    (defvar TeX-command-list)
    (add-to-list 'TeX-command-list
                 '("Make" "make %o" TeX-run-command nil t))
    ;; XXX: Must have this set in the spacemacs tex layer!
    (setq TeX-command-default "Make"))

  (with-eval-after-load 'projectile
    (setq projectile-globally-ignored-directories
          (append projectile-globally-ignored-directories
                  '(".ropeproject" ".cache" "__pycache__"
                    ".pytest_cache" ".mypy_cache")))
    (setq projectile-tags-file-name ".TAGS")
    (setq projectile-use-git-grep t))

  (with-eval-after-load 'org-projectile
    (setq org-projectile-capture-template "* TODO %?
  %u
  %a"))

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

  (with-eval-after-load 'evil-lisp-state
    ;; Let's get rid of that annoying mode switching.
    (dolist (x evil-lisp-state-commands)
      (let ((key (car x))
            (cmd (cdr x)))
        (eval
         `(progn
            (if evil-lisp-state-global
                (define-key evil-lisp-state-map ,(kbd key) (quote ,cmd))
              (define-key evil-lisp-state-major-mode-map ,(kbd key) (quote ,cmd)))))))

    ;; (spacemacs/set-leader-keys "k" evil-lisp-state-map)
    )

  (with-eval-after-load 'persp-mode
    (setq persp-autokill-buffer-on-remove t)
    ;; Add all opened buffers (filter certain ones below).
    (setq persp-add-buffer-on-after-change-major-mode t)

    (defun btw/persp-assign-projectile-root (persp persp-hash)
      "Add a variable to the perspective tracking the projectile project name
 (if any).

 This is run before the buffer is created, so we need to get the project name
 from this perspective's path.  We assume the perspective's name is the project
 path (which it is per Spacemacs)"
      (let* ((persp-name (safe-persp-name persp))
             (persp-projectile-dir (when (and (f-dir? persp-name)
                                              (funcall projectile-project-name-function
                                                       persp-name))
                                     persp-name)))
        (set-persp-parameter 'projectile-project-root
                             persp-projectile-dir
                             persp)))

    (add-hook 'persp-created-functions #'btw/persp-assign-projectile-root)

    (defun btw/persp-projectile-project-root (oldfun &rest r)
      "Use the perp project name and regular `projectile-project-root' as a
 fallback."
      (let* ((persp-name (spacemacs//current-layout-name))
             (persp-projectile-dir (when (and (f-dir? persp-name)
                                              (funcall projectile-project-name-function
                                                       persp-name))
                                     persp-name)))
        ;; If the persp name is a directory and is mapped to a projectile project,
        ;; return the directory; otherwise, use the fallback.
        (or persp-projectile-dir
            (persp-parameter 'projectile-project-root)
            (apply oldfun r))))

    (advice-add #'projectile-project-root :around #'btw/persp-projectile-project-root)

    (defun btw/persp-projectile-project-name (oldfun &rest r)
      "Query the persp layout for the projectile project name and use projectile
 for the fallback."
      (let* ((persp-name (spacemacs//current-layout-name))
             (persp-projectile-name (if (f-dir? persp-name)
                                        (funcall projectile-project-name-function
                                                 persp-name)
                                      (persp-parameter 'projectile-project-root))))
        (or persp-projectile-name (apply oldfun r))))

    (advice-add #'projectile-project-name :around #'btw/persp-projectile-project-name)

    ;; (add-hook 'persp-common-buffer-filter-functions
    ;;           ;; there is also `persp-add-buffer-on-after-change-major-mode-filter-functions'
    ;;           #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

    ;; Restore eshell buffers.
    ;; See https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-inferior-python-save-load-el
    ;; for more examples (e.g. Python inferior shells).
    (persp-def-buffer-save/load
     :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
     :save-vars '(major-mode default-directory))

    (defun btw/projectile-shell-pop ()
      "Open a term buffer at projectile project root for the current perspective."
      (let* ((current-persp-name (spacemacs//current-layout-name))
             (persp-idx (seq-find #'identity
                                  (seq-map-indexed (lambda (name i)
                                                     (if (eq name current-persp-name)
                                                         i
                                                       nil))
                                                   (persp-names-current-frame-fast-ordered))))
             (shell (if (eq 'multi-term shell-default-shell)
                        'multiterm
                      shell-default-shell))
             (shell-pop-func (intern (format "spacemacs/shell-pop-%S" shell))))
        (funcall shell-pop-func persp-idx)))

    (advice-add #'spacemacs/projectile-shell-pop :override #'btw/projectile-shell-pop))

  (with-eval-after-load 'comint
    ;; Make terminals and REPLs read-only.
    ;; https://emacs.stackexchange.com/a/2897
    (setq-default comint-prompt-read-only t)
    (setq-default comint-use-prompt-regexp nil)
    (setq-default inhibit-field-text-motion nil)
    (setq-default comint-move-point-for-output t)

    (defun btw/comint-maybe-goto-prompt ()
      (when (and (derived-mode-p 'comint-mode)
                 ;; `term-mode' has a different idea of pmark.
                 (not (derived-mode-p 'term-mode))
                 (not (comint-after-pmark-p)))
        (comint-goto-process-mark)
        (move-end-of-line nil)))

    (add-hook 'evil-insert-state-entry-hook
              #'btw/comint-maybe-goto-prompt)
    (add-hook 'evil-hybrid-state-entry-hook
              #'btw/comint-maybe-goto-prompt)

    (defun btw/comint-preoutput-turn-buffer-read-only (text)
      (propertize text 'read-only t))

    (add-hook 'comint-output-filter-functions
              #'btw/comint-preoutput-turn-buffer-read-only
              'append))

  (with-eval-after-load 'evil
    (setq-default evil-want-visual-char-semi-exclusive t)
    (setq-default evil-move-curser-back nil)
    (setq-default evil-escape-key-sequence nil)
    ;; (setq-default evil-emacs-state-modes nil)
    (setq-default evil-insert-state-modes '(magit-popup-mode))
    (setq evil-kill-on-visual-paste nil)
    ;; (setq-default evil-motion-state-modes nil)

    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'company
    (setq company-search-filtering t)
    (setq company-idle-delay nil)
    ;; (setq company-backends-emacs-lisp-mode
    ;;       (cons '(company-elisp :with company-yasnippet)
    ;;             (seq-remove (lambda (x) (eq (car x) 'company-capf))
    ;;                         company-backends-emacs-lisp-mode)))
    (spacemacs|add-company-backends :backends (company-elisp)
                                    :modes emacs-lisp-mode)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous))

  (with-eval-after-load 'window-purpose-switch
    (setq purpose-display-at-bottom-height 0.4))

  (with-eval-after-load 'helm
    (setq-default helm-follow-mode-persistent nil)
    (setq helm-always-two-windows nil)
    (setq helm-autoresize-mode nil)
    (setq helm-split-window-inside-p nil)
    (setq helm-split-window-default-side 'below)

    ;; NOTE: `window-purpose-switch' can destroy expected
    ;; pop-up/window placement behavior; look at the default
    ;; values for vars like `purpose-display-at-bottom-height'
    ;; if there are problems.

    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (with-eval-after-load 'evil-jumps
    (setq evil-jumps-cross-buffers nil))

  (with-eval-after-load 'c++-mode
    (defun btw/clang-format-bindings ()
      (define-key c++-mode-map [tab] 'clang-format-buffer)
      (global-set-key [C-M-tab] 'clang-format-region))

    (add-hook 'c++-mode-hook 'btw/clang-format-bindings))

  (with-eval-after-load 'term
    ;; TODO: See https://github.com/emacs-evil/evil-collection and
    ;; `evil-collection-term-sync-state-and-mode-p'.
    ;; For now, try some of the additions from this PR:
    ;; https://github.com/syl20bnr/spacemacs/pull/10844
    ;; TODO: Remove when merged.

    (defun btw/term-enable-line-mode ()
      "Enable `term-line-mode' when in `term-mode' buffer."
      (when (eq major-mode 'term-mode)
        (term-line-mode)))

    (defun term-after-prompt-p ()
      "Is point after the prompt?"
      (let ((cur-point (point)))
        (save-mark-and-excursion
          (goto-char (term-process-mark))
          (term-bol nil)
          (<= (point) cur-point))))

    (setq-default term-char-mode-point-at-process-mark nil)

    (defun btw/term-enable-char-mode-maybe-goto-prompt ()
      (when (eq major-mode 'term-mode)
        (let ((term-char-mode-point-at-process-mark nil))
          (unless (term-after-prompt-p)
            (goto-char (term-process-mark)))
          (term-char-mode))))

    (add-hook 'evil-insert-state-entry-hook
              #'btw/term-enable-char-mode-maybe-goto-prompt)
    (add-hook 'evil-hybrid-state-entry-hook
              #'btw/term-enable-char-mode-maybe-goto-prompt)
    (add-hook 'evil-normal-state-entry-hook
              #'btw/term-enable-line-mode)

    (declare-function term-send-raw-string "term.el")
    (declare-function term-send-del "term.el")

    (defun btw/send-C-r ()
      (interactive)
      (term-send-raw-string "\C-r"))

    ;; This allow us to use the same global shortcut for `eval-expression'.
    (unbind-key "M-:" term-raw-map)

    (defun btw/setup-term-mode ()
      ;; (setq-local fringe-mode nil)
      ;; (setq-local fringes-outside-margins t)
      ;; (setq-local mouse-yank-at-point t)
      ;; (setq-local transient-mark-mode nil)
      ;; (setq tab-width 8 )
      ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] ")
      (setq term-prompt-regexp "^.* [#$%>»] ")
      (auto-fill-mode -1)
      (evil-local-set-key 'insert (kbd "<delete>") #'term-send-del)
      ;; From https://github.com/syl20bnr/spacemacs/issues/2345
      (evil-local-set-key 'insert (kbd "C-r") #'btw/send-C-r))

    (add-hook 'term-mode-hook #'btw/setup-term-mode)

    (defvar term-width)
    ;; This was apparently renamed.
    ;; (defvar term-terminal-parameter)
    ;; (define-obsolete-variable-alias 'term-terminal-parameter 'term-terminal-undecoded-bytes)
    (defvar term-terminal-undecoded-bytes)
    (declare-function term-move-columns "term.el")
    (declare-function term-current-column "term.el")

    (defun btw/term-handle-more-ansi-escapes (proc params char)
      "Handle additional ansi escapes.
From https://emacs.stackexchange.com/a/10698"
      (cond
       ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
       ((eq char ?G)
        (let ((col (min term-width
                        (max 0 (or term-terminal-undecoded-bytes 0)))))
          (term-move-columns (- col
                                (term-current-column)))))
       (t)))

    (advice-add 'term-handle-ansi-escape :before #'btw/term-handle-more-ansi-escapes))

  (spacemacs|define-custom-layout "@Spacemacs"
    :binding "e"
    :body (progn (spacemacs/find-dotfile)
                 (set-persp-parameter 'projectile-project-root
                                      (ignore-errors (projectile-project-root)))
                 (set-window-dedicated-p (get-buffer-window) t)
                 (display-buffer-in-side-window (messages-buffer) '((side . right)))
                 (balance-windows-area)))

  ;;; Initialization steps

  (defun btw/after-user-config-setup ()
    ;; (require 'frame-cmds)
    ;; (enlarge-font -3)
    ;; (debug-on-entry #'enlarge-font)
    ;; (persp-mode +1)
    (add-hook 'persp-mode-hook
              (lambda ()
                (spacemacs/custom-perspective-@Spacemacs)))
    (when (file-exists-p custom-file)
      (load-file custom-file)))

  (spacemacs/defer-until-after-user-config
   #'btw/after-user-config-setup))
