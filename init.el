;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(
     csv
     (javascript :packages (not tern))
     ;; (lsp :packages (not flycheck-lsp lsp-ui))
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
             python-test-runner 'pytest
             python-auto-set-local-pyvenv-virtualenv 'on-project-switch
             python-backend nil
             :packages (not live-py-mode))
     python-extras
     hy
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
     (github :variables magit-gh-pulls-pull-detail-limit 200)
     scheme
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
     ;; FIXME: We get `semantic-idle-scheduler-function' errors in `polymode' modes.
     (semantic :enabled-for emacs-lisp common-lisp python)
     common-lisp)

   ;; FYI: You can use MELPA recipes here (i.e. https://github.com/melpa/melpa#recipe-format).
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

                                      ox-jira
                                      ;; ox-confluence
                                      plantuml-mode

                                      org-gcal

                                      ;; Now in hy layer.
                                      ;; ob-hy

                                      dockerfile-mode

                                      evil-extra-operator

                                      (helpful :location (recipe :fetcher github
                                                                 :repo "Wilfred/helpful"))

                                      ;; Override with local versions.
                                      ;; XXX: Make sure package locations are on the `load-path'.
                                      (hy-mode :location local)
                                      (org-ref :location local)

                                      ;; Use a newer version of python.el.
                                      (python :location elpa :min-version "0.26.1"))
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
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
   dotspacemacs-mode-line-theme 'vim-powerline
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

  (setq browse-url-browser-function 'eww-browse-url)
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

  ;; Helps with delays while handling very long lines.
  (setq-default bidi-display-reordering nil)
  (setq debugger-stack-frame-as-list t)
  (setq edebug-print-circle t)
  (setq edebug-print-level 4)
  (setq print-circle t))

(defun dotspacemacs/user-config ()

  (defun btw/spacemacs--set-zoom-factor (zoom-factor)
    (when-let* ((cur-zoom-factor (or (frame-parameter (selected-frame) 'zoomed) 0))
                (zoom-diff (- cur-zoom-factor zoom-factor))
                (zoom-dir (if (< zoom-diff 0) 1 -1)))
      (when (/= zoom-diff 0)
        (loop repeat (abs zoom-diff) do (spacemacs//zoom-frm-do zoom-dir))
        (spacemacs//zoom-frm-powerline-reset))))

  (btw/spacemacs--set-zoom-factor -4)

  (setq comment-empty-lines t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-wrap nil)

  (require 'mode-local)

  (setq-mode-local text-mode scroll-margin 10)
  (setq-mode-local prog-mode scroll-margin 10)
  (setq-mode-local makefile-mode indent-tabs-mode t)

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

  (use-package helpful)

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

  (with-eval-after-load 'magit
    (setq magit-repository-directories '(("~/" . 1)
                                         ("~/projects/code" . 3)
                                         ("~/projects/papers" . 3)
                                         ("~/projects/citybase" . 3))))

  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-exclude-modes 'help-mode)
    (add-to-list 'editorconfig-exclude-modes 'edebug-mode)
    (add-to-list 'editorconfig-exclude-modes 'debugger-mode))

  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-disabled-checkers 'python-flake8))

  (with-eval-after-load 'python
    (setq-default python-eldoc-get-doc nil))

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

  (with-eval-after-load 'lsp
    (lsp-define-stdio-client lsp-python "python"
			                       (lsp-make-traverser #'(lambda (dir)
                                                     (or (when (fboundp 'projectile-project-root)
                                                           (projectile-project-root))
                                                         (directory-files
                                                          dir nil "\\(__init__\\|setup\\)\\.py"))))
			                       '("pyls"))
    (setq lsp-enable-eldoc nil))

  (with-eval-after-load 'lsp-ui
    (setq lsp-ui-sideline-delay nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-enable nil))

  (with-eval-after-load 'org
    ;; (use-package ob-hy
    ;;   :init
    ;;   (progn
    ;;     (add-to-list 'org-babel-load-languages
    ;;                  '(hy . t))))

    ;; TODO: Consider this...
    ;; (org-babel-make-language-alias "python" "ipython")

    (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
    ;; (setq org-plantuml-jar-path plantuml-jar-path)

    (add-to-list 'org-babel-load-languages
                 '(plantuml . t))
    (add-to-list 'org-babel-load-languages
                 '(dot . t))

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
     :save-vars '(major-mode default-directory)))

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
    (setq company-idle-delay nil)

    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous))

  (with-eval-after-load 'window-purpose-switch
    (setq purpose-display-at-bottom-height 0.4))

  (with-eval-after-load 'helm
    (setq-default helm-follow-mode-persistent t)
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

    (declare-function term-send-raw-string "term.el")

    (defun btw/send-C-r ()
      (interactive)
      (term-send-raw-string "\C-r"))

    (defun btw/setup-term-mode ()
      "From https://github.com/syl20bnr/spacemacs/issues/2345"
      (evil-local-set-key 'insert (kbd "C-r") 'btw/send-C-r))

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

  (when (file-exists-p custom-file)
    (load-file custom-file)))
